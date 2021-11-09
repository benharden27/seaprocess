#' Create complete data sheet
#'
#' @param data_input
#' @param summary
#' This function reads in a hand-entered excel sheet along with the summary CSV and produces a well formated CSV with all the relevent data
#' and meta data.
#'
#' @return
#' @export
#'
#' @examples
create_datasheet <- function(data_input, summary_input, data_type = "CTD",
                             csv_output = "output/csv", cruiseID = NULL, add_cruiseID = TRUE,
                             ...) {

  if(csv_output == "output/csv") {
    csv_output <- file.path("output","csv",paste0(stringr::str_to_lower(data_type),"_datasheet.csv"))
  }

  if(data_type == "bottle") {
    data_type <- c("HC","B")
  }

  if(data_type == "neuston") {
    data_type <- "NT"
  }

  data_type <- stringr::str_to_upper(data_type)

  # read in the data_input excel sheet datasheet
  data <- readxl::read_excel(data_input)
  #Set all header names to lower case
  colnames(data) <- stringr::str_to_lower(colnames(data))
  #replace spaces with _
  colnames(data) <- stringr::str_replace_all(colnames(data), " ", "_")
  #remove unit identifiers inside ()
  colnames(data) <- stringr::str_remove_all(colnames(data), "_\\(.*\\)")
  colnames(data) <- stringr::str_remove_all(colnames(data), "_%")
  colnames(data) <- stringr::str_remove_all(colnames(data), "\\.")

  data <- dplyr::mutate(data, bottle = as.character(bottle))
  # add a new column to aid the joining later
  data <- dplyr::mutate(data, deployment = ifelse(is.na(as.numeric(bottle)) | as.numeric(bottle) > 12, "B", "HC"))

  # read in station summary datasheet
  # TODO: determine what formating to apply when read in (beyond zd)
  summary <- readr::read_csv(
    summary_input,
    col_types = readr::cols(
      zd = readr::col_character()
    )
  )

  # filter by data_type
  summary <- dplyr::filter(summary, deployment %in% data_type)

  data <- dplyr::right_join(summary, data, by=c("station","deployment"))

  # Bottle specific stuff
  if(sum(data_type %in% c("HC", "B")) > 0) {
    data <- compile_bottle(data, ...)
  }

  # Neuston specific stuff
  if(data_type == "NT") {
    data <- compile_neuston(data, ...)
  }

  # Remove columns we don't need, e.g. deployment
  data <- dplyr::select(data, -deployment)

  # export to csv
  if(!is.null(csv_output)) {
    if(add_cruiseID == TRUE & !is.null(cruiseID)) {
      csv_output <- add_file_cruiseID(csv_output, cruiseID)
    }
    readr::write_csv(format_csv_output(data),csv_output, na = "")
  }

  return(data)

}

#' @export
#' @rdname compile_neuston
compile_neuston <- function(data, elg_input) {

  # WORK OUT TOW DISTANCE AND ASSIGN TOW LENGTH
  # find out time_out in UTC
  time_diff <- lubridate::time_length(lubridate::hms(data$time_out) - lubridate::hms(data$time_in), unit= "minutes")
  time_diff[time_diff<0] <- time_diff[time_diff<0] + 60 * 24
  data <- dplyr::mutate(data,dttm_out = dttm + lubridate::minutes(time_diff))

  # read in elg
  elg <- get_elg(elg_input)

  # find nearest points and calculate along-path distance
  sti <- find_near(elg$dttm, data$dttm)
  eni <- find_near(elg$dttm, data$dttm_out)
  tow_length <- rep(NA, length(sti))
  for (i in 1:length(sti)) {
    tow_length[i] <- tail(
      oce::geodDist(
        elg$lon[sti[i]:eni[i]],
        elg$lat[sti[i]:eni[i]],
        alongPath = TRUE),1)
  }


  #add tow length in meters to data, set dec to 1
  data <- dplyr::mutate(data, tow_length = tow_length)
    tdec <- 1
    #data <- format_decimal(data, "tow_length", tdec)
    #data <- as.double(data$tow_length)

  # calculate biodensity
  data <- dplyr::mutate(data, biodens = data$`Zooplankton Biovol. (ml)`/`tow_length`)

  # Calculate moon data
  moon_data <- oce::moonAngle(data$dttm,data$lon,data$lat)

  # Add moon info to dataset and set the decimal to zero
  data <- dplyr::mutate(data,
                        moon_phase = moon_data$illuminatedFraction * 100,
                        moon_risen = moon_data$altitude > 0)
  nodec <- 0
  data <- format_decimal(data, "moon_phase", nodec)


  return(data)
}


#' Create bottle file sheet
#'
#' @param summary_csv
#' @param ros_input
#' @param datasheet_folder
#'
#' @return
#' @export
#'
#' @examples
compile_bottle <- function(data, ros_input) {

  # get just the list of stations that are unique
  stations <- unique(data$station)

  # Go find appropriate bottle files from ctd folder and
  # list all files with *.ros extension in ros_input
  ros_files <- list.files(ros_input,pattern = "\\.ros")

  # Loop through all ros files and
  ros_output <- NULL

  for (i in 1:length(stations)) {

    # select the ros file name than matches the station name from the input data
    ros_file <- ros_files[stringr::str_detect(ros_files,stations[i])]
    if(length(ros_file) > 0) {
      if(length(ros_file) > 1) {
        ros_file <- ros_file[1]
        warning(paste0("More than one ros file in folder matches the stations number ",
                      stations[i],
                      ". Selecting the first file found: ",
                      ros_file[1]))
      }
      # Read in the ros file and combine with datasheet
      ros <- read_ros(file.path(ros_input,ros_file))
      ros <- mutate(ros, station = stations[i])
    } else {
      warning(paste("No .ros file found for station",
                    stations[i]))
      ros <- NULL
    }

    if(!is.null(ros)) {
      if(is.null(ros_output)) {
        ros_output <- ros
      } else {
        ros_output <- bind_rows(ros_output, ros)
      }
    }

  }

  if(!is.null(ros_output)) {
    ros_output$bottle <- as.character(ros_output$bottle)
  }

  # Now that we have our depths and metadata for each bottle in a hydrocast, add all the bucket samples to this
  bottle_lines <- filter(data, deployment == "B")
  if(nrow(bottle_lines) > 0) {
    if(!is.null(ros_output)) {
      data_add <- purrr::quietly(tibble::as_tibble)(t(rep(NA_real_, ncol(ros_output))))$result
      names(data_add) <- names(ros_output)
      data_add <- dplyr::mutate(data_add, count = nrow(bottle_lines))
      data_add <- tidyr::uncount(data_add, count)

      data_add <- dplyr::mutate(data_add,
                                bottle = "SS",
                                depth = 0,
                                temperature = bottle_lines$temp,
                                pressure = 0,
                                salinity = bottle_lines$sal,
                                theta = oce::swTheta(salinity = salinity,
                                                     temperature = temperature,
                                                     pressure = pressure),
                                sigma = oce::swSigma0(salinity = salinity,
                                                      temperature = temperature,
                                                      pressure = pressure),
                                station = bottle_lines$station)
      # combine ros with bottle
      all_output <- dplyr::bind_rows(ros_output, data_add)


    } else {
      data_add <- tibble::tibble(bottle = "SS",
                                 depth = 0,
                                 temperature = bottle_lines$temp,
                                 pressure = 0,
                                 salinity = bottle_lines$sal,
                                 theta = oce::swTheta(salinity = salinity,
                                                      temperature = temperature,
                                                      pressure = pressure),
                                 sigma = oce::swSigma0(salinity = salinity,
                                                       temperature = temperature,
                                                       pressure = pressure))

      # set to be the total output as no ros data exists
      all_output <- data_add
    }
  } else {
    all_output <- ros_output
  }

  output <- dplyr::left_join(data,all_output, by = c("station","bottle"))


return(output)

}

##Part of function to take in a archive CSV and make it into a readable excel file
#data_input <- "~/GitHub/seaprocess/inst/extdata/Data Sheet CSVs/S285_NT.csv"

#data <- read.csv(data_input)

#colnames(data) <- stringr::str_to_title(colnames(data))
#colnames(data) <- stringr:: str_replace_all(colnames(data), "_", " ")
