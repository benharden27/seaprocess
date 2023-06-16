#' Create complete data sheet from xls input
#'
#' This function reads in a hand-entered excel data sheet along with a station
#' summary csv and combines them to produce well-formatted csv and odv file
#' outputs.
#'
#' data_input is the only argument that the function *needs* to create an
#' output. The default values for summary_input and the csv and odv outputs are
#' set to work with the default directory configuration of the SEA cruise
#' project so shouldn't need to be set unless you are trying to produce
#' alternative outputs for specific custom cases.
#'
#' data_type can be either a stand-alone value entered into the deployment
#' column of the station summary sheet. Or it can take one of these special
#' cases:
#'
#' * "neuston" for neuston datasheets
#' * "CTD" for ctd datasheets
#' * "bottle" for bottle datasheets
#' * "meter" for meter net datasheets
#'
#' @param data_input Filepath for the xls file with hand-recorded data values
#' @param summary_input Filepath for the csv summary datasheet produced with
#'   create_summary() (see details below)
#' @param data_type The data type code that will draw data from the summary
#'   sheet (see details below)
#' @param csv_folder The directory path to output the csv file. Set to NULL for
#'   no csv output.
#' @param csv_filename The csv filename to output the data
#' @param odv_folder The directory path to output the odv file. Set to NULL for
#'   no odv output.
#' @param odv_filename The odv .txt filename to output the data
#' @param cruiseID Optional string specifying cruise ID (i.e. "S301")
#' @param add_cruise_ID If cruiseID is set, logical to specify whether cruiseID
#'   should be appended to beginning of filenames for csv and odv output
#' @param add_deployment_type logical to tell function whether to add a new
#'   directory to the end of the odv_folder directory path to keep .txt files
#'   separate. Will create the new directory name depending on data_type
#' @param ... option arguments to be sent to compile_bottle. Initially, this is
#'   just ros_input
#'
#'
#' @return If assigned to an object, the function will return the formatted
#'   tibble that was exported to csv
#'
#' @md
#'
#' @export
#'
create_datasheet <- function(data_input, summary_input = "output/csv/summary_datasheet.csv",
                             data_type = "CTD",
                             csv_folder = "output/csv", csv_filename = "datasheet.csv",
                             odv_folder = "output/odv", odv_filename = "datasheet.txt",
                             cruiseID = NULL, add_cruiseID = TRUE,
                             add_deployment_type = TRUE,
                             add_deployment_subfold = TRUE, ...) {

  if(add_cruiseID == TRUE & !is.null(cruiseID)) {
    if(summary_input == "output/csv/summary_datasheet.csv") {
      summary_input <- add_file_cruiseID(summary_input, cruiseID)
    }
  }


  odv_export <- FALSE
  if(data_type == "bottle") {

    if(add_deployment_type) {
      csv_filename <- paste0("bottle_", csv_filename)
      odv_filename <- paste0("bottle_", odv_filename)
    }
    odv_export <- TRUE
    if(add_deployment_subfold) {
      odv_folder <- file.path(odv_folder,"bottle")
    }
    data_type <- c("HC","B")

  } else if (data_type == "neuston") {

    if(add_deployment_type) {
      csv_filename <- paste0("neuston_", csv_filename)
      odv_filename <- paste0("neuston_", odv_filename)
    }
    odv_export <- TRUE
    if(add_deployment_subfold) {
      odv_folder <- file.path(odv_folder,"neuston")
    }
    data_type <- "NT"

  } else if (data_type == "CTD") {
    data_type <- c("CTD","HC")
    if(add_deployment_type) {
      csv_filename <- paste0("ctd_", csv_filename)
      odv_filename <- paste0("ctd_", odv_filename)
    }
  } else if (data_type == "meter") {

    if(add_deployment_type) {
      csv_filename <- paste0("meter_", csv_filename)
      odv_filename <- paste0("meter_", odv_filename)
    }
    odv_export <- TRUE
    if(add_deployment_subfold) {
      odv_folder <- file.path(odv_folder,"meter")
    }
    data_type <- c("MN")
  } else {
    if(add_deployment_type) {
      csv_filename <- paste0(stringr::str_to_lower(data_type), "_", csv_filename)
      odv_filename <- paste0(stringr::str_to_lower(data_type), "_", odv_filename)
    }
  }




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



  # Bottle specific stuff
  if(sum(data_type %in% c("HC", "B")) > 1) {

    data <- dplyr::mutate(data, bottle = as.character(bottle))
    # add a new column to aid the joining later
    data <- dplyr::mutate(data, deployment = ifelse(is.na(as.numeric(bottle)) | as.numeric(bottle) > 12, "B", "HC"))

    data <- dplyr::right_join(summary, data, by=c("station","deployment"))

    data <- compile_bottle(data, ...)

  } else {
    data <- dplyr::right_join(summary, data, by=c("station"))
  }


  # Neuston specific stuff
  if(sum(data_type %in% "NT")>0) {
    data <- compile_neuston(data)
  }

  # Neuston specific stuff
  if(sum(data_type %in% c("MN","2MN")) > 0) {
    data <- compile_meter(data)
  }

  # Remove columns we don't need, e.g. deployment
  data <- dplyr::select(data, -deployment)

  # export to csv
  if(!is.null(csv_filename) & !is.null(csv_folder)) {
    csv_output <- file.path(csv_folder, csv_filename)
    if(add_cruiseID == TRUE & !is.null(cruiseID)) {
      csv_output <- add_file_cruiseID(csv_output, cruiseID)
    }
    readr::write_csv(format_csv_output(data),csv_output, na = "")
  }

  # export to odv
  if(odv_export) {
    if(!is.null(odv_filename) & !is.null(odv_folder)) {
      odv_output <- file.path(odv_folder, odv_filename)
      if(add_cruiseID == TRUE & !is.null(cruiseID)) {
        odv_output <- add_file_cruiseID(odv_output, cruiseID)
      }
      format_odv(data, odv_output, data_type = data_type, cruiseID = cruiseID)
    }
  }



  return(data)

}


compile_meter <- function(data) {


  data <- dplyr::mutate(data, total_flow = ifelse(is.na(total_flow),
                                                  flow_out - flow_in,
                                                  total_flow),
                        .after = flow_in)

  data <- dplyr::mutate(data, tow_length = ifelse(is.na(tow_length),
                                                  total_flow * flow_constant,
                                                  tow_length),
                        .after = flow_constant)

  data <- dplyr::mutate(data, tow_volume = ifelse(is.na(tow_volume),
                                                  tow_length * net_area,
                                                  tow_volume),
                        .after = net_area)

  data <- dplyr::mutate(data, biodens = as.numeric(zooplankton_biovol)/tow_volume, .after = zooplankton_biovol)

}


#' @export
#' @rdname compile_neuston
compile_neuston <- function(data) {

  # calculate biodensity
  if(length(which(is.na(data$station_distance)))>0) {
    warning("One or more tow distances are not available - be sure that they exist in the summary data csv")
  }

  # Make sure all non note/description/station columns are numeric
  data <- dplyr::mutate(data,dplyr::across(!dplyr::matches("note|desc|stat"),as.numeric))

  # calculate the biodensity
  data <- dplyr::mutate(data, biodens = zooplankton_biovol/(station_distance/1000))
  data <- dplyr::relocate(data, biodens, .after = zooplankton_biovol)

  # sum the total 100 count animals
  data <- dplyr::rowwise(data)
  data <- dplyr::mutate(data, total_100count = sum(dplyr::c_across(medusa:other3)))
  # data <- dplyr::mutate(data, shannon_wiener = sum(dplyr::c_across(medusa:other3)/total_100count * log(dplyr::c_across(medusa:other3)/total_100count)))
  data <- dplyr::ungroup(data)

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
      ros <- dplyr::mutate(ros, station = stations[i])
    } else {
      warning(paste("No .ros file found for station",
                    stations[i]))
      ros <- NULL
    }

    if(!is.null(ros)) {
      if(is.null(ros_output)) {
        ros_output <- ros
      } else {
        ros_output <- dplyr::bind_rows(ros_output, ros)
      }
    }

  }

  if(!is.null(ros_output)) {
    ros_output$bottle <- as.character(ros_output$bottle)
  }

  # Now that we have our depths and metadata for each bottle in a hydrocast, add all the bucket samples to this
  bottle_lines <- dplyr::filter(data, deployment == "B")
  if(nrow(bottle_lines) > 0) {
    if(!is.null(ros_output)) {
      data_add <- purrr::quietly(tibble::as_tibble)(t(rep(NA_real_, ncol(ros_output))))$result
      names(data_add) <- names(ros_output)
      data_add <- dplyr::mutate(data_add, count = nrow(bottle_lines))
      data_add <- tidyr::uncount(data_add, count)

      data_add <- dplyr::mutate(data_add,
                                bottle = "B",
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
