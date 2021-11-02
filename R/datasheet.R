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
create_datasheet <- function(data_input, summary_input, data_type = "CTD", csv_output = NULL, elg_input = NULL) {

  # read in the data_input excel sheet datasheet
  data <- read_csv(data_input)
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
    summary_csv,
    col_types = readr::cols(
      zd = readr::col_character()
    )
  )

  # filter by data_type
  summary <- dplyr::filter(summary, deployment == data_type)

  data <- dplyr::left_join(summary, data, by="station")

  # Neuston specific stuff
  if(data_type == "NT") {
    data <- compile_neuston(data, elg_input)
  }

  # Remove columns we don't need, e.g. deployment
  data <- dplyr::select(data, -deployment)

  # export to csv
  if(!is.null(csv_output)) {
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


##Part of function to take in a archive CSV and make it into a readable excel file
#data_input <- "~/GitHub/seaprocess/inst/extdata/Data Sheet CSVs/S285_NT.csv"

#data <- read.csv(data_input)

#colnames(data) <- stringr::str_to_title(colnames(data))
#colnames(data) <- stringr:: str_replace_all(colnames(data), "_", " ")
