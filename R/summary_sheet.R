#' Create Station Summary Sheet
#'
#' This function combines hand-entered station metadata with electronically recorded instrument data to output a well formatted station summary sheet.
#'
#' @param summary_input The input datasheet that includes the relevent station and deployment metadata
#' @param elg The cruise elg file or folder or files for extracting continuous data from
#'
#' @return
#' @export
#'
#' @examples
#' # Create a summary from S285 data input and elgs
#' summary_input <- system.file("extdata", "S285_station.xlsx", package="seaprocess")
#' elg_input <- system.file("extdata", "S285_elg", package="seaprocess")
#' summary <- create_summary(summary_input, elg_input)
create_summary <- function(summary_input, elg_input, csv_output = NULL) {

  # read in the summary_input xlsx file
  summary <- readxl::read_excel(summary_input)

  # convert zone description to time zone
  summary <- dplyr::mutate(summary, tz = zd_to_tz(zd))

  # combine date and time and convert to R datatime object using specified time zone
  summary <- dplyr::mutate(summary, dttm = do.call("c",
                                                   purrr::pmap(list(paste(summary$date,summary$time),
                                                                    tz=summary$tz),
                                                               lubridate::mdy_hm)))

  # Test to see whether elg_input is a file or a folder and read elg file(s) accordingly
  # TODO: add ability to tune reading elg per options provided in that function
  # TODO: find way to store Rdata file in local folder so we don't have the delay of loading
  # TODO: confirm consistency in field names
  if(file_test("-f",elg_input)) {
    elg <- read_elg(elg_input)
  } else {
    elg <- read_elg_fold(elg_input)
  }

  # find all the nearest date time values of summary sheet to the elg file and add these indeces
  # TODO: what happens if any of ii are blank or at beginning or end of the elg?
  # TODO: add fail safe to ensure that times are actually close
  ii <- find_near(elg$dttm, summary$dttm)

  # extract these values and add to the right of summary
  # TODO: make the ourpurs selectable when you run the function
  elg_to_add <- dplyr::select(elg[ii,], lon, lat, temp, sal, fluor)
  summary <- dplyr::bind_cols(summary,elg_to_add)

  # TODO: add checks to ensure that there are no duplicate deployments for any one station

  if(!is.null(csv_output)) {
    readr::write_csv(summary,csv_output)
  }

  return(summary)

}

#' Zone Description to Time Zone
#'
#' @param zd
#'
#' @return
#' @export
#'
#' @examples
zd_to_tz <- function(zd) {

  tz <- as.numeric(zd) * -1
  tz <- as.character(tz)
  ii <- !stringr::str_sub(tz,1,1) == "-"
  tz[ii] <- paste0("+", tz[ii])
  tz<- paste0("Etc/GMT",tz)

  return(tz)
}



