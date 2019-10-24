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
  summary <- readxl::read_excel(summary_input, col_types = "text")

  # combine date and time and convert to R datatime object using specified time zone
  summary <- dplyr::mutate(
    summary,
    dttm = lubridate::ymd_hm(
      paste(summary$date,summary$time_in)
      ) + lubridate::hours(summary$zd)
    )


  # Test to see whether elg_input is a file or a folder and read elg file(s) accordingly
  # TODO: add ability to tune reading elg per options provided in that function
  # TODO: find way to store Rdata file in local folder so we don't have the delay of loading
  # TODO: confirm consistency in field names
  if(file_test("-f",elg_input)) {
    elg <- sea::read_elg(elg_input)
  } else {
    elg <- sea::read_elg_fold(elg_input)
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
    readr::write_csv(format_csv_output(summary),csv_output)
  }

  return(summary)

}

#' Format for CSV output
#'
#' This is a catch-all function to ensure that everytime a data-frame in this
#' software is exported to a csv that certain fields are formated correctly
#'
#' @param df Data frame to be exported
#' @param dttm_format dttm_format
#'
#' @return
#' @export
#'
#' @examples
format_csv_output <- function(df, dttm_format = "%Y-%m-%dT%H:%M", dttm_suffix = "Z", ll_dec = 4, temp_dec = 2, sal_dec = 3, fluor_dec = 2) {

  # Format date and time to be in ISO 8601 format including timezone
  if("zd" %in% colnames(df) & "dttm" %in%colnames(df))
    df$dttm <- paste0(format(df$dttm - lubridate::hours(df$zd), dttm_format), zd_to_tz(df$zd))


  # TODO: create list of all column names and default resolutions

  # Format lon and lat to ~10 m resolution (by default)
  df <- format_decimal(df, "lon", ll_dec)
  df <- format_decimal(df, "lat", ll_dec)

  # Format temp, sal and fluor to set decimal lengths
  df <- format_decimal(df, "temp", temp_dec)
  df <- format_decimal(df, "sal", sal_dec)
  df <- format_decimal(df, "fluor", fluor_dec)
  df <- format_decimal(df, "depth", 1)

  return(df)
}


#' Format to number of decimal places
#'
#' Rather than significant figures as format() provides in its digits = 2 option.
#'
#' @param value
#' @param digits
#'
#' @return
#' @export
#'
#' @examples
format_decimal <- function(df, col_name, digits) {

  if(col_name %in% colnames(df)){
    df[col_name] <- format(round(df[[col_name]],digits), nsmall = digits)
  }

  return(df)
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

  return(tz)
}
