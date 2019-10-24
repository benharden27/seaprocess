#' Create complete data sheet
#'
#' @param data_input
#' @param summary
#'
#' @return
#' @export
#'
#' @examples
create_datasheet <- function(data_input, summary, data_type = "CTD", csv_output = NULL, elg_input = NULL) {

  # read in the data_input excel sheet datasheet
  data <- readxl::read_excel(data_input)

  # read in station summary datasheet
  # TODO: determine what formating to apply when read in (beyond zd)
  summary <- readr::read_csv(
    summary,
    col_types = readr::cols(
      zd = readr::col_character()
    )
  )

  # filter by data_type
  summary <- dplyr::filter(summary, deployment == data_type)

  data <- dplyr::right_join(summary,data,by="station")

  # Neuston specific stuff
  if(data_type == "NT") {
    data <- compile_neuston(data, elg_input)
  }

  # TODO: Remove columns we don't need, e.g. deployment

  if(!is.null(csv_output)) {
    readr::write_csv(format_csv_output(data),csv_output, na = "")
  }

  return(data)
}

compile_neuston <- function(data, elg_input) {

  # find out time_out in UTC
  time_diff <-   time_length(lubridate::hms(data$time_out) - lubridate::hms(data$time_in), unit = "minute")
  time_diff[time_diff<0] <- time_diff[time_diff<0] + 60 * 24
  data <- dplyr::mutate(data,dttm_out = dttm + lubridate::minutes(time_diff))

  # read in elg
  elg <- get_elg(elg_input)


  sti <- find_near(elg$dttm, data$dttm)
  eni <- find_near(elg$dttm, data$dttm_out)
  data <- dplyr::mutate(data, dist = NA)
  for (i in 1:length(sti)) {
    data$dist[i] <- tail(
      oce::geodDist(
        elg$lon[sti[i]:eni[i]],
        elg$lat[sti[i]:eni[i]],
        alongPath = TRUE),1)
  }

  return(data)
}
