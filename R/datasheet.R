#' Create complete data sheet
#'
#' @param data_input
#' @param summary
#'
#' @return
#' @export
#'
#' @examples
create_datasheet <- function(data_input, summary, data_type = "ctd", csv_output = NULL) {

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

  # TODO: Remove columns we don't need, e.g. deployment

  if(!is.null(csv_output)) {
    readr::write_csv(format_csv_output(data),csv_output, na = "")
  }

  return(data)
}
