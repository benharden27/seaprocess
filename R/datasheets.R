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

  # read in the data_input datasheet
  data <- readxl::read_excel(data_input)

  # read in station summary datasheet
  # TODO: keep all formating as charactor when read in
  summary <- readr::read_csv(summary)

  # filter by data_type
  summary <- dplyr::filter(summary, deployment == data_type)

  data <- dplyr::right_join(summary,data,by="station")

  # TODO: Remove columns we don't need, e.g. deployment

  if(!is.null(csv_output)) {
    readr::write_csv(data,csv_output)
  }

  return(data)
}
