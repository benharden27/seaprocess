# Functions that are designed to take the processing from a folder of files to
# a complete data archive and odv output
#

process_adcp <- function(adcp_folder, ...) {

  # Read in all adcp file in the folder
  adcp <- read_adcp_fold(adcp_folder, ...)


  # Convert adcp to csv output
  # Convert adcp to odv output
}

#' Title
#'
#' @param elg_folder
#' @param output_file
#'
#' @return
#' @export
#'
#' @examples
process_elg <- function(elg_folder, output_csv = NULL, output_odv = NULL,
                        average_window = 60, cruiseID = NULL, ... ) {

  # Read in all the ELG files in the folder
  elg <- read_elg_fold(elg_folder, ...)

  # Average the elg data (default is to 60 mins)
  if(average_window > 1) {
    elg <- average_elg(elg, average_window = average_window)
  }

  # Output csv file
  if(!is.null(output_csv)) {
    output <- purrr::safely(readr::write_csv)(elg, output_csv)
    if(!is.null(output$error)) {
      message("Couldn't export the data to a csv file. Most likely specified directory doesn't exist")
    }
  }

  # output odv file
  if(!is.null(output_odv)) {
    format_elg_odv(elg, output_odv, cruiseID = cruiseID)
  }

  return(elg)

}
