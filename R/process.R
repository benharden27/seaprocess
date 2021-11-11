# Functions that are designed to take the processing from a folder of files to
# a complete data archive and odv output
#

#' Master processing function for ADCP data
#'
#' @param adcp_folder
#' @param csv_folder
#' @param odv_folder
#' @param cruiseID
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
process_adcp <- function(adcp_folder, cruiseID = NULL,
                         csv_folder = "output/csv", csv_filename = "adcp.csv",
                         odv_folder = "output/odv/adcp", odv_filename = "adcp.txt",
                         add_cruiseID = TRUE, ...) {

  # Read in all adcp file in the folder
  adcp <- read_adcp_fold(adcp_folder, ...)

  # write adcp data to csv
  # add cruise ID to default output if not already existing
  if(!is.null(csv_filename) & !is.null(csv_folder)) {
    csv_output <- file.path(csv_folder, csv_filename)
    if(add_cruiseID == TRUE & !is.null(cruiseID)) {
      csv_output <- add_file_cruiseID(csv_output, cruiseID)
    }
    safely_write_csv(adcp, csv_output)
  }

  # Convert adcp to odv output
  if(!is.null(odv_folder)) {
    format_odv(adcp, file.path(odv_folder,odv_filename), data_type = "adcp", cruiseID = cruiseID)
  }
}

#' Master processing function for event data
#'
#' @param elg_folder
#' @param output_file
#'
#' @return
#' @export
#'
#' @examples
process_elg <- function(elg_folder, cruiseID = NULL,
                        csv_folder = "output/csv", csv_filename = "elg.csv",
                        odv_folder = "output/odv/elg", odv_filename = "elg.txt",
                        add_cruiseID = TRUE, average_window = 60, ...) {

  # Read in all the ELG files in the folder
  elg <- read_elg_fold(elg_folder, ...)

  # Average the elg data (default is to 60 mins)
  if(average_window > 1) {
    elg <- average_elg(elg, average_window = average_window)
  }

  # Output csv file
  # add cruise ID to default output if not already existing
  if(!is.null(csv_filename) & !is.null(csv_folder)) {
    csv_output <- file.path(csv_folder, csv_filename)
    if(add_cruiseID == TRUE & !is.null(cruiseID)) {
      csv_output <- add_file_cruiseID(csv_output, cruiseID)
    }
    safely_write_csv(elg, csv_output)
  }

  # output odv file
  if(!is.null(odv_filename) & !is.null(odv_folder)) {
    odv_output <- file.path(odv_folder, odv_filename)
    if(add_cruiseID == TRUE & !is.null(cruiseID)) {
      odv_output <- add_file_cruiseID(odv_output, cruiseID)
    }
    format_odv(elg, file.path(odv_folder,odv_filename), data_type = "elg", cruiseID = cruiseID)
  }

  return(elg)

}

#' Master processing function for ctd data
#'
#' @param ctd_folder
#' @param csv_folder
#' @param odv_folder
#' @param cruiseID
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
process_ctd <- function(ctd_folder, cruiseID = NULL,
                        csv_folder = "output/csv", csv_filename = "ctd.csv",
                        odv_folder = "output/odv/ctd", odv_filename = "ctd.txt",
                        add_cruiseID = TRUE, ...) {

  # create data
  ctd <- read_ctd_fold(ctd_folder, cruiseID = cruiseID, ...)

  # output to csv
  if(!is.null(csv_filename) & !is.null(csv_folder)) {
    csv_output <- file.path(csv_folder, csv_filename)
    if(add_cruiseID == TRUE & !is.null(cruiseID)) {
      csv_output <- add_file_cruiseID(csv_output, cruiseID)
    }
    safely_write_csv(ctd, csv_output)
  }


  # output odv
  if(!is.null(odv_filename) & !is.null(odv_folder)) {
    odv_output <- file.path(odv_folder, odv_filename)
    if(add_cruiseID == TRUE & !is.null(cruiseID)) {
      odv_output <- add_file_cruiseID(odv_output, cruiseID)
    }
    format_odv(ctd, file.path(odv_folder,odv_filename), data_type = "ctd", cruiseID = cruiseID)
  }

}


safely_write_csv <- function(data, csv_filepath = NULL) {
  print(csv_filepath)
  if(!is.null(csv_filepath)) {
    output <- purrr::safely(readr::write_csv)(data, csv_filepath)
    if(!is.null(output$error)) {
      print(output$error)
      message("Couldn't export the data to a csv file. Most likely specified directory doesn't exist")
    }
  }

}
