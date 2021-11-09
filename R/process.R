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
  if(add_cruiseID) {
    csv_filename <- add_file_cruiseID(csv_filename, cruiseID)
  }
  # write file
  safely_write_csv(adcp, file.path(csv_folder,csv_filename))

  # Convert adcp to odv output
  if(!is.null(odv_folder)) {
    format_adcp_odv(adcp, file.path(odv_folder,odv_filename), cruiseID = cruiseID)
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
  if(add_cruiseID) {
    csv_filename <- add_file_cruiseID(csv_filename, cruiseID)
  }
  # write file
  safely_write_csv(elg, file.path(csv_folder,csv_filename))

  # output odv file
  if(!is.null(odv_folder)) {
    format_elg_odv(elg, file.path(odv_folder,odv_filename), cruiseID = cruiseID)
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

  #
  ctd <- read_ctd_fold(ctd_folder, cruiseID = cruiseID, ...)
  if(add_cruiseID) {
    csv_filename <- add_file_cruiseID(csv_filename, cruiseID)
  }
  safely_write_csv(ctd, file.path(csv_folder,csv_filename))

  if(!is.null(odv_folder)) {
    format_ctd_odv(ctd, file.path(odv_folder,odv_filename), cruiseID = cruiseID)
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


process_datasheet <- function(datasheet_input, summary_csv,
                              data_type = "CTD", ...) {




}


add_file_cruiseID <- function(filename, cruiseID) {

  if(!is.null(filename) & !is.null(cruiseID)) {
    file_dir <- dirname(filename)
    filename <- basename(filename)
    filename <- paste0(cruiseID,"_",filename)
    filename <- file.path(file_dir,filename)
  } else {
    stop("filename or cruiseID are not set")
  }

  return(filename)

}
