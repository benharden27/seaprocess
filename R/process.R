# Functions that are designed to take the processing from a folder of files to
# a complete data archive and odv output
#

#' Master processing function for ADCP data
#'
#' Function takes in a filepath to a folder containing the cruise adcp files,
#' reads them in, combines them into one data object and then exports to a
#' comma-separated-file and an odv text file.
#'
#' Most likely optional arguments to go in for ...
#'
#' @param adcp_folder filepath to the folder containing adcp files
#' @param cruiseID Optional string specifying cruise ID (i.e. "S301")
#' @param csv_folder The directory path to output the csv file. Set to NULL for
#'   no csv output.
#' @param csv_filename The csv filename to output the data
#' @param odv_folder The directory path to output the odv file. Set to NULL for
#'   no odv output.
#' @param odv_filename The odv .txt filename to output the data
#' @param add_cruise_ID If cruiseID is set, logical to specify whether cruiseID
#'   should be appended to beginning of filenames for csv and odv output
#' @param ... option arguments to be passed to [read_adcp_fold()]. A few
#'   options, but most likely is the file_type which defaults to '.LTA' (long
#'   term averages), but can be changed to ".STA" if you want to read in the
#'   short term averages
#'
#' @return
#' @export
#'
#' @md
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
#' Function takes in a filepath to a folder containing the cruise elg files,
#' reads them in, combines them into one data object and then exports to a
#' comma-separated-file and an odv text file.
#'
#' Note that the elg folder doesn not have to solely contain elg files, the
#' reading function will find all the elg files amoungst the other files
#' exported by SCS
#'
#' @param elg_folder filepath to the folder containg elg files
#' @param cruiseID Optional string specifying cruise ID (i.e. "S301")
#' @param csv_folder The directory path to output the csv file. Set to NULL for
#'   no csv output.
#' @param csv_filename The csv filename to output the data
#' @param odv_folder The directory path to output the odv file. Set to NULL for
#'   no odv output.
#' @param odv_filename The odv .txt filename to output the data
#' @param add_cruise_ID If cruiseID is set, logical to specify whether cruiseID
#'   should be appended to beginning of filenames for csv and odv output
#' @param average_window the averaging window in minutes for the exported file.
#'   Set to NULL for no averaging. Default is 60 minutes.
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
#' Function takes in a filepath to a folder containing the cruise cnv CTD files,
#' reads them in, combines them into one data object and then exports to a
#' comma-separated-file and an odv text file.
#'
#' @param ctd_folder filepath to the folder containing ctd cnv files
#' @param cruiseID Optional string specifying cruise ID (i.e. "S301")
#' @param csv_folder The directory path to output the csv file. Set to NULL for
#'   no csv output.
#' @param csv_filename The csv filename to output the data
#' @param odv_folder The directory path to output the odv file. Set to NULL for
#'   no odv output.
#' @param odv_filename The odv .txt filename to output the data
#' @param add_cruise_ID If cruiseID is set, logical to specify whether cruiseID
#'   should be appended to beginning of filenames for csv and odv output
#' @param ... optional arguments passed to [read_ctd_fold()]. Most likely one
#'   you'd set would be the depth_step which is the depth resolution of output
#'   data in meters. Default is 1m.
#'
#' @md
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
