#' Create bottle file sheet
#'
#' @param ctd_summary
#' @param ros_folder
#' @param datasheet_folder
#'
#' @return
#' @export
#'
#' @examples
create_bottle <- function(ctd_file, ros_folder, datasheet_folder = NULL, csv_output = NULL) {

  # read ctd datasheet summary
  data <- readr::read_csv(ctd_file, col_types = readr::cols(zd = readr::col_character()))

  # then remove all the extra columns we wont need
  data <- dplyr::transmute(data, station, date, time_in, zd, dttm, lon, lat)

  # Go find appropriate bottle files from ctd
  # list all files with *.ros extension in ros_folder
  create_output = TRUE
  ros_files <- list.files(ros_folder,pattern = "\\.ros")
  for (i in 1:length(data$station)) {
    ros_file <- ros_files[stringr::str_detect(ros_files,data$station[i])]
    if(length(ros_file)>0) {

      # Read in the ros file and arrange in bottle decending order
      ros <- read_ros(file.path(ros_folder,ros_file))
      ros <- dplyr::arrange(ros, desc(bottle))

      # duplicate the ctd info to be the same number of rows
      ctd_info <- dplyr::mutate(data[i, ], count = nrow(ros))
      ctd_info <- tidyr::uncount(ctd_info, count)

      # combine the two data frames
      ros <- dplyr::bind_cols(ctd_info, ros)

      # create or add to the output data
      if(create_output) {
        output <- ros
        create_output = FALSE
      } else {
        output <- dplyr::bind_rows(output,ros)
      }
    }
  }

  # then go find appropriate surface station information - add in and sort


  # then go capture output from calcsheets - add in and sort


  # output as csv
  if(!is.null(csv_output)) {
    readr::write_csv(format_csv_output(output),csv_output, na = "")
  }


}
