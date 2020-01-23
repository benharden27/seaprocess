#' Create bottle file sheet
#'
#' @param summary_csv
#' @param ros_folder
#' @param datasheet_folder
#'
#' @return
#' @export
#'
#' @examples
create_bottle <- function(summary_csv, ros_folder, datasheet_folder = NULL, csv_output = NULL) {

  # read ctd datasheet summary
  data <- readr::read_csv(summary_csv, col_types = readr::cols(zd = readr::col_character()))


  # filter to just CTD and HC stations
  data_ctd <- dplyr::filter(data, deployment == "CTD" | deployment == "HC")

  # then remove all the extra columns we wont need
  data_ctd <- dplyr::transmute(data_ctd, station, deployment, date, time_in, zd, dttm, lon, lat)

    # Go find appropriate bottle files from ctd
  # list all files with *.ros extension in ros_folder
  create_output = TRUE
  ros_files <- list.files(ros_folder,pattern = "\\.ros")
  for (i in 1:length(data_ctd$station)) {
    ros_file <- ros_files[stringr::str_detect(ros_files,data_ctd$station[i])]
    # TODO test for length == 1
    if(length(ros_file)>0) {

      # Read in the ros file and arrange in bottle decending order
      ros <- read_ros(file.path(ros_folder,ros_file))
      ros <- dplyr::arrange(ros, desc(bottle))

      # duplicate the ctd info to be the same number of rows
      ctd_info <- dplyr::mutate(data_ctd[i, ], count = nrow(ros))
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
  data_ss <- dplyr::filter(data, deployment == "SS")
  data_ss <- rename(data_ss, temperature = temp, salinity = sal, fluorescence = fluor)
  data_ss <- mutate(data_ss, depth = 0)
  data_ss <- select(data_ss, -time_out)


  #join the SS data with the ros output data
  output <- dplyr::bind_rows(output, data_ss)
  output <- dplyr::arrange(output, by = station)

  # then go capture output from calcsheets - add in and sort
  test <- read_calc_fold(calc_folder, output, bottle_is_file = FALSE)

  # output as csv
  if(!is.null(csv_output)) {
    readr::write_csv(format_csv_output(output),csv_output, na = "")
  }


}
