#TODO eliminate the confusion b/t bottle , bottle2, and bottle_sum. Which is from the csv, which is the utility object, and which is the product?
#TODO make it recognize that all the chl (and future nutrients) are the same variable.

#' Append calculation sheet data to bottle summary
#'
#' This function appends hand-entered and calculated data from calculation sheets to the bottle summary csv.
#' It takes the 'output' sheet on the calculation excel file with whatever header value is used and matches it with station number and bottle number.
#'
#'
#'
#'
#' @param calc_file #the calculation file to be read, chlorophyll, PO4, NO3, pH. This file must be formatted correctly with an 'output' sheet.
#' @param bottle_sum #the bottle summary from the [create_bottle()] function
#' @param bottle_is_file() #if TRUE, this will read the [bottle.csv] file and creat the [bottle] object. If FALSE (the default), it will append data to the bottle object.
#'
#' @return #creates the [bottle] object that will be updated with new data each time it is run.
#' @export #currently does not export a csv file. The final [bottle] object must be exported for use in ODV
#'
#'
#'@details
#'This function, and the [read_calc_fold()] function combine the [output] sheet from any calculation sheet (from path ['calc_file]) and the [bottle] object.
#'If [bottle] is not an object (ex. if it is the first time this is run) it will read the [bottle.csv] file from the [bottle_sum] diriectory and create the [bottle] object.
#'[create_bottle()] will overwrite the [bottle.csv] and delete any calc sheet data added in past uses of this function. RUnning this function again will replace these data.
#'
#'
#'
#'
#' @examples
read_calc_sheet <- function(calc_file, bottle_sum, bottle_is_file = FALSE) {

  #read calc sheet as specified in datasheet_examples.R
  calc_sheet <- readxl::read_excel(calc_file, sheet = "output")
  colnames(calc_sheet) <- stringr::str_to_lower(colnames(calc_sheet))
  #TODO Make it read the bottle collum, even if there are -, blanks or NA, N/A etc. Anything thats not a number is a SS and gets NA


  #if the column names contain "filter" (eg. it is a chlorophyll sheet), then make each filter size its own column with the chl values.
  if(c("filter") %in% colnames(calc_sheet)) {
    calc_sheet <-  pivot_wider(calc_sheet, names_from = "filter size", values_from = "chl")}


  #if bottle_is_file = TRUE, create an object 'bottle' from the bottle.csv. If FALSE (default), load the object 'bottle_sum'
  if(bottle_is_file == TRUE) {
    bottle <-  readr::read_csv(bottle_sum)



  } else{
    bottle <- bottle_sum
  }


  #join bottle and calc sheet tibbles into one. Common errors from header typos in the seperate excel sheets. Check spelling.
  #TODO is bottle2 still a valid object? Just use bottle?
  bottle2 <- left_join(bottle, calc_sheet, by = c('station', 'bottle'))

  return(bottle2)
}


#'#Read a folder of calculation sheets, and append the data to the bottle summary
#'
#'This function reads the [output] sheet of every excel file in a folder and appends the new values to the bottle summary object [bottle_sum].
#'
#'
#'
#' @param calc_folder #the calculation folder to be read. chlorophyll, PO4, NO3, pH. All excel files must be formatted correctly with an 'output' sheet.
#' @param bottle_sum #the bottle summary from the [create_bottle()] function
#' @param bottle_is_file #if TRUE (the default) this will read the [bottle.csv] file and creat the [bottle_sum] object. If FALSE, it will append data to the bottle object.
#'
#' @return
#' @export #currently does not export an ODV csv file.
#'
#' @details This function, and the [read_calc_file()] function combine the [output] sheet from any calculation excel file (from path ['calc_file]) and the [bottle_sum] object.
#'If [bottle] is not an object (ex. if it is the first time this is run) it will read the [bottle.csv] file from the [bottle_sum] diriectory and create the [bottle_sum] object.
#'[create_bottle()] will overwrite the [bottle.csv] and delete any calc sheet data added in past uses of this function. RUnning this function again will replace these data.
#'This function will read every excel file in the specified folder [calc_folder] from [datasheet_examples.R] and append the [output] sheet to the bottle summary csv.
#'Common errors arise from spelling typos in the header data of the many calculation excel files, or from unexpected values in the columns (characters vs. numbers etc.).
#'Individual files may be run with the [read_calc_file()] function for troubleshooting.
#'
#' @examples
read_calc_fold <- function(calc_folder, bottle_sum, bottle_is_file = TRUE) {

  #if bottle_is_file = TRUE (default), create the object 'bottle_sum' from the bottle.csv. If FALSE, load the object 'bottle_sum'.
  if(bottle_is_file == TRUE) {
    bottle <-  readr::read_csv(bottle_sum)

  } else{
    bottle <- bottle_sum
  }

  #a loop that reads all the excel files in the specified folder and calls the read_calc_file() function on each one.
  calc_files <- list.files(calc_folder,pattern = '\\.xls$|\\.xlsx$')
  if(length(calc_files) > 0) {
    for (i in 1:length(calc_files)) {
      filein <- file.path(calc_folder,calc_files[i])
      bottle <- read_calc_sheet(filein, bottle)
       }
  } else {
    stop("No calc files in specified folder.")
    calc <- NULL
  }
  return(bottle)

  #should we make this auto-output a CSV file?

}
