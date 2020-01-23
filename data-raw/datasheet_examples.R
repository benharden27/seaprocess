# Create output csv datasheets from example inputs from cruise S285

# Create Station summary
summary_input <- "inst/extdata/S285_station.xlsx"
elg_input <- "inst/extdata/S285_elg/"
csv_output <- "inst/extdata/S285_station.csv"
create_summary(summary_input, elg_input, csv_output = csv_output)

# Create CTD datasheet
data_input <- "inst/extdata/S285_ctd.xlsx"
summary_input <- "inst/extdata/S285_station.csv"
csv_output <- "inst/extdata/S285_ctd.csv"
data_type = "CTD"
create_datasheet(data_input, summary , csv_output = csv_output, data_type = data_type)

# Create bottle datasheet
ctd_file <- "inst/extdata/S285_ctd.csv"
ros_folder <- "inst/extdata/S285_ctd/"
csv_output <- "inst/extdata/S285_bottle.csv"
create_bottle(ctd_file, ros_folder , csv_output = csv_output)

#Append calculation sheet data to bottle datasheet
calc_file <- "inst/extdata/Calculation Sheets/CalcChlaNoAcid_0.45_run1.xls"
bottle_sum <- "inst/extdata/S285_bottle.csv"
read_calc_sheet(calc_file, bottle_sum)

#Read a folder of calculation sheets, all must contain the 'output' sheet
calc_folder <- "inst/extdata/Calculation Sheets"
bottle_sum <- "inst/extdata/S285_bottle.csv"
bottle_test <- read_calc_fold(calc_folder, bottle_sum)

# Create neuston datasheet
data_input <- "inst/extdata/S285_neuston.xlsx"
summary <- "inst/extdata/S285_station.csv"
csv_output <- "inst/extdata/S285_neuston.csv"
elg_input <- "inst/extdata/S285_elg/"
data_type = "NT"
create_datasheet(data_input, summary, csv_output = csv_output, data_type = data_type, elg_input = elg_input)
