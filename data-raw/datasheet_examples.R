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
create_datasheet(data_input, summary , csv_output = csv_output, data_type = "CTD")

# Create bottle datasheet
ctd_file <- "inst/extdata/S285_ctd.csv"
ros_folder <- "inst/extdata/S285_ctd/"
csv_output <- "inst/extdata/S285_bottle.csv"
create_bottle(ctd_file, ros_folder , csv_output = csv_output)

# Create neuston datasheet
data_input <- "inst/extdata/S285_neuston.xlsx"
summary <- "inst/extdata/S285_station.csv"
csv_output <- "inst/extdata/S285_neuston.csv"
elg_input <- "inst/extdata/S285_elg/"
create_datasheet(data_input, summary, csv_output = csv_output, data_type = "NT", elg_input = elg_input)
