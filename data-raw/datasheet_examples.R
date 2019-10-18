# Create output csv datasheets from example inputs from cruise S285

# Create Station summary
summary_input <- "inst/extdata/S285_station.xlsx"
elg_input <- "inst/extdata/S285_elg/"
csv_output <- "inst/extdata/S285_station.csv"
seaprocess::create_summary(summary_input, elg_input, csv_output = csv_output)

# Create CTD datasheet
data_input <- "inst/extdata/S285_ctd.xlsx"
summary <- "inst/extdata/S285_station.csv"
csv_output <- "inst/extdata/S285_ctd.csv"
seaprocess::create_datasheet(data_input, summary , csv_output = csv_output)
