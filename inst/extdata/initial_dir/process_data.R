### Master processing file for the cruise
###

# This script is intended to be edited. You wont mess anything up with the raw
# data or data entred into the input datasheets by changing anything in here.
# You *may* end up overwriting some output data, but that is all
# recoverable.


# Loading the processing library ------------------------------------------

library(seaprocess)



# Set up code parameters --------------------------------------------------
# Enter the cruise ID and the folder paths for where the data are stored.
#
# Filepaths likely to looks something like "Z:\\Data" for data stored on other
# machines and "ctd" for data folders in this project directory

cruiseID <- ""

## Data folders
elg_folder <- ""
ctd_folder <- "ctd/Cnv"
adcp_folder <- ""
ros_folder <- "ctd/Cnv"

## Datasheets
summary_input <- "datasheets/summary_input.xls"
ctd_input <- "datasheets/ctd_input.xls"
neuston_input <- "datasheets/neuston_input.xls"
bottle_input <- "datasheets/bottle_input.xls"
other_input <- ""




# Process non-datasheet data sources --------------------------------------

# Process elg (event file) data
process_elg(elg_folder, cruiseID = cruiseID, average_window = 60)

# Process ctd data
process_ctd(ctd_folder, cruiseID = cruiseID)

# Process ADCP data
process_adcp(adcp_folder, cruiseID = cruiseID)


# Process datasheets ------------------------------------------------------

# Start with creating a robust summary sheet of all deployments
create_summary(summary_input, elg_folder, cruiseID = cruiseID)

#
summary_csv <- file.path("output","csv",paste0(cruiseID,"_summary.csv"))

# CTD datasheet
create_datasheet(ctd_input, data_type = "CTD",
                 summary_csv = summary_csv,
                 cruiseID = cruiseID)

# Neuston datasheet
create_datasheet(neuston_input, data_type = "neuston",
                 summary_csv = summary_csv,
                 elg_input = elg_input,
                 cruiseID = cruiseID)

# Bottle datasheet
create_datasheet(bottle_input, data_type = "bottle",
                 summary_csv = summary_csv,
                 ros_input = ros_folder,
                 cruiseID = cruiseID)
