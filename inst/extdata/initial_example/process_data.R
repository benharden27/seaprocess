### Master processing file for the cruise
###

# This script is intended to be edited. You wont mess anything up with the raw
# data or data entred into the input datasheets by changing anything in here.
# You *may* end up overwriting some output data, but that is all
# recoverable.


# Loading the processing library ------------------------------------------

library(seaprocess)



# Set up code parameters --------------------------------------------------
# Enter the cruise code and the folder paths for where the data are stored.
#
# Filepaths likely to looks something like "Z:\\Data" for data stored on other
# machines and "ctd/Cnv" for data folders in this project directory

cruiseID <- ""

adcp_folder <- "adcp"
ctd_folder <- "ctd/Cnv"
elg_folder <- "elg"
ros_folder <- "ctd/Cnv"

summary_input <- "datasheets/summary_input.xls"
ctd_input <- "datasheets/ctd_input.xls"
neuston_input <- "datasheets/neuston_input.xls"
bottle_input <- "datasheets/bottle_input.xls"
other_input <- ""



# Process non-datasheet data sources --------------------------------------

# Process ADCP data
process_adcp(adcp_folder, cruiseID = cruiseID)

# Process elg (event file) data
process_elg(elg_folder, cruiseID = cruiseID, average_window = 60)

# Process ctd data
process_ctd(ctd_folder, cruiseID = cruiseID)



# Process datasheets ------------------------------------------------------

create_summary(summary_input, elg_folder, cruiseID = cruiseID)


# Neuston datasheet
create_datasheet(neuston_input, data_type = "neuston",
                 elg_input = elg_folder,
                 cruiseID = cruiseID)

# Bottle datasheet
create_datasheet(bottle_input, data_type = "bottle",
                 ros_input = ros_folder,
                 cruiseID = cruiseID)

# ctd datasheet
create_datasheet(ctd_input, data_type = "CTD",
                 cruiseID = cruiseID)

# example of changing output folder/filename for trial runs and specific cases
create_datasheet(neuston_input, data_type = "neuston",
                 csv_folder = "~/Desktop/", csv_filename = "test.csv",
                 elg_input = elg_folder,
                 cruiseID = cruiseID)
