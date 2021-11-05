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
# machines and "ctd" for data folders in this project directory

cruiseID <- ""
adcp_folder <- "adcp"
ctd_folder <- "ctd"
elg_folder <- "elg"



# Process non-datasheet data sources --------------------------------------

# Process ADCP data
process_adcp(adcp_folder, cruiseID = cruiseID)

# Process elg (event file) data
process_elg(elg_folder, cruiseID = cruiseID, average_window = 60)

# Process ctd data
process_ctd(ctd_folder, cruiseID = cruiseID)



# Process datasheets ------------------------------------------------------

# create_summary()
#
# create_neuston()
#
# create_meter()
#
# create_bottle()
#
# create_ctd()
#
# create_other_datasheet()
