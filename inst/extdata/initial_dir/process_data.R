### Master processing file for the cruise
###

# This script is intended to be edited. You wont mess anything up with the raw
# data or data entered into the input datasheets by changing anything in here.
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

# Data folders
#
# elg_folder will most likely be on the datalogger computer. This folder has to
# just contain data from this cruise. It can have other files other than .elg
# files in there, but shouldn't have data from previous cruises
#
# ctd_folder can be the local ctd folder where you can put .cnv and .ros files
# in from other "Cnv" folder or you direct the code to this other folder
# directly
#
# ros_folder can be the same as ctd_folder
#
# adcp_folder will probably be on the remote adcp machine. It can have all the
# ADCP files in there, but will only read, by default the LTA files
#
elg_folder <- "<enter-location-of-elg-folder>"
ctd_folder <- "<enter-location-of-ctd-folder>"
ros_folder <- "<enter-location-of-ros-folder>"
adcp_folder <- "<enter-location-of-adcp-folder>"



# Datasheets
#
# These are stored within this project under the "datasheets" folder.
# other_input is there for you to include other data sheets that you need to
# include in the processing beyond the default ones included in this processing
# script
#
summary_input <- "datasheets/summary_input.xls"
ctd_input <- "datasheets/ctd_input.xls"
neuston_input <- "datasheets/neuston_input.xls"
meter_input <- "datasheets/meter_input.xls"
bottle_input <- "datasheets/bottle_input.xls"
other_input <- ""




# Process non-datasheet data sources --------------------------------------

# Process elg (event file) data
process_elg(elg_folder, cruiseID = cruiseID)

# Process ctd data
process_ctd(ctd_folder, cruiseID = cruiseID)

# Process ADCP data
process_adcp(adcp_folder, cruiseID = cruiseID)


# Process datasheets ------------------------------------------------------

# Start with creating a robust summary sheet of all deployments. This summary
# sheet will then be used by subsequent functions to add metadata to their
# datasheets

# CTD datasheet
create_datasheet(ctd_input, data_type = "CTD",
                 cruiseID = cruiseID)

# Neuston datasheet
create_datasheet(neuston_input, data_type = "neuston",
                 cruiseID = cruiseID)

# Meter datasheet
create_datasheet(meter_input, data_type = "meter",
                 cruiseID = cruiseID)

# Bottle datasheet
create_datasheet(bottle_input, data_type = "bottle",
                 ros_input = ros_folder,
                 cruiseID = cruiseID)
