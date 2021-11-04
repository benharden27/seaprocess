### Master processing file for the cruise
###

cruiseID <- NULL

process_adcp("adcp", cruiseID = cruiseID)

process_elg("elg",cruiseID = cruiseID)

process_ctd("ctd/Cnv",cruiseID = cruiseID)

# create_summary()
#
# create_hourly()
#
# create_neuston
#
# create_meter
#
# create_bottle
