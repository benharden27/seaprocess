### Master processing file for the cruise
###


process_adcp("C://<WHERE THE DATA IS>")

process_elg("C://<WHERE THE DATA IS>")

process_ctd("ctd")

create_summary()

create_hourly()

create_neuston

create_meter

create_bottle
