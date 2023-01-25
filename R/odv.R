#' Wrapper function to format any odv file
#``
#' @param data
#' @param odv_output
#' @param data_type
#' @param cruiseID
#'
#' @return
#' @export
#'
#' @examples
format_odv <- function(data, odv_output, data_type = "CTD", cruiseID = NULL) {

  if("HC" %in% data_type | "B" %in% data_type ) {
    format_function <- format_bottle_odv
  } else if ("ctd" %in% data_type) {
    format_function <- format_ctd_odv
  } else if ("NT" %in% data_type) {
    format_function <- format_neuston_odv
  } else if ("elg" %in% data_type) {
    format_function <- format_elg_odv
  } else if ("adcp" %in% data_type) {
    format_function <- format_adcp_odv
  } else {
    warning("No valid data tyoe specified to output odv. No output created.")
    return()
  }

  odv_out <- format_function(data, odv_output, cruiseID = cruiseID)

  return(odv_out)

}

#' Format ADCP Data for ODV inport
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
format_adcp_odv <- function(data, odv_output, cruiseID = NULL) {

  odv_out <- initialize_odv_tibble(data, cruiseID, type = "C")

  odv_out <- tibble::add_column(odv_out,
                                `Depth [m]` = data$depth,
                                `Echo Amplitude [counts]` = data$backscat,
                                `East Component [mm/s]` = data$u * 1000,
                                `North Component [mm/s]` = data$v * 1000,
                                `Magnitude [mm/s]` = data$sp * 1000,
                                `Direction [deg]` = data$dir,
                                Ensemble = 0)

  write_odv(odv_out, odv_output)

  return(odv_out)

}


#' Format CTD Data for ODV inport
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
format_ctd_odv <- function(data,file,cruiseID = NULL) {

  odv_out <- initialize_odv_tibble(data, cruiseID, type = "C")

  odv_out <- tibble::add_column(odv_out,
                                `Depth [m]` = data$dep,
                                `Temperature [~^oC]` = data$temp,
                                `Salinity [psu]` = data$sal,
                                `Density[Kg/m~^3]` = data$sigtheta,
                                `Chl a Fluorescence [V]` = data$fluor,
                                `Oxygen,SBE43[~$m~#mol/kg]` = data$oxygen,
                                `Oxygen [mL/L]` = data$oxygen2,
                                `CDOM Fluorescence [mg/m~^3]` = " ",
                                `PAR Irradience [~$m~#E/m~^2/s]` = data$par,
                                `Transmittance [V]` = " ")

  readr::write_tsv(odv_out,file)

  return(odv_out)

}


#' Format Hourly Data for ODV inport
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
format_elg_odv <- function(data, odv_output = NULL ,cruiseID = NULL) {

  odv_out <- initialize_odv_tibble(data, cruiseID, type = "C")

  # create lookup table for field names as known to elg_read
  odv_lookup <- tibble::tribble(~varname, ~odvname,
                          "sys_date", "System Date",
                          "sys_time", "System Time",
                          "nav_time", "Nav Time",
                          "nav_lon", "Nav Longitude",
                          "nav_lat", "Nav Latitude",
                          "nav_sog", "Nav Speed Over Ground [knots]",
                          "nav_cog", "Nav Course Over Ground [degrees true]",
                          "nav_quality", "Nav GPS Quality",
                          "lab_time", "Lab Time",
                          "lab_lon", "Lab Longitude",
                          "lab_lat", "Lab Latitude",
                          "lab_sog", "Lab Speed Over Ground [knots]",
                          "lab_cog", "Lab Course Over Ground [knots]",
                          "lab_quality", "Lab GPS Quality",
                          "temp", "Temperature [deg C]",
                          "temp_1min", "Temperature, 1 min avg [deg C]",
                          "temp_60min", "Temperature, 60 min avg [dec C]",
                          "sal", "Salinity [psu]",
                          "sal_1min", "Salinity, 1 min avg [psu]",
                          "sal_60min", "Salinity, 60 min avg [psu]",
                          "sound_vel", "Sound Velocity [m/s]",
                          "fluor", "Rel. Fluorescence",
                          "fluor_1min", "Rel. Fluorescence, 1 min avg",
                          "fluor_60min", "Rel. Fluorescence, 60 min avg",
                          "cdom", "CDOM [counts]",
                          "cdom_1min", "CDOM, 1 min avg [counts]",
                          "cdom_60min", "CDOM, 60 min avg [counts]",
                          "xmiss", "Transmissometer [counts]",
                          "xmiss_1min", "Transmissometer, 1 min avg [counts]",
                          "xmiss_60min", "Transmissometer, 60 min avg [counts]",
                          "wind_sp", "True Wind Speed [knots]",
                          "wind_dir", "True Wind Direction [degrees]",
                          "wind_sp_rel", "Relative Wind Speed [knots]",
                          "wind_dir_rel", "Relative Wind Direction [degrees]",
                          "heading", "Ship's Heading [degrees true]",
                          "pitch", "Pitch [degrees]",
                          "roll", "Roll [degrees]",
                          "depth", "CHIRP depth [m]",
                          "wire_payout", "Wire Payout",
                          "wire_tension", "Wire Tension",
                          "wire_speed", "Wire Speed",
                          )
  # get names from elg, skip dttm, lat, lon; n, filename_first, filename_last if binned
  elg_names <- names(data)[! names(data) %in% c("dttm","lon","lat","n","filename_first","filename_last")]

  # subset elg to relevant columns by name
  odv_names <- dplyr::pull(odv_lookup[odv_lookup$varname %in% elg_names,"odvname"],odvname)

  # subset and rename columns by ODV name
  elg_sub <- data[,elg_names]
  colnames(elg_sub) <- odv_names

  # add Depth [m] in first spot
  odv_out <- tibble::add_column(odv_out, `Depth [m]` = 0)

  # bind data tibble with metadata
  odv_out <- dplyr::bind_cols(odv_out, elg_sub)

  # if wind speed and win dir, add E/W / N/S comp
  if all(c("lat", "lon") %in% names(elg_names)){
    odv_out <- tibble::add_column(odv_out,
                                `Wind-E/W Comp. [m/s]` = wswd_to_uv(data$wind_sp,data$wind_dir)$u,
                                `Wind-N/S Comp. [m/s]` = wswd_to_uv(data$wind_sp,data$wind_dir)$v)
  }

  # odv_out <- tibble::add_column(odv_out,
  #                               `Depth [m]` = 0,
  #                               `Temperature [~^oC]` = data$temp,
  #                               `Salinity [PSU]` = data$sal,
  #                               `Fluorescence` = data$fluor,
  #                               `Wind Speed [knots]` = data$wind_sp,
  #                               `Wind Direction [deg]` = data$wind_dir,
  #                               `Wind-E/W Comp. [m/s]` = wswd_to_uv(data$wind_sp,data$wind_dir)$u,
  #                               `Wind-N/S Comp. [m/s]` = wswd_to_uv(data$wind_sp,data$wind_dir)$v,
  #                               `CDOM` = data$cdom,
  #                               `Xmiss` = data$xmiss)

  write_odv(odv_out, odv_output)

  return(odv_out)

}

#' Format Neuston Data for ODV inport
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
format_neuston_odv <- function(data,file,cruiseID = NULL) {

  odv_out <- initialize_odv_tibble(data, cruiseID, type = "B")

  odv_out <- tibble::add_column(odv_out,
                                `Depth [m]` = 0,
                                `Temperature [~^oC]` = data$temp,
                                `Salinity [PSU]` = data$sal,
                                `Fluorescence` = data$fluor)

  # Add the rest of the data
  # TODO create look-up sheet to find real names
  ii <- which(colnames(data) == "station_distance")
  odv_out <- dplyr::bind_cols(odv_out, data[ii:ncol(data)])

  readr::write_tsv(odv_out,file)

  return(odv_out)

}


#' Format bottle datasheet for ODV
#'
#' @param data
#' @param file
#' @param cruiseID
#'
#' @return
#' @export
#'
#' @examples
format_bottle_odv <- function(data, file = NULL, cruiseID = NULL) {

  odv_out <- initialize_odv_tibble(data, cruiseID, type = "B")

  odv_out <- tibble::add_column(odv_out,
                                `Depth [m]` = data$depth)

  # Add the rest of the data skipping depth column
  # TODO create look-up sheet to find real names
  ii <- which(colnames(data) == "bottle")
  ii2 <- which(colnames(data) == "depth")
  odv_out <- dplyr::bind_cols(odv_out, data[c(ii:(ii2-1), (ii2+1):ncol(data))])

  if(!is.null(file)){
    readr::write_tsv(odv_out,file)
  } else {
    return(odv_out)
  }

}

initialize_odv_tibble <- function(data, cruiseID = NULL, type = "C") {

  # Try and determine the name of the cruise from the data input if not specified. If not, assign "unknown".
  if(is.null(cruiseID)) {
    cruiseID = "unknown"
  }

  # change the value of station depending on whether it has a column in data
  if("station" %in% colnames(data)) {
    station <- data$station
  } else {
    station <- 1:nrow(data)
  }

  odv_out <- tibble::tibble(Cruise = cruiseID,
                            Station = data$station,
                            Type = type,
                            `mon/day/yr` = format(data$dttm,"%m/%d/%Y"),
                            `hh:mm` = format(data$dttm,"%H:%M"),
                            `Lon [degrees_east]` = data$lon,
                            `Lat [degrees_north]` = data$lat,
                            `Bot. Depth [m]` = " ")

  return(odv_out)

}

#' Write formatted data to odv txt file
#'
#' @param data
#' @param odv_output
#'
#' @return
#' @export
#'
#' @examples
write_odv <- function(odv_out, odv_output = NULL) {
  if(!is.null(odv_output)) {
    output <- purrr::safely(readr::write_tsv)(odv_out,odv_output)
    if(!is.null(output$error)) {
      warning("Couldn't export the data to a odv file. Most likely specified directory doesn't exist")
    }
  } else {
    warning("No file written - No output odv file specified")
  }
}

#' Import a txt file to ODV
#'
#' @param odv_txt path to the odv txt file you want to import
#'
#' @return
#' @export
#'
#' @examples
import_odv <- function(odv_txt) {
  odv_txt <- path.expand(odv_txt)
  cmd_file <- stringr::str_replace(odv_txt,".txt",".cmd")
  cmd_line <- paste("open_data_file",odv_txt)
  readr::write_lines(cmd_line,cmd_file)
  system(paste("/Applications/Ocean\\ Data\\ View\\ \\(64bit\\).app/Contents/MacOS/odv4 -x",cmd_file,"-q"))
}

