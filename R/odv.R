#' Format
#'
#' @param data
#' @param folder
#'
#' @return
#' @export
#'
#' @examples
format_odv <- function(data,folder,
                       fields = c("hourly","surfsamp","neuston","adcp","ctd","hydro"),
                       import = FALSE, ...) {

  # Return error is data is not a sea structure
  if(!is_sea_struct(data)) {
    stop("Data is not a sea package structure")
  }

  # create folder if it doesn't exist
  if (!file.exists(folder)){
    dir.create(folder)
  }

  # loop through fields
  for (field in fields) {
    subfolder <- file.path(folder,field)
    if (!file.exists(subfolder)){
      dir.create(subfolder)
    }

    file <- file.path(subfolder,paste0(field,".txt"))

    if(field == "hourly") {
      format_hourly_odv(data,file)
    }

    if(field == "surfsamp") {
      format_surfsamp_odv(data,file)
    }

    if(field == "neuston") {
      format_neuston_odv(data,file)
    }

    if(field == "adcp") {
      format_adcp_odv(data,file)
    }

    if(field == "ctd") {
      format_ctd_odv(data,file)
    }

    if(field == "hydro") {
      format_hydro_odv(data,file)
    }

    if(import) {
      import_odv(file)
    }
  }


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
write_odv <- function(odv_out, output_odv = NULL) {
  if(!is.null(output_odv)) {
    output <- safely(readr::write_tsv)(odv_out,output_odv)
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


#' Format ADCP Data for ODV inport
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
format_adcp_odv <- function(data, output_odv, cruiseID = NULL) {

  # Try and determine the name of the cruise from the data input if not specified. If not, assign "unknown".
  if(is.null(cruiseID)) {
    cruiseID <- deparse(substitute(data))
    if(!stringr::str_detect(stringr::str_sub(cruiseID,1,1),"[C|S|c|s]")) {
      cruiseID = "unknown"
    }
  }

  nc <- dim(data$u)[2]
  spdir <- uv_to_wswd(data$u,data$v)

  odv_out <- tibble::tibble(
    Cruise = cruiseID,
    Station = rep(1:dim(data$u)[1], each = nc),
    Type = "C",
    `mon/day/yr` = rep(format(data$dttm,"%m/%d/%Y"), each = nc),
    `Lon [degrees_east]` = rep(data$lon, each = nc),
    `Lat [degrees_north]` = rep(data$lat, each = nc),
    `Bot. Depth [m]` = " ",
    `Depth [m]` = rep(data$d, dim(data$u)[1]),
    `Echo Amplitude [counts]` = as.vector(t(data$backscat)),
    `East Component [mm/s]` = as.vector(t(data$u)) * 1000,
    `North Component [mm/s]` = as.vector(t(data$v)) * 1000,
    `Magnitude [mm/s]` = as.vector(t(spdir$ws)) * 1000,
    `Direction [deg]` = as.vector(t(spdir$wd)),
    Ensemble = 0,
    `hh:mm` = rep(format(data$dttm,"%H:%M"), each = nc)
  )

  write_odv(odv_out, output_odv)

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

  # Try and determine the name of the cruise from the data input if not specified. If not, assign "unknown".
  if(is.null(cruiseID)) {
    cruiseID <- deparse(substitute(data))
    if(!stringr::str_detect(stringr::str_sub(cruiseID,1,1),"[C|S|c|s]")) {
      cruiseID = "unknown"
    }
  }

  # Convert from sea structure to get the adcp part
  if(is_sea_struct(data)){
    data <- data$ctd
  }

  s <- make_section(data)
  s <- oce::sectionGrid(s)

  p <- unique(s[['pressure']])
  n <- length(data)

  odv_out <- tibble::tibble(
    Cruise = cruiseID,
    Station = rep_each(get_ctd_meta(data,"station"),length(p)),
    Type = "C",
    `mon/day/yr` = rep_each(format(s[["time"]],"%m/%d/%Y"),length(p)),
    `hh:mm` = rep_each(format(s[["time"]],"%H:%M"), length(p)),
    `Lon [degrees_east]` = s[["longitude"]],
    `Lat [degrees_north]` = s[["latitude"]],
    `Bot. Depth [m]` = rep_each(get_ctd_meta(data,"waterDepth"),length(p)),
    `Depth [m]` = s[["depth"]],
    `Temperature [~^oC]` = s[["temperature"]],
    `Salinity [psu]` = s[["salinity"]],
    `Density[Kg/m~^3]` = s[["sigmaTheta"]],
    `Chl a Fluorescence [V]` = s[["fluorescence"]],
    `Oxygen,SBE43[~$m~#mol/kg]` = s[["oxygen"]],
    `CDOM Fluorescence [mg/m~^3]` = " ",
    `PAR Irradience [~$m~#E/m~^2/s]` = s[["par"]],
    `Transmittance [V]` = " ",
    `Oxygen [mL/L]` = s[["oxygen2"]]
  )

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
format_elg_odv <- function(data, output_odv = NULL ,cruiseID = NULL) {

  # Try and determine the name of the cruise from the data input if not specified. If not, assign "unknown".
  if(is.null(cruiseID)) {
    cruiseID <- deparse(substitute(data))
    if(!stringr::str_detect(stringr::str_sub(cruiseID,1,1),"[C|S|c|s]")) {
      cruiseID = "unknown"
    }
  }

  odv_out <- tibble::tibble(
    Cruise = cruiseID,
    Station = 1:dim(data)[1],
    Type = "B",
    `mon/day/yr` = format(data$dttm,"%m/%d/%Y"),
    `Lon [degrees_east]` = data$lon,
    `Lat [degrees_north]` = data$lat,
    `Bot. Depth [m]` = data$depth_bot,
    `Depth [m]` = 0,
    `Temperature [~^oC]` = data$temp,
    `Salinity [PSU]` = data$sal,
    `Fluorescence` = data$fluor,
    `hh:mm` = format(data$dttm,"%H:%M"),
    `Wind Speed [knots]` = data$wind_sp,
    `Wind Direction [deg]` = data$wind_dir,
    `Wind-E/W Comp. [m/s]` = wswd_to_uv(data$wind_sp,data$wind_dir)$u,
    `Wind-N/S Comp. [m/s]` = wswd_to_uv(data$wind_sp,data$wind_dir)$v,
    `CDOM` = data$cdom_1min,
    `Xmiss` = data$xmiss_1min
  )

  write_odv(odv_out, output_odv)

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

  # Try and determine the name of the cruise from the data input if not specified. If not, assign "unknown".
  if(is.null(cruiseID)) {
    cruiseID <- deparse(substitute(data))
    if(!stringr::str_detect(stringr::str_sub(cruiseID,1,1),"[C|S|c|s]")) {
      cruiseID = "unknown"
    }
  }

  # Convert from sea structure to get the hourly part
  if(is_sea_struct(data)){
    data <- data$neuston
  }

  odv_out <- tibble::tibble(
    Cruise = cruiseID,
    Station = 1:dim(data)[1],
    Type = "B",
    `mon/day/yr` = format(data$dttm_in,"%m/%d/%Y"),
    `hh:mm` = format(data$dttm_in,"%H:%M"),
    `Lon [degrees_east]` = data$lon,
    `Lat [degrees_north]` = data$lat,
    `Depth [m]` = 0
  )

  ii <- which(names(data) == "temp")

  odv_out <- dplyr::bind_cols(odv_out,data[9:dim(data)[2]])

  readr::write_tsv(odv_out,file)

  return(odv_out)

}

#' Format Surface Data for ODV inport
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
format_surfsamp_odv <- function(data,file,cruiseID = NULL) {

  # Try and determine the name of the cruise from the data input if not specified. If not, assign "unknown".
  if(is.null(cruiseID)) {
    cruiseID <- deparse(substitute(data))
    if(!stringr::str_detect(stringr::str_sub(cruiseID,1,1),"[C|S|c|s]")) {
      cruiseID = "unknown"
    }
  }

  # Convert from sea structure to get the hourly part
  if(is_sea_struct(data)){
    data <- data$surfsamp
  }

  odv_out <- tibble::tibble(
    Cruise = cruiseID,
    Station = 1:dim(data)[1],
    Type = "B",
    `mon/day/yr` = format(data$dttm_local,"%m/%d/%Y"),
    `hh:mm` = format(data$dttm_local,"%H:%M"),
    `Lon [degrees_east]` = data$lon,
    `Lat [degrees_north]` = data$lat,
    `Depth [m]` = 0
  )

  ii <- which(names(data) == "temp")

  odv_out <- dplyr::bind_cols(odv_out,data[9:dim(data)[2]])

  readr::write_tsv(odv_out,file)

  return(odv_out)

}


#' Format hydrowork file dor ODV
#'
#' @param data
#' @param file
#' @param cruiseID
#'
#' @return
#' @export
#'
#' @examples
format_hydro_odv <- function(data,file = NULL,cruiseID = NULL) {

  # Try and determine the name of the cruise from the data input if not specified. If not, assign "unknown".
  if(is.null(cruiseID)) {
    cruiseID <- deparse(substitute(data))
    if(!stringr::str_detect(stringr::str_sub(cruiseID,1,1),"[C|S|c|s]")) {
      cruiseID = "unknown"
    }
  }

  # Convert from sea structure to get the hourly part
  if(is_sea_struct(data)){
    data <- data$hydro
  }

  odv_out <- tibble::tibble(
    Cruise = cruiseID,
    Station = data$station,
    Type = "B",
    `mon/day/yr` = format(data$dttm,"%m/%d/%Y"),
    `hh:mm` = format(data$dttm,"%H:%M"),
    `Lon [degrees_east]` = data$lon,
    `Lat [degrees_north]` = data$lat,
    `Bot. Depth [m]` = "",
    `Depth [m]` = data$z,
    `Temperature [~^oC]` = data$temp,
    `Salinity [PSU]` = data$sal,
    `Density[kg/m~^3]` = data$density,
    `Chlorophyll a A[~$m~#g/l]` = data$chla,
    `Phosphate [~$m~#M]` = data$po4,
    `Nitrate [~$m~#M]` = data$no3,
    `pH` = data$pH,
    `Alkalinity` = data$alk,
    `Microplastics` = data$m_plastics
  )

  if(!is.null(file)){
    readr::write_tsv(odv_out,file)
  } else {
    return(odv_out)
  }

}
