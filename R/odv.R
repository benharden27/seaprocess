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
format_adcp_odv <- function(data,file,cruiseID = NULL) {

  # Try and determine the name of the cruise from the data input if not specified. If not, assign "unknown".
  if(is.null(cruiseID)) {
    cruiseID <- deparse(substitute(data))
    if(!stringr::str_detect(stringr::str_sub(cruiseID,1,1),"[C|S|c|s]")) {
      cruiseID = "unknown"
    }
  }

  # Convert from sea structure to get the adcp part
  if(is_sea_struct(data)){
    data <- data$adcp
  }

  nc <- dim(data$u)[2]
  spdir <- uv_to_wswd(data$u,data$v)

  odv_out <- tibble::tibble(
    Cruise = cruiseID,
    Station = rep_each(1:dim(data$u)[1], nc),
    Type = "C",
    `mon/day/yr` = rep_each(format(data$dttm,"%m/%d/%Y"), nc),
    `Lon [degrees_east]` = rep_each(data$lon, nc),
    `Lat [degrees_north]` = rep_each(data$lat, nc),
    `Bot. Depth [m]` = " ",
    `Depth [m]` = rep(data$d, dim(data$u)[1]),
    `Echo Amplitude [counts]` = as.vector(t(data$backscat)),
    `East Component [mm/s]` = as.vector(t(data$u)) * 1000,
    `North Component [mm/s]` = as.vector(t(data$v)) * 1000,
    `Magnitude [mm/s]` = as.vector(t(spdir[[1]])) * 1000,
    `Direction [deg]` = as.vector(t(spdir[[2]])),
    Ensemble = 0,
    `hh:mm` = rep_each(format(data$dttm,"%H:%M"), nc)
  )

  readr::write_tsv(odv_out,file)

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
format_hourly_odv <- function(data,file,cruiseID = NULL) {

  # Try and determine the name of the cruise from the data input if not specified. If not, assign "unknown".
  if(is.null(cruiseID)) {
    cruiseID <- deparse(substitute(data))
    if(!stringr::str_detect(stringr::str_sub(cruiseID,1,1),"[C|S|c|s]")) {
      cruiseID = "unknown"
    }
  }

  # Convert from sea structure to get the hourly part
  if(is_sea_struct(data)){
    data <- data$hourly
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
    `Wind-E/W Comp. [m/s]` = 0,
    `Wind-N/S Comp. [m/s]` = 0,
    `CDOM 1 min Avg` = data$cdom_1min,
    `Xmiss 1 min Avg` = data$xmiss_1min
  )

  readr::write_tsv(odv_out,file)

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
