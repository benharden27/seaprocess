# Functions to read in and process data from CNV files


#' Read and process CTD data
#'
#' Wrapper function for oce functions read.ctd, ctdTrim, and ctdDecimate.
#' Extracts and adds metadata from header that is unique to SEA headers.
#'
#' @param cnv_file file path to the CNV file to be read
#' @param pmin minimum cut off pressure for ctdTrim
#' @param p pressure bins for ctdDecimate
#' @param ... additional arguments passed to called functions
#'
#' @return
#' @export
#'
#' @examples
read_ctd <- function(cnv_file, pmin = 5, p = 1, to_tibble = TRUE,
                     cruiseID = NULL, depth_vec = NULL, depth_step = 1, ...) {


# Initial read ------------------------------------------------------------

  # Read CTD data from file
  ctd_safe <- purrr::possibly(oce::read.ctd, NULL)
  ctd <- ctd_safe(cnv_file,...)
  if(is.null(ctd)) {
    warning(paste0("No data found in ", cnv_file, " upon opening. 
                    Check cnv file for data. Returning NULL."))
    return(ctd)
  }

  # Trim CTD data to remove upcast and surface values
  ctd_trim <- oce::ctdTrim(ctd, parameters = list(pmin = pmin), ...)
  if(length(ctd_trim@data$temperature)==0) {
    ctd_trim <- oce::ctdTrim(ctd, parameters = list(pmin = pmin), method = "upcast")
  }

  # catch ctd dataset being empty after trimming
  if(length(ctd_trim@data$temperature)==0) {
    warning(strwrap(paste0("No data found in ",cnv_file,"
                  after ctdTrim, which removes surface values. 
                  Check cnv file for valid data across depths in 
                  up and/or downcasts. Returning NULL."), 
                  width = 60, prefix = " "))
    return(NULL)
  }

  # Bin the CTD data into consistent pressure bins
  ctd <- oce::ctdDecimate(ctd_trim, p = 1, ...)

  ## TODO put these on depth bins eventially

# Extract metadata --------------------------------------------------------

  # TODO make this more comprehensive

  X <- read_cnv_latlon(cnv_file)

  line <- stringr::str_which(X$r,"Depth")[1]
  depth <- as.numeric(strsplit(X$r[line],'h')[[1]][2])

  line <- stringr::str_which(X$r,"\\*{2}.*(T|t)ime")[1]
  time <- stringr::str_extract(X$r[line],"(?<== ).*")
  # attempt alternative line formatting for extraction if time is NA 
  if (is.na(time)){
    time <- stringr::str_extract(X$r[line],"(?<=Time ).*")
  }
  line <- stringr::str_which(X$r,"\\*{2}.*(D|d)ate")[1]
  date <- stringr::str_extract(X$r[line],"(?<== ).*")
  # attempt alternative line formatting for extraction if time is NA 
  if (is.na(date)){
    date <- stringr::str_extract(X$r[line],"(?<=Date ).*")
  }
  dttm <- lubridate::dmy_hm(paste(date,time),tz = "UTC")

  # catch issue with datetime in aggregate
  if (is.na(dttm)){
    warning(paste("Could not read date and / or time from",
            cnv_file, ". Expected line containing time to be 
            formatted as: ** NMEA (UTC) Time = 18:35 or
            ** Time 01:41 and date formatted as:
            ** Date = 14 June 2019 or ** Date 19 Jan 23
            Edit CTD/HC metadata setup fields in seabird software
            or manually edit seaprocess output time field. 
            Note in EOC.
            "))
  }

  ctd@metadata$longitude <- X$lon
  ctd@metadata$latitude <- X$lat
  ctd@metadata$station <- as.numeric(strsplit(cnv_file,'-')[[1]][2]) # have to do this to make makeSection work.
  ctd@metadata$waterDepth <- depth
  ctd@metadata$time <- dttm
  ctd@metadata$filename <- cnv_file

  if(to_tibble) {
    ctd <- ctd_to_tibble(ctd, cruiseID = cruiseID, depth_vec = depth_vec, depth_step = depth_step)
  }

  return(ctd)

}


#' Read all CNV files in a folder
#'
#' @param fold folder to read from
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
read_ctd_fold <- function(fold, check_vars = TRUE,
                          cruiseID = NULL,
                          depth_vec = NULL, ...) {

  files <- list.files(fold, pattern = "\\.cnv")

  # try and set cruiseID from filenames if not set in function call
  if(is.null(cruiseID)) {
    for (i in 1:length(files)) {
      cruiseID <- stringr::str_extract(files[i],"[S|C|c|s]{1}[0-9]{3}[a-z|A-Z]?")
      if(!is.na(cruiseID)) {
        cruiseID <- stringr::str_to_upper(cruiseID)
        break
      }
    }
  }

  ctd <- NULL

  for (i in 1:length(files)) {

    # read in the data from the file
    ctd_add <- read_ctd(file.path(fold, files[i]), cruiseID = cruiseID, ...)

    # check to see if the read was successful
    if(is.null(ctd_add)) {
      warning(paste("CTD in file", files[i], "was not added to complete data"))
      next
    }

    # check if the conversion to tibble should happen (default TRUE)
    if(tibble::is_tibble(ctd_add)) {

      # add a column for filename
      ctd_add <- dplyr::mutate(ctd_add, file = files[i])

      # check to see if this is the first file to be added
      # otherwise bind to previous data
      if(is.null(ctd)) {
        ctd <- ctd_add
      } else {
        ctd <- dplyr::bind_rows(ctd, ctd_add)
      }

    # option to keep the ctds as a list
    # (for troubleshooting - this should not be for default data processing)
    } else {
      if(!is.null(ctd_add)) {
        ctd <- append(ctd,ctd_add)
      }
    }


  }

  return(ctd)

}

#' Read lat and lon from CNV header
#'
#' @param cnv_file cnv file to read
#'
#' @return
#' @export
#'
#' @examples
read_cnv_latlon <- function(cnv_file) {

  # TODO: Need to work on this to improve efficiency and neatness

  r <- readr::read_lines(cnv_file,n_max = 100)

  # set possible patterns to search for
  patt <- "([0-9]+[^0-9]+[0-9]+[^0-9]+[0-9]*)"
  patt2 <- "([0-9]+[^0-9]+[0-9])"
  patt3 <- "([0-9]+)"

  # LATITUDE
  # switch depending on what format the lon and lat are stored as
  if(length(grep('^.*Lat.*Lon.*$',r)) > 0) {
    case <- 1
    line <- grep('^.*Lat.*Lon.*$',r)
  } else if (length(grep('Lat|Lat',r,ignore.case=T))==0) {
    case <- 2
    line <- 1
  } else {
    case <- 3
    line <- grep("Lat",r,ignore.case=T)[1] # finds the word "Latitude" in r
  }

  # search for the patterns in order
  a <- regexpr(patt,r[line])
  if (a==-1) {
    a <- regexpr(patt2,r[line])
    if(a==-1) {
      a <- regexpr(patt3,r[line])
    }
  }

  # assign the latitude based on the search findings
  lat <- substr(r[line],a,a+attr(a,"match.length")-1)
  lat <- strsplit(lat,"[^0-9\\.]")[[1]]
  lat <- lat[lat!=""] # removes blank sections

  # Define Hemisphere
  # hemi <- substr(r[line],regexpr("[NS]",r[line]),regexpr("[NS]",r[line]))
  hemi <- substr(r[line],regexpr("[NS].{0,5}$",r[line]),regexpr("[NS].{0,5}$",r[line]))
  if(hemi=='S'){
    fac <- -1
  } else {
    fac <- 1
  }

  # depending on the end format of "lat" do various different things to parse the output
  if(length(lat)==1) {
    if(length(strsplit(lat,"\\.")[[1]])<3) {
      lat <- fac*as.numeric(substr(lat,1,2)) + fac * as.numeric(substr(lat,3,100))/60
    } else {
      lat <- strsplit(lat,"\\.")[[1]]
      lat <- fac * as.numeric(lat[1]) + fac * (as.numeric(lat[2])+as.numeric(lat[3])/10)/60
    }
  } else {
    lat <- fac * as.numeric(lat[1]) + fac * as.numeric(lat[2])/60;
  }


  # LONGITUDE

  # again, switch by the format of the line
  if (case==1) {
    rest<- substr(r[line],a+attr(a,"match.length"),100)
  } else if (case==2) {
    rest <- 'xxxxx'
  } else {
    rest <- r[grep("Lon",r,ignore.case = T)[1]]
  }

  # search for the patterns
  a <- regexpr(patt,rest)
  if (a==-1) {
    a <- regexpr(patt2,rest)
    if(a==-1) {
      a <- regexpr(patt3,rest)
    }
  }

  # assign longitude based on patterns
  lon <- substr(rest,a,a+attr(a,"match.length")-1)
  lon <- strsplit(lon,"[^0-9\\.]")[[1]]
  lon <- lon[lon!=""] # removes blank sections

  # Define Hemisphere
  # hemi <- substr(rest,regexpr("[WE]",rest),regexpr("[WE]",rest));
  hemi <- substr(rest,regexpr("[WE].{0,5}$",rest),regexpr("[WE].{0,5}$",rest));
  if(hemi=='E'){
    fac <- 1
  } else {
    fac <- -1
  }


  # do various formating based on type of "lon" output
  if(length(lon)==1) {
    if(length(strsplit(lon,"\\.")[[1]])==2) {
      lon <- fac * as.numeric(substr(lon,1,2)) + fac * as.numeric(substr(lon,3,100))/60
    } else if (nchar(lon)>3) {
      lon <- strsplit(lon,"\\.")[[1]]
      lon <- fac * as.numeric(lon[1]) +fac * (as.numeric(lon[2])+as.numeric(lon[3])/10)/60
    } else {
      lon <- fac*as.numeric(lon)
    }
  } else {
    lon <- fac* as.numeric(lon[1]) + fac * as.numeric(lon[2])/60;
  }

  # show the lines of output for when there is no lon or no lat
  if(is.na(lon)|is.na(lat)) {
    show(r)
    # a<-readline('Press enter key to continue...')
  }

  X <- NULL
  X$lon <- lon
  X$lat <- lat
  X$r <- r

  return(X)



}


#' Read and process data from a ROS seabird file
#'
#' @param ros_file
#'
#' @return
#' @export
#'
#' @examples
read_ros <- function(ros_file) {

  # read ros file using oce package
  ros <- oce::read.ctd(ros_file)

  # convert data to a tibble
  ros_df <- tibble::as_tibble(ros@data)

  # group by bottles fired and find the means
  ros_df <- dplyr::group_by(ros_df, bottlesFired)
  ros_df <- dplyr::summarise(ros_df,dplyr::across(tidyselect::vars_select_helpers$everything(),  ~mean(.x, na.rm = TRUE)))
  ros_df <- dplyr::rename(ros_df, bottle = bottlesFired)

  ros_df <- dplyr::mutate(ros_df,
                          theta = oce::swTheta(salinity = salinity,
                                               temperature = temperature,
                                               pressure = pressure),
                          sigma = oce::swSigma0(salinity = salinity,
                                                temperature = temperature,
                                                pressure = pressure))

  return(ros_df)

}

#' Convert a ctd profile from OCE class ctd to tibble
#'
#' @param ctd_data
#' @param cruiseID
#'
#' @return
#' @export
#'
#' @examples
ctd_to_tibble <- function(ctd_data, cruiseID = NULL, depth_vec = NULL, depth_step = 1) {

  # get all field names in the data set
  all_fields <- names(ctd_data@metadata$dataNamesOriginal)

  # find the indexes of those which have the words oxygen in them
  ii <- stringr::str_which(all_fields,"oxygen")

  # Set up initially empty oxygen vectors
  oxygen_mL <- NA
  oxygen_mM <- NA

  # look through oxygen instances if any occur
  if(length(ii)>0) {
    for (iii in ii) {
      original <- ctd_data@metadata$dataNamesOriginal[[iii]]
      if(stringr::str_detect(original,"sbeox0Mm")) {
        oxygen_mM <- ctd_data@data[[iii]]
      }
      if(stringr::str_detect(original,"sbeox0ML")) {
        oxygen_mL <- ctd_data@data[[iii]]
      }
    }
  }

  # reset the values of oxygen for clarity
  ctd_data@data$oxygen <- oxygen_mM
  ctd_data@data$oxygen2 <- oxygen_mL

  # check for par sensor
  if(is.null(ctd_data@data$par)) {
    par = NA
  } else {
    par <- ctd_data@data$par
  }

  # check if there is theta specified and provide if not
  if(is.null(ctd_data@data$theta)) {
    ctd_data@data$theta <- oce::swTheta(ctd_data@data$salinity,
                                        ctd_data@data$temperature,
                                        ctd_data@data$pressure)
  }

  # Check for fluoroesence fields and make sure we care selecting the right onw
  ii <- stringr::str_which(all_fields,"fluor")
  if(length(ii) > 0) {
    for (iii in ii) {
      original <- ctd_data@metadata$dataNamesOriginal[[iii]]
      if(stringr::str_detect(original,"flSP")) {
        ctd_data@data$fluorescence <- ctd_data@data[[iii]]
      } else {
        ctd_data@data[[iii]] <- NA
      }
    }
  }
  # final check for is fluoroesence is still NULL
  if(is.null(ctd_data@data$fluorescence)) {
    ctd_data@data$fluorescence <- NA
  }

  # set the station number
  station <- stringr::str_pad(ctd_data@metadata$station,3,pad = "0")
  if(!is.null(cruiseID)) {
    station <- paste0(cruiseID,"-",station)
  }
  
  # finally create the output data frame
  ctd_tibble <- tibble::tibble(dep = ctd_data@data$depth,
                               pres = ctd_data@data$pressure,
                               temp = ctd_data@data$temperature,
                               theta = ctd_data@data$theta,
                               sigtheta = ctd_data@data$sigmaTheta,
                               sal = ctd_data@data$salinity,
                               fluor = ctd_data@data$fluorescence,
                               par = par,
                               oxygen = ctd_data@data$oxygen,
                               oxygen2 = ctd_data@data$oxygen2,
                               lon = ctd_data@metadata$longitude,
                               lat = ctd_data@metadata$latitude,
                               station = station,
                               dttm = ctd_data@metadata$time,
                               cruise = ifelse(is.null(cruiseID),NA,cruiseID))

  # finally regrid to depth bins
  ctd_tibble <- interpolate_depth(ctd_tibble, depth_vec = depth_vec, depth_step = depth_step)
  
  return(ctd_tibble)
}


#' Interpolate a ctd tibble on a regular pressure grid to a regular dept grid
#'
#' @param ctd_tibble
#' @param depth_vec
#'
#' @return
#' @export
#'
#' @examples
interpolate_depth <- function(ctd_tibble, depth_vec = NULL, depth_step = 1) {

  if(dim(ctd_tibble)[1] > 0){
    if(is.null(depth_vec)) {
      depth_vec <- seq(ceiling(min(ctd_tibble$dep,na.rm=T)/depth_step)*depth_step,
                      floor(max(ctd_tibble$dep,na.rm=T)/depth_step)*depth_step,
                      by = depth_step)
    }

    ctd_tibble <- tidyr::pivot_longer(ctd_tibble,!c(lon,lat,dttm,cruise,dep,station))
    ctd_tibble <- dplyr::group_by(ctd_tibble, cruise, station, lon, lat, dttm, name)
    ctd_tibble <- dplyr::filter(ctd_tibble, any(!is.na(value)))
    ctd_tibble <- dplyr::summarise(ctd_tibble, h = list(depth_vec), a = list(approx(x = dep, y = value, xout = depth_vec)$y))
    ctd_tibble <- tidyr::unnest(ctd_tibble, cols = c(h,a))
    ctd_tibble <- tidyr::pivot_wider(ctd_tibble, names_from = name, values_from = a)
    ctd_tibble <- dplyr::rename(ctd_tibble, dep = h)
  }

  return(ctd_tibble)
}

