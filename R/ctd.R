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
read_ctd <- function(cnv_file, pmin = 5, p = 1, ...) {


# Initial read ------------------------------------------------------------

  # Read CTD data from file
  ctd_safe <- purrr::possibly(oce::read.ctd, NULL)
  ctd <- ctd_safe(cnv_file,...)
  if(is.null(ctd)) {
    warning(paste0("No data found in ", cnv_file, ". Returning NULL."))
    return(ctd)
  }

  # Trim CTD data to remove upcast and surface values
  ctd_trim <- oce::ctdTrim(ctd, parameters = list(pmin = pmin), ...)
  if(length(ctd_trim@data$temperature)==0) {
    ctd_trim <- oce::ctdTrim(ctd, parameters = list(pmin = pmin), method = "upcast")
  }

  # Bin the CTD data into consistent pressure bins
  ctd <- oce::ctdDecimate(ctd_trim, p = 1, ...)

# Extract metadata --------------------------------------------------------

  # TODO make this more comprehensive

  X <- read_cnv_latlon(cnv_file)

  line <- stringr::str_which(X$r,"Depth")[1]
  depth <- as.numeric(strsplit(X$r[line],'h')[[1]][2])

  line <- stringr::str_which(X$r,"\\*{2}.*(T|t)ime")[1]
  time <- stringr::str_extract(X$r[line],"(?<== ).*")
  line <- stringr::str_which(X$r,"\\*{2}.*(D|d)ate")[1]
  date <- stringr::str_extract(X$r[line],"(?<== ).*")

  dttm <- lubridate::dmy_hm(paste(date,time),tz = "UTC")

  ctd@metadata$longitude <- X$lon
  ctd@metadata$latitude <- X$lat
  ctd@metadata$station <- as.numeric(strsplit(cnv_file,'-')[[1]][2]) # have to do this to make makeSection work
  ctd@metadata$waterDepth <- depth
  ctd@metadata$time <- dttm
  ctd@metadata$filename <- cnv_file

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
read_ctd_fold <- function(fold, check_vars = TRUE, ...) {

  files <- list.files(fold, pattern = "\\.cnv")

  ctd <- NULL

  for (i in 1:length(files)) {
    ctd_add <- read_ctd(file.path(fold,files[i]),...)
    if(!is.null(ctd_add)) {
      ctd <- append(ctd,ctd_add)
    }
  }

  # checks to see if there are missing vars recorded in some ctds and not others
  # fill with NA is so
  if(check_vars) {
    vars <- NULL
    for (i in 1:length(ctd)) {
      var <- names(ctd[[i]]@data)
      vars <- union(vars,var)
    }
    for (i in 1:length(ctd)) {
      var <- names(ctd[[i]]@data)
      varmiss <- setdiff(vars,var)
      if(length(varmiss)>1){
        l <- length(ctd[[i]]@data$pressure)
        for (j in varmiss) {
          ctd[[i]][[j]] <- rep(NA,l)
        }
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
  output <- dplyr::summarize_all(ros_df, list(mean))
  output <- dplyr::rename(output, bottle = bottlesFired)

  return(output)

}



