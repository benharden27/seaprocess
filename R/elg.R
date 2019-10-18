# Series of functions that work to read in an event data file from SEA's archive

#' Read in SEA data an ELG event file and return a well formatted output
#'
#' SEA event files contain output from a number of instruments
#' including GPS, flow-through, chirp, etc.
#'
#' @param filein .elg file to be read in
#' @param forceGPS option to force the longitude, latitude and time to come from one
#'  or other of the GPS feeds
#' @param preCheck logical to do an initial check of lines to remove any standard issues
#' @param skip number of lines to skip below header
#' @keywords
#' @export
#' @examples
#' read_elg()
#'
read_elg <- function(filein,forceGPS=NULL,preCheck = T,skip=0, zd = NULL) {

  # TODO: Optimize code using pmap from purrr Package

  if(skip>0) {
    col_names <- names(readr::read_csv(filein, n_max = 0))
  } else {
    col_names=T
  }

  # Toggle a preCheck of the elg file for clearly bad lines and commas at end
  if(preCheck) {

    # check for bad lines by checking for number of commas
    liner <- readr::read_lines(filein)
    numcom <- stringr::str_count(liner,"\\,")
    liner <- liner[numcom==numcom[1]]

    # clean and process end of line
    liner <- stringr::str_replace(liner,"\\,$","") # remove the trailing comma on many ELG files
    liner <- stringr::str_replace(liner,"$","\\\n")   # add new line to end of each line
    liner <- stringr::str_c(liner,collapse="")           # collapse vector into single line for read_csv

    # Read in lines using readr package (quicker than base read.csv)
    df <- readr::read_csv(liner,col_types = readr::cols(.default=readr::col_character()),skip=skip,col_names = col_names)

  } else {

    # If no precheck then just read in the file as is
    df <- readr::read_csv(filein,col_types = readr::cols(.default=readr::col_character()),skip=skip,col_names = col_names)

  }

  # Reasign names that have dashes in them to be referenced more easily
  names(df) <- stringr::str_replace_all(names(df),"-",".")

  # Construct the
  args <- tibble::tribble(~name,~regex,~parse_fun,
                          "sys_date","date",lubridate::mdy,
                          "sys_time","^time",readr::parse_time,
                          "nav_time","gps.*nav.*time",readr::parse_character,
                          "nav_lon","gps.*nav.*lon",sea::parse_lon,
                          "nav_lat","gps.*nav.*lat",sea::parse_lat,
                          "nav_sog","gps.*nav.*sog",readr::parse_double,
                          "nav_cog","gps.*nav.*cog",readr::parse_double,
                          "nav_quality","gps.*nav.*quality",readr::parse_integer,
                          "lab_time","gps.*lab.*time",readr::parse_character,
                          "lab_lon","gps.*lab.*lon",sea::parse_lon,
                          "lab_lat","gps.*lab.*lat",sea::parse_lat,
                          "lab_sog","gps.*lab.*sog",readr::parse_double,
                          "lab_cog","gps.*lab.*cog",readr::parse_double,
                          "lab_quality","gps.*lab.*quality",readr::parse_integer,
                          "temp","tsal.*temp",readr::parse_double,
                          "sal","tsal.*sal",readr::parse_double,
                          "fluor","^fluo.*invivo",readr::parse_double,
                          "fluor_1min","fluo.*chl.*1.*min",readr::parse_double,
                          "fluor_60min","^fluo.*chl.*60.*min",readr::parse_double,
                          "cdom","cdom.*raw",readr::parse_double,
                          "cdom_1min","cdom.*1.*min",readr::parse_double,
                          "cdom_60min","cdom.*60.*min",readr::parse_double,
                          "xmiss",c("trans.*raw","xmiss.*raw","xmiss.*[^m]"),readr::parse_double,
                          "xmiss_1min",c("trans.*1\\.*min","xmiss.*1\\.*min"),readr::parse_double,
                          "xmiss_60min",c("trans.*60.*min","xmiss.*60.*min"),readr::parse_double,
                          "wind_sp","true.*wind.*sp",readr::parse_double,
                          "wind_dir","true.*wind.*dir",readr::parse_double,
                          "wind_sp_rel","^wind.*sp",readr::parse_double,
                          "wind_dir_rel","^wind.*dir",readr::parse_double,
                          "heading",c("hdg","heading"),readr::parse_double,
                          "pitch","pitch",readr::parse_double,
                          "roll","roll",readr::parse_double,
                          "depth",c("depth","dbt"),readr::parse_double,
                          "wire_payout","lci90.*payout",readr::parse_double,
                          "wire_tension","lci90.*tension",readr::parse_double,
                          "wire_speed","lci90.*spd",readr::parse_double
                          )

  args_in <- tibble::as_tibble(list(df=list(df),regex=args$regex,parse_fun=args$parse_fun))
  namelist <- purrr::as_vector(dplyr::select(args,name))

  # Work out how to pass format arguments or just post-process afterward

  # output <- purrr::pmap(dplyr::select(args,df,regex,parse_fun),parse_field)
  output <- purrr::pmap(args_in,sea::parse_field)

  names(output) <- namelist
  df <- tibble::as.tibble(output)

  # additional parsing for some elements
  df$nav_time <- readr::parse_time(stringr::str_extract(df$nav_time,"[0-9]{6}"),format="%H%M%S")
  df$lab_time <- readr::parse_time(stringr::str_extract(df$lab_time,"[0-9]{6}"),format="%H%M%S")
  df$sys_dttm <- update(df$sys_date, hour = lubridate::hour(df$sys_time),
                        minute=lubridate::minute(df$sys_time),
                        second=lubridate::second(df$sys_time))

  # Make datetimes from GPS using the system datetime
  df <- dplyr::mutate(df,lab_dttm = create_gps_dttm(lab_time,sys_dttm))
  df <- dplyr::mutate(df,nav_dttm = create_gps_dttm(nav_time,sys_dttm))

  # choose master datetime
  # use nav GPS as the default and revert to lab gps and sys time as required
  if(is.null(forceGPS)) {
    lon <- df$nav_lon
    lon[is.na(lon) & !is.na(df$lab_lon)] <- df$lab_lon[is.na(lon) & !is.na(df$lab_lon)]
    lat <- df$nav_lat
    lat[is.na(lat) & !is.na(df$lab_lat)] <- df$lab_lat[is.na(lat) & !is.na(df$lab_lat)]
    dttm <- df$nav_dttm
    dttm[is.na(dttm) & !is.na(df$lab_dttm)] <- df$lab_dttm[is.na(dttm) & !is.na(df$lab_dttm)]
  } else if (forceGPS == 'nav') {
    lon <- df$nav_lon
    lat <- df$nav_lat
    dttm <- df$nav_dttm
  } else if (forceGPS == 'lab') {
    lon <- df$lab_lon
    lat <- df$lab_lat
    dttm <- df$lab_dttm
  }

  # add the chosen, lon, lat and dttm
  df <- dplyr::mutate(df,lon=lon,lat=lat,dttm=dttm)

  # rearrange the columns into correct order
  df <- df[,c(42,40,41,37,1,2,39,3:8,38,9:36)]

  file <- tail(stringr::str_split(filein, "/")[[1]],1)
  df <- dplyr::mutate(df, file = file)

  df <- dplyr::mutate(df, local_dttm = NA, zd = NA, tz = NA)
  # df$local_dttm <- as.POSIXct(df$local_dttm)
  if(!is.null(zd)) {
    if(is.list(zd)) {
      if(length(zd[[1]]) == length(zd[[2]])-1) {
        time <- zd[[1]]
        zd <- zd[[2]]
        tz <- make_tz(zd)
        dttm_sw <- as.POSIXct(rep(NA,length(time)))
        for (i in 1:length(time)) {
          dttm_sw[i] <- lubridate::ymd_hm(time[i],tz = tz[i])
        }
        dttm_sw <- c(df$dttm[1], lubridate::with_tz(dttm_sw, "UTC"), tail(df$dttm,1))
        for (i in 1:length(tz)) {
          t1 = find_near(df$dttm,dttm_sw[i])
          t2 = find_near(df$dttm,dttm_sw[i+1])
          df$local_dttm[t1:t2] <- format(df$dttm[t1:t2],tz = tz[i],format = "%Y-%m-%d %H:%M")
          df$zd[t1:t2] <- zd[i]
          df$tz[t1:t2] <- tz[i]
          if(t2 == length(df$dttm)) {
            break
          }
        }
      }
    } else {
      tz <- make_tz(zd)
      df$local_dttm <- format(df$dttm,tz = tz,format = "%Y-%m-%d %H:%M")
      df$zd <- zd
      df$tz <- tz
    }
  }

  return(df)

}


#' Make a Olson timezone from ZD
#'
#' @param zd
#'
#' @return
#' @export
#'
#' @examples
make_tz <- function(zd) {

  tzbase <- "Etc/GMT"
  tz <- paste0(tzbase,ifelse(sign(zd)>=0,"+","-"),abs(zd))

  return(tz)

}

#' Read multiple ELG files from a folder
#'
#' @param root_folder
#'
#' @return
#' @export
#'
#' @examples
read_elg_fold <- function(root_folder, sort_elg = TRUE, ...) {

  files <- list.files(root_folder,pattern = '\\.elg$')
  if(length(files) > 0) {
    for (i in 1:length(files)) {
      filein <- file.path(root_folder,files[i])
      if(i==1) {
        elg <- read_elg(filein, ...)
      } else {
        elg <- dplyr::bind_rows(elg,read_elg(filein, ...))
      }
    }
    if(sort_elg) {
      elg <- dplyr::arrange(elg,dttm)
    }
  } else {
    elg <- NULL
  }

  return(elg)

}

#' Convert elg to hourlywork
#'
#' @param elg
#'
#' @return
#' @export
#'
#' @examples
elg_to_hourly <- function(elg) {

  t1 <- lubridate::ceiling_date(min(elg$dttm, na.rm = T),unit = "hour")
  t2 <- lubridate::floor_date(max(elg$dttm, na.rm = F),unit = "hour")
  timevec <- seq(t1,t2,lubridate::dhours(1))

  ii <- find_near(elg$dttm,timevec)
  df <- elg[ii,]

#   df <- tibble::tibble(dttm = timevec)
#   elg_names <- names(elg)
#   for (i in 2:ncol(elg)) {
#     if(length(which(is.na(elg[[i]]))) < nrow(elg)/2) {
#       var_add <- tibble::tibble(approx(elg$dttm,elg[[i]],timevec)$y)
#     } else {
#       var_add <- tibble::tibble(rep(NA,length(timevec)))
#     }
#     names(var_add) <- elg_names[i]
#     df <- dplyr::bind_cols(df,var_add)
#   }

  return(df)

}

#' Update ELG data
#'
#' Add to an existing elg tibble from an SEA Event File that is in the process of being recorded.
#'
#' @param df current tibble of ELG data
#' @param filein location of the *.elg file to update from
#'
#' @return
#' @export
#'
#' @examples
update_elg <- function(df,filein,preCheck=T) {

  # Find number of rows to skip
  nskip <- nrow(df)+1

  # read in the lines
  dfadd <- read_elg(filein,skip=nskip,preCheck=preCheck)

  df <- bind_rows(df,dfadd)
}


#' Cut out bad lines of elg file based on bad nav gps signals
#'
#' SEA event files contain output from a number of instruments
#' including GPS, flow-through, chirp, etc.
#'
#' @param filein .elg filepath to be read in
#' @param latstr regexp string to use to search for
#' @param fileout a specified output filepath.
#' If not specified, "_clean" is appended to name of input file and saved in the same directory.
#' @keywords
#' @export
#' @examples
#' clean_bad_elg()
#'
clean_bad_elg <- function(filein,latstr="[0-9]{4}\\.[0-9]{4}[NS]{1}",lonstr="[0-9]{5}\\.[0-9]{4}[EW]{1}",fileout=NULL) {

  # read elg data as charactor tibble
  df <- read_csv(filein,col_types = cols(.default = col_character()))
  names(df) <- gsub("-",".",names(df))

  # find all the correctly and incorrectly formatted latitude nav values using latstr


  keep1 <- 1:length(df$GPS.nav.Lat) %in% grep(latstr,df$GPS.nav.Lat)
  keep2 <- 1:length(df$GPS.nav.Lon) %in% grep(lonstr,df$GPS.nav.Lon)
  keep3 <- 1:length(df$GPS.nav.quality) %in% grep("^1$",df$GPS.nav.quality)

  badi <- which(!keep1|!keep2|!keep3)
  goodi <- which(keep1&keep2&keep3)

  # read in the raw lines from the elg file
  df_raw <- read_lines(filein)

  # only keep those lines which are good (plus the header line)
  df_raw_clean <- df_raw[append(1,goodi+1)]

  # create output filename if not specified in function call
  if(is.null(fileout)) {
    fileout <- str_replace(filein,".elg","_clean.elg")
  }

  # write data to file
  write_lines(df_raw_clean,fileout)

}


#' Create a GPS DateTime field
#'
#' ELG files typically have a GPS time, but no GPS data.
#' This function takes the system datetime field already parsed and returns a well formated GPS datetime.
#'
#' @param gps_time Raw GPS time field
#' @param sys_dttm system datetime
#'
#' @return
#' @export
#'
#' @examples
create_gps_dttm <- function(gps_time,sys_dttm) {
  if(length(which(is.na(gps_time))) < length(gps_time) &
     length(which(!is.na(gps_time))) > 100) {
    sys_time <- readr::parse_time(format(sys_dttm,"%H:%M:%S"))
    difft <- gps_time - sys_time
    goodi <- !is.na(difft)
    dayoffi <- difft < -8000
    x <- 1:length(difft)
    lf <- lsfit(x[goodi & !dayoffi],difft[goodi & !dayoffi])
    difft <- x*lf[[1]][2] + lf[[1]][1]
    gps_dttm = sys_dttm+difft
  } else {
    gps_dttm = readr::parse_datetime(rep("",length(gps_time)))
  }
  return(gps_dttm)
}



# ELG Parse functions -----------------------------------------------------

# Could make the following two functions into one




# Following code has now been replaced with parse_field

#' Generic parser for fields contained in araw charactor tibble
#'
#' Data read in to character tibble by read_csv(...,col_types = cols(.default=col_character()))).
#' Takes parsing function as input so can differentiate between different column types.
#' Takes tibble and returns vector
#'
#' @param df character tibble
#' @param field name of field to parse (passed to warning and message functions for readability).
#' @param regex regex for finding the appropriate field. Can be vector of searches with earlier values prioritized.
#' @param parse_fun the parsing function to use in creating the new vector
#' @param ... additional parameters passed to parse_fun (i.e. format = "" for parse_date)
#'
#' @return
#' @export
#'
#' @examples
#' parse_elg_field()
parse_elg_field <- function(df,field,regex,parse_fun,...) {

  df_names <- str_to_lower(names(df))

  # Find and parse field
  for (i in 1:length(regex)) {
    ii <- grep(regex[i],df_names)
    if(length(ii)>0) break
  }

  if (length(ii)>1) {
    warning_mult(field,ii)
    ii <- ii[1]
  }
  if (length(ii)==0) {
    warning_empty(field)
    output <- parse_fun(rep(NA,nrow(df)),...)
    # output <- list(parse_fun(rep(NA,nrow(df)),...))
    # names(output) <- name
    # df <- bind_cols(df,output)
  } else {
    message_read(field,ii)
    # names(df[ii]) <- name
    # df[[ii]] <- parse_fun(df[[ii]],...)
    output <- parse_fun(df[[ii]],...)
  }



}

#' Parse GPS fields from ELG file
#'
#' Takes in a raw character tibble data frame and parses the 6 potential GPS fields
#' Takes tibble, returns vector
#'
#' @param df tibble character data frame input
#' @param source choose which GPS source to parse ("lab","nav")
#'
#' @return
#' @export
#'
#' @examples
#' parse_gps()
parse_elg_gps <- function(df,source){
  df_names <- str_to_lower(names(df))

  output <- NULL

  fieldnames <- c("time","lon","lat","sog","cog","quality")
  for (i in 1:6) {
    field <- paste(str_to_title(source),"GPS",str_to_title(fieldnames[i]))
    regexp <- paste("gps",source,fieldnames[i],sep=".")
    outfield <- paste0(source,"_", fieldnames[i])

    ii <- grep(regexp,df_names)
    if (length(ii)>1) {
      warning_mult(field,ii)
      ii <- ii[1]
    }
    if(length(ii)==0) {
      warning_empty(field)
      # TODO: Ensure that correct class is passed on to each empty field here
      assign(outfield,rep(NA,nrow(df)))
    } else {
      message_read(field,ii)
      if (i == 1) {
        assign(outfield,parse_time(str_extract(df[[ii]],"[0-9]{6}"),format="%H%M%S"))
      } else if (i == 2) {
        assign(outfield,parse_lon(df[[ii]]))
      } else if (i == 3) {
        assign(outfield,parse_lat(df[[ii]]))
      } else if (i == 4 | i == 5) {
        assign(outfield,parse_double(df[[ii]]))
      } else if (i == 6) {
        assign(outfield,parse_integer(df[[ii]]))
      }
    }
    output[[i]] <- get(outfield)
    names(output)[i] <- outfield
  }

  return(output)
}
# TODO: Work out if you can change the function called to parse the data on the fly
# Should be able to generate generic parsers for all fields
# See parse_elg_field - need to find a way to impliment for parse_gps


# warning/message functions -------------------------------------------------------

# NB: All these functions should become obsolete when parsing of elg fields is optimized


#' Produces warning for an empty field
#'
#' Called during parse_elg_* functions if no match to the input regex is found
#'
#' @param field The proper name of the field - gets passed to output string to better read
#'
#' @return
#' @export
#'
#' @examples
#' warning_empty("Temperature")
warning_empty <- function(field) {
  warning(paste(field,"not found in ELG file. Setting all values to NA"))
}

#' Produces warning for multiple name matches
#'
#' Called during parse_elg_* functions if multiple matches to the input regex are found
#' NB: by default the first value is selected.
#'
#' @param field  The proper name of the field - gets passed to output string to better read
#' @param ii matched field names
#'
#' @return
#' @export
#'
#' @examples
#' warning_mult("Temperature,c(2,23))
warning_mult <- function(field,ii) {
  warning(paste(field,"found in multiple slots:", paste0(ii,collapse="; "), ". By default, using data from slot", ii[1]))
}

#' Prints message that states which slot is being read
#'
#' Called during parse_elg_* functions when field is being parsed
#'
#' @param field The proper name of the field - gets passed to output string to better read
#' @param ii slot used for the parsing
#'
#' @return
#' @export
#'
#' @examples
#' message_read("Temperature",13)
message_read <- function(field,ii) {
  message(paste("Reading", field, "data from slot", ii))
}





