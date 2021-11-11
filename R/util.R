#' Find index of nearest value in vector
#'
#' @param vec
#' @param vals
#'
#' @return
#' @export
#'
#' @examples
find_near <- function(vec,vals) {

  locs <- rep(NA,length(vals))
  for (i in 1:length(vals)) {
    if(!is.na(vals[i])) {
      locs[i] <- which.min(abs(vec-vals[i]))
    } else {
      locs[i] <- NA
    }
  }

  return(locs)

}


#' Convert wind speed and wind direction to u and v
#'
#' @param ws input wind speed vector
#' @param wd input wind direction vector
#'
#' @return
#' @export
#'
#' @examples
wswd_to_uv <- function(ws,wd) {

  v = -ws*cos(wd*pi/180)
  u = -ws*sin(wd*pi/180)

  out <- tibble::tibble(u = u, v = v)

}


#' U and V to wind speed and wind direction
#'
#' @param u
#' @param v
#'
#' @return
#' @export
#'
#' @examples
uv_to_wswd <- function(u,v) {

  ws <- sqrt(u^2+v^2)
  wd <- 90 - atan2(v,u)*180/pi
  wd[wd<0&!is.na(wd)] <- wd[wd<0&!is.na(wd)] + 360

  out <- list(ws = ws, wd = wd)

  return(out)
}

DIM <- function(x) if(is.null(dim(x))) length(x) else dim(x)


add_file_cruiseID <- function(filename, cruiseID) {

  if(!is.null(filename) & !is.null(cruiseID)) {
    file_dir <- dirname(filename)
    filename <- basename(filename)
    filename <- paste0(cruiseID,"_",filename)
    filename <- file.path(file_dir,filename)
  } else {
    stop("filename or cruiseID are not set")
  }

  return(filename)

}
