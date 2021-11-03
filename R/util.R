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

  out <- tibble::tibble(ws = ws, wd = wd)

  return(out)
}
