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
