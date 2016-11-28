#' @title convert_lat_lon
#' @description Converts lat, lon to negative if in southern, western 
#'   hemisphere, respectively
#' @param x integer
#' @param y character
#' @return integer positive or negative
#' @export
convert_lat_lon <- function(x, y) {
  
  if(!is.numeric(x)) {stop("x is not numeric!")}
  if(!(y %in% c('N', 'S', 'E', 'W'))) { stop("y must be c('N','S','E','W')") }
  
  ifelse(y == 'S' | y == 'W', return(x * -1), return(x))
  
}

#' @title knots_to_mph
#' @description convert knots (kt) to miles per hour (mph)
#' @param x wind speed in knots
#' @return x in mph
#' @export
knots_to_mph <- function(x) {
  return(x * 1.15077945)
}

#' @title mb_to_in
#' @description convert millibars (mb) to inches of mercury (in)
#' @param x barometric pressure in mb
#' @return x in in
#' @export
mb_to_in <- function(x) {
  return(x * 0.029529983071)
}
