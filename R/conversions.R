#' @title convert_lat_lon
#' @description Converts lat, lon to negative if in southern, western hemisphere, respectively
#' @param x integer
#' @param y character
#' @return integer positive or negative
convert_lat_lon <- function(x, y) {
  
  if(!is.numeric(x)) {stop("x is not numeric!")}
  if(!(y %in% c('N', 'S', 'E', 'W'))) { stop("y must be c('N', 'S', 'E', 'W')") }
  
  ifelse(y == 'S' | y == 'W', return(x * -1), return(x))
  
}
