#' @title Hurricanes
#' @description Hurricanes is a web-scraping library for R designed to deliver 
#' hurricane data (past and current) into well-organized datasets. With these 
#' datasets you can explore past hurricane tracks, forecasts and structure 
#' elements. 
#' 
#' Text products (Forecast/Advisory, Public Advisory, Discussions and 
#' Probabilities) are only available from 1998 to current. An effort will be 
#' made to add prior data as available.
#' 
#' @section Getting Storms:
#' List all storms that have developed by year and basin. Year must be in a 
#' four-digit format (\%Y) and no earlier than 1998. Basin can be one or both 
#' of Atlantic ("AL") or East Pacific ("EP"). 
#' \describe{
#'   \item{\code{\link{get_storms}}}{List all storms by year, basin}
#' }
#' 
#' @section Getting Storm Data:
#' There are several text products available for any given observation time. 
#' Some text products are deprecated (Strike Probabilities, replaced by Wind 
#' Probabilities). Some text products are rarely issued (Updates, Position 
#' Estimates). Expect some discrepancies between datasets. 
#' 
#' Forecast/Advisory products contain the bulk of data available for a specifc 
#' storm. These products contain what I call the 'basic' information: latitude, 
#' longitude, wind, pressure. In addition they contain the 
#' structure of the storm such as eye size and wind spread. Of course, forecast 
#' tracks are also available and forecast wind spreads. 
#' 
#' In order to obtain links to the forecast products you must run 
#' \code{\link{get_fstadv_links}} to obtain a list of links. 
#' 
#' \strong{These datasets will be reset on each run.}
#' 
#' @docType package
#' @name Hurricanes
NULL

#' @title get_nhc_link
#' @description Return root link of NHC archive pages.
#' @param withTrailingSlash True, by default. False returns URL without 
#' trailing slash.
#' @export
get_nhc_link <- function(withTrailingSlash = TRUE) {
  if(withTrailingSlash)
    return('http://www.nhc.noaa.gov/')
  return('http://www.nhc.noaa.gov')
}

#' @title month_str_to_num
#' @description Convert three-character month abbreviation to integer
#' @param m Month abbreviated (SEP, OCT, etc.)
#' @return numeric 1-12
month_str_to_num <- function(m) {
  
  if(is.character(m) & length(m) != 3) {
    m <- which(month.abb == to_proper(m))
    return(m)
  } else {
    stop('Expected three-character string.')
  }
  
}
