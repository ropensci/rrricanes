#' @title filter_discussions 
#' @description Filter out storm discussion links.
#' @export
filter_discussions <- function(links) {
  ptn <- c("/dis/N[AL|EP]", "discus")
  return(.filter_products(ptn, links))
}

#' @title filter_forecast_advisories
#' @description Filter out forecast/advisory links
#' @export
filter_forecast_advisories <- function(links) {
  ptn <- c("/mar/M[AL|EP]", "fstadv")
  return(.filter_products(ptn, links))
}

#' @title filter_orig
#' @description For Katrina, 2005, there are two identical discussions; one of which has 
#'   'orig' in the URL. Because this link is not captured above it will throw an 
#'   error. This function is an effort to capture it and pass 
#'   it through the validation but, at least for this Katrina it is not 
#'   necessary. That being said, if there is output for any other storms it should 
#'   be reviewed as it is common for the NHC to issue UPDATED statements.
#' @export
filter_orig <- function(links) {
  ptn <- "orig"
  return(.filter_products(ptn, links))
}

#' @title filter_position_estimate
#' @description Get position estimates
#' @export
filter_position_estimate <- function(links) {
  ptn <- "posest"
  return(.filter_products(ptn, links))
}

#' @title filter_public_advisories
#' @description Get public advisories
#' @export
filter_public_advisories <- function(links) {
  ptn <- c("/pub/P[AL|EP]", "/pub/PA[AL|EP]", "/pub/PB[AL|EP]", "public")
  return(.filter_products(ptn, links))
}

#' @title filter_strike_probabilities
#' @description Get strike probabilities.
#' @details Strike probability products were terminated at the end of the 2005 
#' season, replaced by wind probabilities.
#' @seealso \code{\link{filter_wind_probabilities}}
#' @export
filter_strike_probabilities <- function(links) {
  ptn <- c("/prb/L[AL|EP]", "prblty")
  return(.filter_products(ptn, links))
}

#' @title filter_updates
#' @description Get updates
#' @export
filter_updates <- function(links) {
  ptn <- "update"
  return(.filter_products(ptn, links))
}

#' @title filter_wind_probabilities
#' @description Get wind probabilities
#' @details Wind probability products replaced the strike probability products 
#' at the beginning of the 2006 season.
#' @seealso \code{\link{filter_strike_probabilities}}
#' @export
filter_wind_probabilities <- function(links) {
  ptn <- "wndprb"
  return(.filter_products(ptn, links))
}

#' @title .filter_products
#' @description Filter out links matching pattern ptn
.filter_products <- function(ptn, links) {
  return(links[grepl(paste(ptn, collapse = "|"), links)])
}
