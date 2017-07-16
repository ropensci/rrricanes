#' @title filter_discus
#' @description Filter out storm discussion links.
#' @param links Vector of URLs retrieved from storm's archive page.
#' @keywords internal
filter_discus <- function(links) {
  ptn <- c("/dis/N[AL|EP]", "discus")
  return(filter_products(ptn, links))
}

#' @title filter_fstadv
#' @description Filter out forecast/advisory links
#' @param links Vector of URLs retrieved from storm's archive page.
#' @keywords internal
filter_fstadv <- function(links) {
  ptn <- c("/mar/M[AL|EP]", "fstadv")
  return(filter_products(ptn, links))
}

#' @title filter_orig
#' @description For Katrina, 2005, there are two identical discussions; one of which has
#'   'orig' in the URL. Because this link is not captured above it will throw an
#'   error. This function is an effort to capture it and pass
#'   it through the validation but, at least for this Katrina it is not
#'   necessary. That being said, if there is output for any other storms it should
#'   be reviewed as it is common for the NHC to issue UPDATED statements.
#' @param links Vector of URLs retrieved from storm's archive page.
#' @keywords internal
filter_orig <- function(links) {
  ptn <- "orig"
  return(filter_products(ptn, links))
}

#' @title filter_posest
#' @description Get position estimates
#' @param links Vector of URLs retrieved from storm's archive page.
#' @keywords internal
filter_posest <- function(links) {
  ptn <- "posest"
  return(filter_products(ptn, links))
}

#' @title filter_public
#' @description Get public advisories
#' @param links Vector of URLs retrieved from storm's archive page.
#' @keywords internal
filter_public <- function(links) {
  ptn <- c("/pub/P[AL|EP]", "/pub/PA[AL|EP]", "/pub/PB[AL|EP]", "public")
  return(filter_products(ptn, links))
}

#' @title filter_prblty
#' @description Get strike probabilities.
#' @details Strike probability products were terminated at the end of the 2005
#' season, replaced by wind probabilities.
#' @seealso \code{\link{filter_wndprb}}
#' @param links Vector of URLs retrieved from storm's archive page.
#' @keywords internal
filter_prblty <- function(links) {
  ptn <- c("/prb/L[AL|EP]", "prblty")
  return(filter_products(ptn, links))
}

#' @title filter_update
#' @description Get updates
#' @param links Vector of URLs retrieved from storm's archive page.
#' @keywords internal
filter_update <- function(links) {
  ptn <- "update"
  return(filter_products(ptn, links))
}

#' @title filter_wndprb
#' @description Get wind probabilities
#' @details Wind probability products replaced the strike probability products
#' at the beginning of the 2006 season.
#' @seealso \code{\link{filter_prblty}}
#' @param links Vector of URLs retrieved from storm's archive page.
#' @keywords internal
filter_wndprb <- function(links) {
  ptn <- "wndprb"
  return(filter_products(ptn, links))
}

#' @title filter_products
#' @description Filter out links matching pattern ptn
#' @param ptn Regex pattern to find
#' @param links Vector of links to filter
#' @keywords internal
filter_products <- function(ptn, links) {
  x <- purrr::map(links, ~.[grepl(paste(ptn, collapse = "|"), .)])
  return(x)
}
