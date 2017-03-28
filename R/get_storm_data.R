#' @title get_products
#' @description Get list of all products from a storm's archive page
#' @param link URL to storm's archive page.
#' @export
get_products <- function(link) {
  
  # Get year
  year <- .extract_year_archive_link(link)
  
  products <- .get_storm_content(link) %>% 
    rvest::html_attr("href") %>% 
    na.omit()
  
  nhc_url <- get_nhc_link(withTrailingSlash = FALSE)
  
  # Depending on the year these URL's are formatted various ways.
  if(year == 1998) {
    products <- stringr::str_c(nhc_url, '/archive/', year, '/', products)
  } else {
    products <- stringr::str_c(nhc_url, products)
  }
  
  return(products)
}

#' @title .get_storm_content
#' @description Extract content from a storm's archive page
#' @param link of archive page
#' @return page content of archive page
.get_storm_content <- function(link) {
  
  # Get year
  year <- .extract_year_archive_link(link)
  
  # There are different layouts for various years. Have to work through it...
  if(year <= 2001) {
    page <- xml2::read_html(link) %>% 
      rvest::html_nodes('body') %>% 
      rvest::html_nodes('table') %>% 
      rvest::html_nodes('td') %>% 
      rvest::html_children()
    
  } else {
    page <- xml2::read_html(link) %>%
      rvest::html_nodes('.center') %>%
      rvest::html_nodes('.content') %>%
      rvest::html_nodes('table') %>%
      rvest::html_nodes('td') %>%
      rvest::html_children()
  }
  
  return(page)
}

#' @title get_storm_data
#' @description Retrieve data from products.
#' @details \code{get_storm_data} is a wrapper function to make it more 
#' convenient to access the various storm products. 
#' 
#' Types of products:
#' \describe{
#'   \item{discus}{Storm Discussions. This is technical information on the 
#'     cyclone such as satellite presentation, forecast model evaluation, etc.}
#'   \item{fstadv}{Forecast/Advisory. These products contain the meat of an 
#'     advisory package. Current storm information is available as well as 
#'     structural design and forecast data.}
#'   \item{posest}{Position Estimate. Issued generally when a storm is 
#'     threatening; provides a brief update on location and winds.}
#'   \item{public}{Public Advisory. Issued for public knowledge; more often for 
#'     Atlantic than East Pacific storms. Contains general information.}
#'   \item{prblty}{Strike Probability. Discontinued after the 2005 hurricane 
#'     season, strike probabilities list the chances of x-force winds in a 
#'     particular city.}
#'   \item{update}{Cyclone Update. Generally issued when a significant change 
#'     occurs in the cyclone.}
#'   \item{windprb}{Wind Probability. Replace strike probabilities beginning in 
#'     the 2006 season. Nearly identical.}
#' }
#' @param ... Products to retrieve. c("discus", "fstadv", "posest", "public", 
#' "prblty", "update", "windprb")
#' @param names By default product dataframes will be returned as shown in 
#' \code{...}. The names parameter gives a way to provide alternative names for 
#' the returned dataframes. Pass a named list where the name of each element 
#' is that of the product. See examples for more information.
#' @param link to storm's archive page.
#' @return Dataframes for each of the products.
#' @examples
#' ## Get public advisories for Tropical Storm Charley, 1998
#' get_storm_data("public", 
#'                link = "http://www.nhc.noaa.gov/archive/1998/1998CHARLEYadv.html")
#' 
#' ## Same as above but give alternate name.
#' get_storm_data("public", 
#'                names = list("public" = "al.1998.charley.public"), 
#'                link = "http://www.nhc.noaa.gov/archive/1998/1998CHARLEYadv.html")
#' ## Get forecast/advisory and storm discussion
#' get_storm_data("fstadv", "discus", 
#'                names = list("fstadv" = "al.1998.charley.fstadv", 
#'                             "discus" = "al.1998.charley.discus"), 
#'                link = "http://www.nhc.noaa.gov/archive/1998/1998CHARLEYadv.html")
#' @export
get_storm_data <- function(..., names = list(), link) {
  
  x <- list(...)
  if(!(all(x %in% c("discus", "fstadv", "posest", "public", "prblty", "update", 
                    "windprb"))) | length(x) == 0)
    stop(paste0("Invalid products included. Only discus, fstadv, posest, ", 
                "public, prblty, update, windprb are valid options.", 
                "See ?get_storm_data for more info."))
  
  if(is.null(link))
    stop("No link provided.")
  
  x <- unlist(x)
  
  s <- sapply(x, function(z, n = names, l = link){
    f <- paste("get", z, sep = "_")
    res <- do.call(f, args = list("link" = l))
    if(!is.null(n[[z]])) {
      assign(n[z][[1]], res, envir = .GlobalEnv)
    } else {
      assign(z, res, envir = .GlobalEnv)
    }
  })
  
  return(TRUE)
  
}
