#' @title get_discus
#' @description Return dataframe of discussion data.
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane, 
#'     etc.}
#'   \item{Name}{Name of storm}
#'   \item{Adv}{Advisory Number}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Contents}{Text content of product}
#' }
#' @param link URL to storm's archive page.
#' @seealso \code{\link{get_storms}}, \code{\link{public}}
#' @export
get_discus <- function(link) {
  if(!.status(link))
    stop(sprintf("Link unavailable. %d", l))
  
  products <- get_products(link)
  
  products.discus <- lapply(filter_discussions(products), discus)
  
  discus <- data.table::rbindlist(products.discus)
  
  return(discus)
  
}

#' @title get_fstadv
#' @description Return dataframe of forecast/advisory data.
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane, 
#'     etc.}
#'   \item{Name}{Name of storm}
#'   \item{Adv}{Advisory Number}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Contents}{Text content of product}
#' }
#' @param link URL to storm's archive page.
#' @seealso \code{\link{get_storms}}, \code{\link{public}}
#' @export
get_fstadv <- function(link) {
  if(!.status(link))
    stop(sprintf("Link unavailable. %d", l))
  
  products <- get_products(link)
  
  products.fstadv <- lapply(filter_forecast_advisories(products), fstadv)
  
  fstadv <- data.table::rbindlist(products.fstadv)
  
  return(fstadv)
  
}

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

#' @title get_public
#' @description Return dataframe of public advisory data.
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane, 
#'     etc.}
#'   \item{Name}{Name of storm}
#'   \item{Adv}{Advisory Number}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Contents}{Text content of product}
#' }
#' @param link URL to storm's archive page.
#' @seealso \code{\link{get_storms}}, \code{\link{public}}
#' @export
get_public <- function(link) {
  if(!.status(link))
    stop(sprintf("Link unavailable. %d", l))
  
  products <- get_products(link)
  
  products.public <- lapply(filter_public_advisories(products), public)
  
  public <- data.table::rbindlist(products.public)
  
  return(public)
  
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
