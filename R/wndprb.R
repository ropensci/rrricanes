#' @title get_wndprb
#' @description Return dataframe of wind probability data.
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane, 
#'     etc.}
#'   \item{Name}{Name of storm}
#'   \item{Adv}{Advisory Number}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Contents}{Text content of product}
#' }
#' @param link URL to storm's archive page.
#' @seealso \code{\link{get_storms}}, \code{\link{wndprb}}
#' @export
get_wndprb <- function(link) {
  if(!.status(link))
    stop(sprintf("Link unavailable. %d", l))
  
  products <- get_products(link)
  
  products.wndprb <- lapply(filter_wind_probabilities(products), wndprb)
  
  wndprb <- data.table::rbindlist(products.wndprb)
  
  return(wndprb)
}

#' @title wndprb
#' @description Parse wind probability products
#' @details Given a direct link to a wind probability product, parse and return 
#' dataframe of values.
#' @param l Link to a storm's specific wind probability product.
#' @param display_link Display each link as being worked; default is TRUE.
#' @return Dataframe
#' @seealso \code{\link{get_wndprb}}
#' @export
wndprb <- function(l, display_link = TRUE) {
  
  if(!.status(l))
    stop(sprintf("Link unavailable. %d", l))
  
  if(display_link)
    message(sprintf("Working %s", l))
  
  contents <- l %>% 
    xml2::read_html() %>% 
    rvest::html_text()
  
  # Make sure this is a wndprb advisory product
  if(!any(stringr::str_count(contents, c("MIAPWSAT", "MIAPWSEP"))))
    stop(sprint("Invalid Wind Probability link. %s", l))
  
  df <- .create_df_public_advisories()
  
  status <- scrape_header(contents, ret = "status")
  name <- scrape_header(contents, ret = "name")
  adv <- scrape_header(contents, ret = "adv")
  date <- scrape_header(contents, ret = "date")
  
  df <- df %>% 
    tibble::add_row("Status" = status, 
                    "Name" = name, 
                    "Adv" = adv, 
                    "Date" = date, 
                    "Contents" = contents)
  
  return(df)
}