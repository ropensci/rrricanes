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

#' @title fstadv
#' @description Extrapolate data from FORECAST/ADVISORY products. 
#' @details Given a direct link to a forecast/advisory product, parse and 
#' return dataframe of values.
#' @param l URL of a specific FORECAST/ADVISORY product
#' @param display_link Display each link as being worked; default is TRUE.
#' @return Dataframe
#' @seealso \code{\link{get_fstadv}}
#' @export
fstadv <- function(l, display_link = TRUE) {
  
  if(!.status(l))
    stop(sprintf("Link unavailable. %d", l))
  
  if(display_link)
    message(sprintf("Working %s", l))
  
  contents <- l %>% 
    xml2::read_html() %>% 
    rvest::html_text()
  
  # Make sure this is a public advisory product
  if(!any(stringr::str_count(contents, c("MIATCMAT", "MIATCMEP"))))
    stop(sprint("Invalid Forecast/Advisory link. %s", l))
  
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
