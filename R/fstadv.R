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

  # Check status of link(s)
  valid.link <- sapply(link, .status)
  valid.link <- na.omit(valid.link)
  if(length(valid.link) == 0)
    stop("No valid links.")
  
  products <- unlist(sapply(valid.link, get_products))

  products.fstadv <- lapply(filter_forecast_advisories(products), fstadv)
  
  fstadv <- data.table::rbindlist(products.fstadv)
  
  return(fstadv)
  
}

#' @title fstadv
#' @description Extrapolate data from FORECAST/ADVISORY products. 
#' @details Given a direct link to a forecast/advisory product, parse and 
#' return dataframe of values.
#' @param link URL of a specific FORECAST/ADVISORY product
#' @param display_link Display each link as being worked; default is TRUE.
#' @return Dataframe
#' @seealso \code{\link{get_fstadv}}
#' @export
fstadv <- function(link, display_link = TRUE) {
  
  valid.link <- sapply(link, .status)
  valid.link <- na.omit(valid.link)
  if(length(valid.link) == 0)
    stop("No valid links.")
  
  if(display_link)
    message(sprintf("Working %s", valid.link))
  
  contents <- valid.link %>% 
    xml2::read_html() %>% 
    rvest::html_nodes("pre") %>% 
    rvest::html_text()
  
  # Make sure this is a public advisory product
  if(!any(stringr::str_count(contents, c("MIATCMAT", "MIATCMEP"))))
    stop(sprint("Invalid Forecast/Advisory link. %s", l))
  
  df <- .create_df_fstadv()

  status <- scrape_header(contents, ret = "status")
  name <- scrape_header(contents, ret = "name")
  adv <- scrape_header(contents, ret = "adv")
  date <- scrape_header(contents, ret = "date")
  key <- scrape_header(contents, ret = "key")
  
  df <- df %>% 
    tibble::add_row("Status" = status, 
                    "Name" = name, 
                    "Adv" = adv, 
                    "Date" = date, 
                    "Key" = key, 
                    "Contents" = contents)
  
  return(df)
}
