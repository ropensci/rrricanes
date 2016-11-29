#' @title get_prblty
#' @description Return dataframe of strike probability data.
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane, 
#'     etc.}
#'   \item{Name}{Name of storm}
#'   \item{Adv}{Advisory Number}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Contents}{Text content of product}
#' }
#' @param link URL to storm's archive page.
#' @seealso \code{\link{get_storms}}, \code{\link{prblty}}
#' @export
get_prblty <- function(link) {

  # Check status of link(s)
  valid.link <- sapply(link, .status)
  valid.link <- na.omit(valid.link)
  if(length(valid.link) == 0)
    stop("No valid links.")
  
  products <- unlist(sapply(valid.link, get_products))
  
  products.prblty <- lapply(filter_strike_probabilities(products), prblty)
  
  prblty <- data.table::rbindlist(products.prblty)
  
  return(prblty)
}

#' @title prblty
#' @description Parse strike probability products
#' @details Given a direct link to a strike probability advisory product, parse 
#' and return dataframe of values.
#' @param link Link to a storm's specific strike probability advisory product.
#' @param display_link Display each link as being worked; default is TRUE.
#' @return Dataframe
#' @seealso \code{\link{get_prblty}}
#' @export
prblty <- function(link, display_link = TRUE) {
  
  valid.link <- sapply(link, .status)
  valid.link <- na.omit(valid.link)
  if(length(valid.link) == 0)
    stop("No valid links.")
  
  if(display_link)
    message(sprintf("Working %s", valid.link))
  
  contents <- valid.link %>% 
    xml2::read_html() %>% 
    rvest::html_text()
  
  # Make sure this is a strike probability product
  if(!any(stringr::str_count(contents, c("MIASPFAT", "MIASPFEP"))))
    stop(sprint("Invalid Strike Probability link. %s", l))
  
  df <- .create_df_prblty()
  
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