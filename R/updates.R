#' @title get_updates
#' @description Return dataframe of cyclone update data.
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane, 
#'     etc.}
#'   \item{Name}{Name of storm}
#'   \item{Adv}{Advisory Number}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Contents}{Text content of product}
#' }
#' @param link URL to storm's archive page.
#' @seealso \code{\link{get_storms}}, \code{\link{updates}}
#' @export
get_updates <- function(link) {

  # Check status of link(s)
  valid.link <- sapply(link, .status)
  valid.link <- na.omit(valid.link)
  if(length(valid.link) == 0)
    stop("No valid links.")
  
  products <- unlist(sapply(valid.link, get_products))
  
  products.updates <- lapply(filter_updates(products), updates)
  
  updates <- data.table::rbindlist(products.updates)
  
  return(updates)
}

#' @title updates
#' @description Parse cyclone update products
#' @details Given a direct link to a cyclone update product, parse and return 
#' dataframe of values.
#' @param link Link to a storm's specific updates advisory product.
#' @param display_link Display each link as being worked; default is TRUE.
#' @return Dataframe
#' @seealso \code{\link{get_updates}}
#' @export
updates <- function(link, display_link = TRUE) {
  
  valid.link <- sapply(link, .status)
  valid.link <- na.omit(valid.link)
  if(length(valid.link) == 0)
    stop("No valid links.")
  
  if(display_link)
    message(sprintf("Working %s", valid.link))
  
  contents <- valid.link %>% 
    xml2::read_html() %>% 
    rvest::html_text()
  
  # Make sure this is a updates advisory product
  if(!any(stringr::str_count(contents, c("MIATCUAT", "MIATCUEP"))))
    stop(sprint("Invalid Cyclone Update link. %s", l))
  
  df <- .create_df_updates()
  
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