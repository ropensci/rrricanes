#' @title get_posest
#' @description Return dataframe of position estimate data.
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane, 
#'     etc.}
#'   \item{Name}{Name of storm}
#'   \item{Adv}{Advisory Number}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Contents}{Text content of product}
#' }
#' @param link URL to storm's archive page.
#' @seealso \code{\link{get_storms}}, \code{\link{posest}}
#' @export
get_posest <- function(link) {
  if(!.status(link))
    stop(sprintf("Link unavailable. %d", l))
  
  products <- get_products(link)
  
  products.posest <- lapply(filter_discussions(products), posest)
  
  posest <- data.table::rbindlist(products.posest)
  
  return(posest)
  
}

#' @title posest
#' @description Extrapolate data from Position Estimate products. 
#' @details Given a direct link to a position estimate product, parse and return 
#' dataframe of values.
#' @param l URL of a specific position estimate product
#' @param display_link Display each link as being worked; default is TRUE.
#' @return Dataframe
#' @seealso \code{\link{get_posest}}
#' @export
posest <- function(l, display_link = TRUE) {
  
  if(!.status(l))
    stop(sprintf("Link unavailable. %d", l))
  
  if(display_link)
    message(sprintf("Working %s", l))
  
  contents <- l %>% 
    xml2::read_html() %>% 
    rvest::html_text()
  
  # Make sure this is a public advisory product
  if(!any(stringr::str_count(contents, c("MIATCEAL", "MIATCEEP"))))
    stop(sprint("Invalid Position Estimate link. %s", l))
  
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
