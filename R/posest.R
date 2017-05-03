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
#' @param msg Show link being worked. Default, FALSE.
#' @seealso \code{\link{get_storms}}, \code{\link{posest}}
#' @export
get_posest <- function(link, msg = FALSE) {

  # Check status of link(s)
  valid.link <- sapply(link, .status)
  valid.link <- na.omit(valid.link)
  if(length(valid.link) == 0)
    stop("No valid links.")
  
  products <- unlist(sapply(valid.link, get_products))
  
  products.posest <- lapply(filter_discussions(products), posest, msg = msg)
  
  posest <- data.table::rbindlist(products.posest)
  
  return(posest)
  
}

#' @title posest
#' @description Extrapolate data from Position Estimate products. 
#' @details Given a direct link to a position estimate product, parse and return 
#' dataframe of values.
#' @param link URL of a specific position estimate product
#' @param msg Display each link as being worked; default is FALSE.
#' @return Dataframe
#' @seealso \code{\link{get_posest}}
#' @export
posest <- function(link, msg = FALSE) {
  
  contents <- scrape_contents(link, msg = msg)
  
  # Make sure this is a public advisory product
  if(!any(stringr::str_count(contents, c("MIATCEAT", "MIATCEEP"))))
    stop(sprint("Invalid Position Estimate link. %s", l))
  
  df <- .create_df_posest()
  
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
