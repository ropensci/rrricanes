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

  # Check status of link(s)
  valid.link <- sapply(link, .status)
  valid.link <- na.omit(valid.link)
  if(length(valid.link) == 0)
    stop("No valid links.")
  
  products <- unlist(sapply(valid.link, get_products))
  
  products.public <- lapply(filter_public_advisories(products), public)
  
  public <- data.table::rbindlist(products.public)
  
  return(public)
}

#' @title public
#' @description Parse Public Advisory products
#' @details Given a direct link to a public advisory product, parse and return 
#' dataframe of values.
#' @param link Link to a storm's specific public advisory product.
#' @param display_link Display each link as being worked; default is TRUE.
#' @return Dataframe
#' @seealso \code{\link{get_public}}
#' @export
public <- function(link, display_link = TRUE) {

  valid.link <- sapply(link, .status)
  valid.link <- na.omit(valid.link)
  if(length(valid.link) == 0)
    stop("No valid links.")
  
  if(display_link)
    message(sprintf("Working %s", valid.link))
  
  contents <- valid.link %>% 
    xml2::read_html() %>% 
    rvest::html_text()
  
  # Make sure this is a public advisory product
  if(!any(stringr::str_count(contents, c("MIATCPAT", "MIATCPEP"))))
    stop(sprint("Invalid Public Advisory link. %s", l))

  df <- .create_df_public()
  
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