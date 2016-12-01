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

  # Check status of link(s)
  valid.link <- sapply(link, .status)
  valid.link <- na.omit(valid.link)
  if(length(valid.link) == 0)
    stop("No valid links.")
  
  products <- unlist(sapply(valid.link, get_products))
  
  products.discus <- lapply(filter_discussions(products), discus)
  
  discus <- data.table::rbindlist(products.discus)
  
  return(discus)
  
}

#' @title discus
#' @description Parse storm Discussion products
#' @details Given a direct link to a discussion product, parse and return 
#' dataframe of values.
#' @param link Link to a storm's specific discussion product.
#' @param display_link Display each link as being worked; default is TRUE.
#' @return Dataframe
#' @seealso \code{\link{get_discus}}
#' @export
discus <- function(link, display_link = TRUE) {
  
  contents <- scrape_contents(link, display_link = display_link)
  
  # Make sure this is a discussion product
  if(!any(stringr::str_count(contents, c("MIATCDAT", "MIATCDEP"))))
    stop(sprint("Invalid Discussion link. %s", l))
  
  df <- .create_df_discus()
  
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