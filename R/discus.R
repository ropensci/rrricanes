#' @title discus
#' @description Parse storm Discussion products
#' @details Given a direct link to a discussion product, parse and return 
#' dataframe of values.
#' @param l Link to a storm's specific discussion product.
#' @param display_link Display each link as being worked; default is TRUE.
#' @return Dataframe
#' @seealso \code{\link{get_discus}}
#' @export
discus <- function(l, display_link = TRUE) {
  
  if(!.status(l))
    stop(sprintf("Link unavailable. %d", l))
  
  if(display_link)
    message(sprintf("Working %s", l))
  
  contents <- l %>% 
    xml2::read_html() %>% 
    rvest::html_text()
  
  # Make sure this is a discussion product
  if(!any(stringr::str_count(contents, c("MIATCDAT", "MIATCDEP"))))
    stop(sprint("Invalid Discussion link. %s", l))
  
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