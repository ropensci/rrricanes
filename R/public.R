#' @title public
#' @description Parse Public Advisory products
#' @details Given a direct link to a public advisory product, parse and return 
#' dataframe of values.
#' @param l Link to a storm's specific public advisory product.
#' @return Dataframe
#' @seealso \code{\link{get_public_advisories}}
#' @export
public <- function(l, display_link = TRUE) {

  if(!.status(l))
    stop(sprintf("Link unavailable. %d", l))
  
  if(display_link)
    message(sprintf("Working %s", l))
  
  contents <- l %>% 
    xml2::read_html() %>% 
    rvest::html_text()

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