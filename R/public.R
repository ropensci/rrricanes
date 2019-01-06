#' @title get_public
#' @description Return dataframe of public advisory data.
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane,
#'   etc.}
#'   \item{Name}{Name of storm}
#'   \item{Adv}{Advisory Number}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Key}{Unique ID of the cyclone}
#'   \item{Contents}{Text content of product}
#' }
#' @param links URL to storm's archive page.
#' @seealso \code{\link{get_storms}}, \code{\link{public}}
#' @export
get_public <- function(links) {
  df <- get_storm_data(links, products = "public")
  return(df$public)
}

#' @title public
#' @description Parse Public Advisory products
#' @details Given a direct link to a public advisory product, parse and return
#' dataframe of values.
#' @param contents Link to a storm's specific public advisory product.
#' @return Dataframe
#' @seealso \code{\link{get_public}}
#' @keywords internal
public <- function(contents) {

  # Replace all carriage returns with empty string.
  contents <- stringr::str_replace_all(contents, "\r", "")

  status <- scrape_header(contents, ret = "status")
  name <- scrape_header(contents, ret = "name")
  adv <- scrape_header(contents, ret = "adv")
  date <- scrape_header(contents, ret = "date")

  safely_scrape_header <- purrr::safely(scrape_header)
  key <- safely_scrape_header(contents, ret = "key")
  if (is.null(key$error)) {
    key <- key$result
  } else {
    key <- NA
  }

  if (getOption("rrricanes.working_msg"))
    message(sprintf("Working %s %s Public Advisory #%s (%s)",
                    status, name, adv, date))

  df <- df %>%
    tibble::add_row("Status" = status,
                    "Name" = name,
                    "Adv" = adv,
                    "Date" = date,
                    "Key" = key,
                    "Contents" = contents)

  return(df)
}
