#' @title create_df_discus
#' @description Template for storm discussions dataframe
#' @return empty dataframe
#' @seealso \code{\link{get_discus}}
#' @keywords internal
create_df_discus <- function() {
  df <- tibble::data_frame("Status" = character(),
               "Name" = character(),
               # Allow for intermediate advisories,
               # i.e., "1A", "2", "2A"...
               "Adv" = integer(),
               "Date" = as.POSIXct(character(), tz = "UTC"),
               "Key" = character(),
               "Contents" = character())

  return(df)
}

#' @title get_discus
#' @description Return dataframe of discussion data.
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane,
#'   etc.}
#'   \item{Name}{Name of storm}
#'   \item{Adv}{Advisory Number}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Key}{ID of cyclone}
#'   \item{Contents}{Text content of product}
#' }
#' @param links URL to storm's archive page.
#' @seealso \code{\link{get_storms}}, \code{\link{public}}
#' @examples
#' \dontrun{
#' # Return dataframe of storm discussions for Tropical Storm Alex (AL011998)
#' get_discus("http://www.nhc.noaa.gov/archive/1998/1998ALEXadv.html")
#' }
#' @export
get_discus <- function(links) {
  df <- get_storm_data(links, products = "discus")
  return(df$discus)
}

#' @title discus
#' @description Parse storm Discussion products
#' @details Given a direct link to a discussion product, parse and return
#' dataframe of values.
#' @param contents Link to a storm's specific discussion product.
#' @return Dataframe
#' @seealso \code{\link{get_discus}}
#' @keywords internal
discus <- function(contents) {

  # Replace all carriage returns with empty string.
  contents <- stringr::str_replace_all(contents, "\r", "")

  df <- create_df_discus()

  status <- scrape_header(contents, ret = "status")
  name <- scrape_header(contents, ret = "name")
  adv <- scrape_header(contents, ret = "adv") %>% as.numeric()
  date <- scrape_header(contents, ret = "date")

  # Keys were added to discus products beginning 2006. Prior, it doesn't
  # exist. safely run scrape_header for key. If error, then use NA. Otherwise,
  # add it.
  safely_scrape_header <- purrr::safely(scrape_header)
  key <- safely_scrape_header(contents, ret = "key")
  if (is.null(key$error)) {
  key <- key$result
  } else {
  key <- NA
  }

  if (getOption("rrricanes.working_msg"))
  message(sprintf("Working %s %s Storm Discussion #%s (%s)",
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
