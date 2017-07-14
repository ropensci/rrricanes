#' @title create_df_update
#' @description Template for cyclone update dataframe
#' @return empty dataframe
#' @seealso \code{\link{get_update}}
#' @keywords internal
create_df_update <- function() {
    df <- tibble::data_frame("Status" = character(),
                             "Name" = character(),
                             "Date" = as.POSIXct(character(), tz = "UTC"),
                             "Key" = character(),
                             "Contents" = character())

    return(df)
}

#' @title get_update
#' @description Return dataframe of cyclone update data.
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane,
#'     etc.}
#'   \item{Name}{Name of storm}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Key}{Unique ID of cyclone}
#'   \item{Contents}{Text content of product}
#' }
#' @param links URL to storm's archive page.
#' @seealso \code{\link{get_storms}}, \code{\link{update}}
#' @export
get_update <- function(links) {
  df <- get_storm_data(links, products = "update")
  return(df$update)
}

#' @title update
#' @description Parse cyclone update products
#' @details Given a direct link to a cyclone update product, parse and return
#' dataframe of values.
#' @param contents Link to a storm's specific update advisory product.
#' @return Dataframe
#' @seealso \code{\link{get_update}}
#' @keywords internal
update <- function(contents) {

  # Replace all carriage returns with empty string.
  contents <- stringr::str_replace_all(contents, "\r", "")

  df <- create_df_update()

  status <- scrape_header(contents, ret = "status")
  name <- scrape_header(contents, ret = "name")
  date <- scrape_header(contents, ret = "date")

  safely_scrape_header <- purrr::safely(scrape_header)
  key <- safely_scrape_header(contents, ret = "key")
  if (is.null(key$error)) {
    key <- key$result
  } else {
    key <- NA
  }

  if (getOption("rrricanes.working_msg"))
    message(sprintf("Working %s %s Update #%s (%s)",
                    status, name, date))

  df <- df %>%
    tibble::add_row("Status" = status,
                    "Name" = name,
                    "Date" = date,
                    "Key" = key,
                    "Contents" = contents)

  return(df)
}
