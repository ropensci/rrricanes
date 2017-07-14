#' @title get_prblty
#' @description Strike probabilities; the chances of the center of a cyclone
#' passing within 65 nautical miles of a location.
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane,
#'     etc.}
#'   \item{Name}{Name of storm}
#'   \item{Adv}{Advisory Number}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Location}{Location for which the probability statistics rely}
#'   \item{A}{Probability of a strike within the next 12 hours}
#'   \item{B}{Probability of a strike between 12 and 24 hours}
#'   \item{C}{Probability of a strike between 24 and 36 hours}
#'   \item{D}{Probability of a strike between 36 and 48 hours}
#'   \item{E}{Probability of a strike between 48 and 72 hours}
#' }
#' @param links URL to storm's archive page.
#' @export
get_prblty <- function(links) {
  df <- get_storm_data(links, products = "prblty")
  return(df$prblty)
}

#' @title prblty
#' @description Parse strike probability products
#' @details Given a direct link to a strike probability advisory product, parse
#' and return dataframe of values.
#' @param contents Link to a storm's specific strike probability advisory product.
#' @return Dataframe
#' @seealso \code{\link{get_prblty}}
#' @keywords internal
prblty <- function(contents) {
  # Replace all carriage returns with empty string.
  contents <- stringr::str_replace_all(contents, "\r", "")

  status <- scrape_header(contents, ret = "status")
  name <- scrape_header(contents, ret = "name")
  adv <- scrape_header(contents, ret = "adv")
  date <- scrape_header(contents, ret = "date")

  if (getOption("rrricanes.working_msg"))
    message(sprintf("Working %s %s Strike Probability #%s (%s)",
                    status, name, adv, date))

  # 15.0N  43.4W      43  1  X  X 44   16.8N  48.2W       X  4 16  2 22
  # 15.8N  45.9W       1 26  1  X 28

  ptn <- paste0("(?<=[:blank:]{3}|\n)",
                "([[:alpha:][:digit:][:punct:][:blank:]]{17})",   # Location
                "[:blank:]+",                                     # Delimiter
                "([:digit:]{1,2}|X)",                             # A
                "[:blank:]+",                                     # Delimiter
                "([:digit:]{1,2}|X)",                             # B
                "[:blank:]+",                                     # Delimiter
                "([:digit:]{1,2}|X)",                             # C
                "[:blank:]+",                                     # Delimiter
                "([:digit:]{1,2}|X)",                             # D
                "[:blank:]+",                                     # Delimiter
                "([:digit:]{1,2}|X)")                             # E

  matches <- stringr::str_match_all(contents, ptn)

  prblty <- tibble::as_data_frame(matches[[1]])

  names(prblty) <- c("Del", "Location", "A", "B", "C", "D", "E")

  prblty$Del <- NULL

  # Trim whitespace
  prblty <- purrr::map_df(.x = prblty, .f = stringr::str_trim)

  # If no strike probabilities, return NULL
  if (nrow(prblty) == 0)
    return(NULL)

  # Many values will have "X" for less than 1% chance. Make 0
  prblty[prblty == "X"] <- 0

  # dplyr 0.6.0 renames .cols parameter to .vars. For the time being,
  # accomodate usage of both 0.5.0 and >= 0.6.0.
  if (packageVersion("dplyr") > "0.5.0") {
    prblty <- dplyr::mutate_at(.tbl = prblty,
                               .vars = c(2:6),
                               .funs = "as.numeric")
  } else {
    prblty <- dplyr::mutate_at(.tbl = prblty,
                               .cols = c(2:6),
                               .funs = "as.numeric")
  }

  prblty <- prblty %>%
    dplyr::mutate("Status" = status,
                  "Name" = name,
                  "Adv" = adv,
                  "Date" = date) %>%
    dplyr::select_("Status", "Name", "Adv", "Date", "Location", "A", "B",
                   "C", "D", "E") %>%
    dplyr::arrange_("Date", "Adv")

  return(prblty)
}
