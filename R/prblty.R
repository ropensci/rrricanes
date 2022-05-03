#' @title get_prblty
#' @description Strike probabilities; the chances of the center of a cyclone
#' passing within 65 nautical miles of a location.
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane,
#'   etc.}
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
  get_product(links = links, product = "prblty")
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

  status <- scrape_header(
    contents = contents,
    # The "SPECIAL" pattern has to be left here; moving it under
    # `scrape_header` will break posest and update products.
    ptn_product_title = "(?:\n?SPECIAL\\s+)?(?:PROBABILITIES)?"
  )

  issue_date <- scrape_date(contents)

  # 15.0N  43.4W    43  1  X  X 44   16.8N  48.2W     X  4 16  2 22
  # 15.8N  45.9W     1 26  1  X 28

  ptn <- stringr::str_c("(?<=[:blank:]{3}|\n)",
                        "([[:alpha:][:digit:][:punct:][:blank:]]{17})",   # Location
                        "[:blank:]+",                   # Delimiter
                        "([:digit:]{1,2}|X)",               # A
                        "[:blank:]+",                   # Delimiter
                        "([:digit:]{1,2}|X)",               # B
                        "[:blank:]+",                   # Delimiter
                        "([:digit:]{1,2}|X)",               # C
                        "[:blank:]+",                   # Delimiter
                        "([:digit:]{1,2}|X)",               # D
                        "[:blank:]+",                   # Delimiter
                        "([:digit:]{1,2}|X)")               # E

  prblty <-
    contents |>
    stringr::str_match_all(ptn) |>
    purrr::map(tibble::as_tibble, .name_repair = "minimal") |>
    purrr::map(
      rlang::set_names, nm = c("X1", "Location", "A", "B", "C", "D", "E")
    ) |>
    purrr::map2(status[,1], ~tibble::add_column(.x, Status = .y, .before = 1)) |>
    purrr::map2(status[,2], ~tibble::add_column(.x, Name = .y, .after = 1)) |>
    purrr::map2(status[,3], ~tibble::add_column(.x, Adv = .y, .after = 2)) |>
    purrr::map2(issue_date, ~tibble::add_column(.x, Date = .y, .after = 3)) |>
    purrr::map_df(tibble::as_tibble, .name_repair = "minimal") |>
    dplyr::select(-c("X1")) |>
    # Trim whitespace
    dplyr::mutate_all(.funs = stringr::str_trim)

  # Many values will have "X" for less than 1% chance. Make 0
  prblty[prblty == "X"] <- "0"

  # Convert date
  prblty <- dplyr::mutate_at(
    .tbl = prblty,
    .vars = "Date",
    .funs = lubridate::ymd_hms
  )

  prblty <- dplyr::mutate_at(
    .tbl = prblty,
    .vars = c(6:10),
    .funs = "as.numeric"
  )

}
