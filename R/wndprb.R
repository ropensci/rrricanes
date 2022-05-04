#' @title al_prblty_stations
#' @description Retrieve list of probability stations based in the Atlantic
#' basin from the NHC. To be used in tandem with `wndprb` products.
#' @details Originally it was believed this data source would be removed by the
#' National Hurricane Center but it appears to have been updated. Additional
#' columns have been added, one up front and three in the back. These columns
#' all contain the same values each and I am unable to find documentation
#' describing the values.
#'
#' Regardless, the data is kept, just in case.
#'
#' @section Warnings:
#'
#' Calling \code{al_prblty_stations} will generate a warning:
#'
#' > "Expected 7 pieces. Additional pieces discarded in 1 rows [90]."
#'
#' Station PATRICK AFB actually has eight columns. The data is kept for
#' consistency; you decide if you want it or not.
#'
#' @export
al_prblty_stations <- function() {
  url <- sprintf(
    "%sdata/wsp/al_prblty_station.lst.csv.txt",
    get_nhc_link(protocol = "http")
  )
  parse_stations(url)
}

#' @title cp_prblty_stations
#' @description Retrieve list of probability stations based in the central
#' Pacific from the NHC. To be used in tandem with `wndprb` products.
#' @export
cp_prblty_stations <- function() {
  url <-
    sprintf(
      "%sdata/wsp/cp_prblty_station.lst.csv.txt",
      get_nhc_link(protocol = "http")
    )
  parse_stations(url)
}

#' @title ep_prblty_stations
#' @description Retrieve list of probability stations based in the eastern
#' Pacific from the NHC. To be used in tandem with `wndprb` products.
#' @details Originally it was believed this data source would be removed by the
#' National Hurricane Center but it appears to have been updated. Additional
#' columns have been added, one up front and three in the back. These columns
#' all contain the same values each and I am unable to find documentation
#' describing the values.
#'
#' Regardless, the data is kept, just in case.
#'
#' @section Warnings:
#'
#' Calling \code{ep_prblty_stations} will generate a warning:
#'
#' > "Expected 7 pieces. Missing pieces filled with `NA` in 1 rows [41]."
#'
#' Station SALINA CRUZ actually has six columns.
#'
#' @export
ep_prblty_stations <- function() {
  url <- sprintf(
    "%sdata/wsp/ep_prblty_station.lst.csv.txt",
    get_nhc_link(protocol = "http")
  )
  parse_stations(url)
}

#' @title get_wndprb
#' @description Return dataframe of wind speed probability data.
#' @details Wind Speed Probability product replaced Strike Probabilities product
#'   after the 2005 hurricane season. These products may not be issued for
#'   every advisory/cyclone.
#'
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane,
#'   etc.}
#'   \item{Name}{Name of storm}
#'   \item{Adv}{Advisory Number}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Wind}{Minimum wind speed for which probabilities reference}
#'   \item{Wind12}{Probability of sustained `Wind` within 12 hours}
#'   \item{Wind24}{Probability of sustained `Wind` within 24 hours}
#'   \item{Wind24Cum}{Cumulative probability through 24 hours}
#'   \item{Wind36}{Probability of sustained `Wind` within 36 hours}
#'   \item{Wind36Cum}{Cumulative probability through 36 hours}
#'   \item{Wind48}{Probability of sustained `Wind` within 48 hours}
#'   \item{Wind48Cum}{Cumulative probability through 48 hours}
#'   \item{Wind72}{Probability of sustained `Wind` within 72 hours}
#'   \item{Wind72Cum}{Cumulative probability through 72 hours}
#'   \item{Wind96}{Probability of sustained `Wind` within 96 hours}
#'   \item{Wind96Cum}{Cumulative probability through 96 hours}
#'   \item{Wind120}{Probability of sustained `Wind` within 120 hours}
#'   \item{Wind120Cum}{Cumulative probability through 120 hours}
#' }
#' @param links URL to storm's archive page.
#' @source \url{http://www.nhc.noaa.gov/about/pdf/About_Windspeed_Probabilities.pdf}
#' @export
get_wndprb <- function(links) {
  get_product(links = links, product = "wndprb")
}

#' @title parse_stations
#' @description Parse probability station listings for each basin.
#' @details At the moment, documentation on the format is unavailable. The
#' format changed during the 2017/2018 offseason and now includes a
#' numeric first column and numeric fifth, sixth and seventh column. All
#' values are identical per column.
#'
#' Additionally, as of publication, PATRICK AFB in the Atlantic data source
#' actually contains eight columns; this is noted in
#' \code{\link{al_prblty_stations}}. SALINA CRUZ in
#' \code{\link{ep_prblty_stations}} is short one column.
#'
#' I see no issues with the extra or missing data as I am unsure the value of
#' the data to begin with. A warning will be given so the user is aware,
#' but the important pieces (Location, Lat, Lon) all seem good.
#' @param x URL of station list
#' @keywords internal
parse_stations <- function(x) {
  df <- readLines(x) |>
    tibble::as_tibble() |>
    tidyr::separate(.data$value,
                    c("X1", "Location", "Lat", "Lon", "X5", "X6", "X7"),
                    sep = ",",
                    extra = "warn") |>
    dplyr::arrange(.data$Location)
  df
}

#' @title wndprb
#' @description Parse wind probability products
#' @details Given a direct link to a wind probability product, parse and return
#' dataframe of values.
#' @param contents Link to a storm's specific wind probability product.
#' @keywords internal
wndprb <- function(contents) {

  status <- scrape_header(
    contents = contents,
    # The "SPECIAL" pattern has to be left here; moving it under
    # `scrape_header` will break posest and update products.
    ptn_product_title = "(?:\n?SPECIAL )?(?:WIND SPEED PROBABILITIES)?"
  )

  issue_date <- scrape_date(contents)

  key <- scrape_key(contents)

  ptn <- stringr::str_c("(?<=\n)", # Look-behind
                        # Location - first value must be capital letter.
                        "([:upper:]{1}[[:alnum:][:blank:][:punct:]]{14})",
                        # Wind
                        "([[:digit:]]{2})",
                        # Wind12
                        "[:blank:]+([:digit:]{1,2}|X)",
                        # Delim
                        "[:blank:]+",
                        # Wind24
                        "([:digit:]{1,2}|X)",
                        # Wind24 cumulative
                        "+\\([:blank:]*([:digit:]{1,2}|X)\\)",
                        # Delim
                        "[:blank:]+",
                        # Wind36
                        "([:digit:]{1,2}|X)",
                        # Wind36 cumulative
                        "+\\([:blank:]*([:digit:]{1,2}|X)\\)",
                        # Delim
                        "[:blank:]+",
                        # Wind48
                        "([:digit:]{1,2}|X)",
                        # Wind48 cumulative
                        "+\\([:blank:]*([:digit:]{1,2}|X)\\)",
                        # Delim
                        "[:blank:]+",
                        # Wind72
                        "([:digit:]{1,2}|X)",
                        # Wind72 cumulative
                        "+\\([:blank:]*([:digit:]{1,2}|X)\\)",
                        # Delim
                        "[:blank:]+",
                        # Wind96
                        "([:digit:]{1,2}|X)",
                        # Wind96 cumulative
                        "+\\([:blank:]*([:digit:]{1,2}|X)\\)",
                        # Delim
                        "[:blank:]+",
                        # Wind120
                        "([:digit:]{1,2}|X)",
                        # Wind120 cumulative
                        "+\\([:blank:]*([:digit:]{1,2}|X)\\)",
                        # End
                        "[[:blank:]\n]+")

  wndprb <-
    contents |>
    stringr::str_match_all(ptn) |>
    purrr::map(tibble::as_tibble, .name_repair = "minimal") |>
    purrr::map(
      .f = rlang::set_names,
      nm = c("X1", "Location", "Wind", "Wind12", "Wind24", "Wind24Cum",
             "Wind36",  "Wind36Cum", "Wind48", "Wind48Cum", "Wind72",
             "Wind72Cum", "Wind96", "Wind96Cum", "Wind120", "Wind120Cum")

    ) |>
    purrr::map2(key, ~tibble::add_column(.x, StormKey = .y, .before = 1)) |>
    purrr::map2(status[,3], ~tibble::add_column(.x, Adv = .y, .after = 2)) |>
    purrr::map2(issue_date, ~tibble::add_column(.x, Date = .y, .after = 3)) |>
    purrr::map_df(tibble::as_tibble, .name_repair = "minimal") |>
    dplyr::select(-c("X1")) |>

    # Trim whitespace
    dplyr::mutate_all(.funs = stringr::str_trim)

  # Make "X" values 0
  wndprb[wndprb == "X"] <- "0"

  wndprb <- dplyr::mutate_at(
    .tbl = wndprb,
    .vars = "Date",
    .funs = lubridate::ymd_hms
  )

  # Make Wind:Wind120Cum numeric
  wndprb <- dplyr::mutate_at(
    .tbl = wndprb,
    .vars = c(2, 5:18),
    .funs = "as.numeric"
  )

}
