#' @title al_prblty_stations
#' @description Retrieve list of probability stations based in the Atlantic
#' basin from the NHC. To be used in tandem with `wndprb` products.
#' @details This function may be deprecated soon as the data sources may be
#' removed from the National Hurricane Center.
#' @export
al_prblty_stations <- function() {
    url <- "http://www.nhc.noaa.gov/data/wsp/al_prblty_station.lst.csv.txt"
    df <- readr::read_csv(url,
                          col_names = c("Location", "Lat", "Lon"),
                          col_types = readr::cols(Location = "c",
                                                  Lat = "n",
                                                  Lon = "n")) %>%
        dplyr::arrange_("Location")
    return(df)
}

#' @title cp_prblty_stations
#' @description Retrieve list of probability stations based in the central
#' Pacific from the NHC. To be used in tandem with `wndprb` products.
#' @details This function may be deprecated soon as the data sources may be
#' removed from the National Hurricane Center.
#' @export
cp_prblty_stations <- function() {
    url <- "http://www.nhc.noaa.gov/data/wsp/cp_prblty_station.lst.csv.txt"
    df <- readr::read_csv(url,
                          col_names = c("Location", "Lat", "Lon"),
                          col_types = readr::cols(Location = "c",
                                                  Lat = "n",
                                                  Lon = "n")) %>%
        dplyr::arrange_("Location")
    return(df)
}

#' @title ep_prblty_stations
#' @description Retrieve list of probability stations based in the eastern
#' Pacific from the NHC. To be used in tandem with `wndprb` products.
#' @details This is a placeholder function. The current listing does not match
#' the format for Atlantic and central Pacific stations.
#'
#' This function may be deprecated soon as the data sources may be
#' removed from the National Hurricane Center.
#' @export
ep_prblty_stations <- function() {
    url <- "http://www.nhc.noaa.gov/data/wsp/ep_prblty_station.lst.csv.txt"
    return(FALSE)
}

#' @title get_wndprb
#' @description Return dataframe of wind speed probability data.
#' @details Wind Speed Probability product replaced Strike Probabilities product
#'     after the 2005 hurricane season. These products may not be issued for
#'     every advisory/cyclone.
#'
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane,
#'     etc.}
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
#' @param link URL to storm's archive page.
#' @source \url{http://www.nhc.noaa.gov/about/pdf/About_Windspeed_Probabilities.pdf}
#' @export
get_wndprb <- function(link) {

    # Get all products for the current storm
    products <- purrr::map(link, get_products) %>% purrr::flatten_chr()

    # Filter out wndprb products
    products <- filter_wndprb(products)

    # Set progress bar
    p <- dplyr::progress_estimated(n = length(products))

    products.wndprb <- purrr::map(products, wndprb, p)

    wndprb <- purrr::map_df(products.wndprb, dplyr::bind_rows)

    return(wndprb)
}

#' @title wndprb
#' @description Parse wind probability products
#' @details Given a direct link to a wind probability product, parse and return
#' dataframe of values.
#' @param link Link to a storm's specific wind probability product.
#' @param p dplyr::progress_estimate.
#' @keywords internal
wndprb <- function(link, p = dplyr::progress_estimated(n = 1)) {

    p$pause(0.5)$tick()$print()

    contents <- scrape_contents(link)

    # Replace all carriage returns with empty string.
    contents <- stringr::str_replace_all(contents, "\r", "")

    # Make sure this is a wndprb advisory product
    if (!any(stringr::str_count(contents, c("MIAPWS", "PWS"))))
        stop(sprintf("Invalid Wind Probability link. %s", link))

    status <- scrape_header(contents, ret = "status")
    key <- scrape_header(contents, ret = "key")
    adv <- scrape_header(contents, ret = "adv")
    date <- scrape_header(contents, ret = "date")
    name <- scrape_header(contents, ret = "name")

    if (getOption("rrricanes.working_msg"))
        message(sprintf("Working %s %s Wind Speed Probability #%s (%s)",
                        status, name, adv, date))

    ptn <- paste0("(?<=\n)", # Look-behind
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

    matches <- stringr::str_match_all(contents, pattern = ptn)

    # Load matches into dataframe
    wndprb <- tibble::as_data_frame(matches[[1]][,2:16])

    # If only one row, need to transpose wndprb
    if (ncol(wndprb) == 1)
        wndprb <- wndprb %>% t() %>% tibble::as_data_frame()

    # If no wnd speed probabilities, return NULL
    if (nrow(wndprb) == 0)
        return(NULL)

    # Rename variables
    names(wndprb) <- c("Location", "Wind", "Wind12", "Wind24", "Wind24Cum",
                       "Wind36", "Wind36Cum", "Wind48", "Wind48Cum", "Wind72",
                       "Wind72Cum", "Wind96", "Wind96Cum", "Wind120",
                       "Wind120Cum")

    # Trim whitespace
    wndprb <- purrr::map_df(.x = wndprb, .f = stringr::str_trim)

    # Make "X" values 0
    wndprb[wndprb == "X"] <- 0

    # Make Wind:Wind120Cum numeric
    # dplyr 0.6.0 renames .cols parameter to .vars. For the time being,
    # accomodate usage of both 0.5.0 and >= 0.6.0.
    if (packageVersion("dplyr") > "0.5.0") {
        wndprb <- dplyr::mutate_at(.tbl = wndprb,
                                   .vars = c(2:15),
                                   .funs = "as.numeric")
    } else {
        wndprb <- dplyr::mutate_at(.tbl = wndprb,
                                   .cols = c(2:15),
                                   .funs = "as.numeric")
    }

    # Add Key, Adv, Date and rearrange.
    wndprb <- wndprb %>%
        dplyr::mutate("Key" = key,
               "Adv" = adv,
               "Date" = date) %>%
        dplyr::select_("Key:Date", "Location:Wind120Cum") %>%
        dplyr::arrange_("Key", "Date", "Adv")

    return(wndprb)
}
