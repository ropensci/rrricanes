#' @title al_prblty_stations
#' @description Retrieve list of probability stations based in the Atlantic
#'     basin from the NHC. To be used in tandem with `wndprb` products.
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
#'     Pacific from the NHC. To be used in tandem with `wndprb` products.
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
#'     Pacific from the NHC. To be used in tandem with `wndprb` products.
#' @details This is a placeholder function. The current listing does not match
#'     the format for Atlantic and central Pacific stations.
#' @keywords internal
ep_prblty_stations <- function() {
    url <- "http://www.nhc.noaa.gov/data/wsp/ep_prblty_station.lst.csv.txt"
    return(FALSE)
}

#' @title get_wndprb
#' @description Return dataframe of wind probability data.
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane,
#'     etc.}
#'   \item{Name}{Name of storm}
#'   \item{Adv}{Advisory Number}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Contents}{Text content of product}
#' }
#' @param link URL to storm's archive page.
#' @param msg Show link being worked. Default, FALSE.
#' @seealso \code{\link{get_storms}}, \code{\link{wndprb}},
#'     \code{\link{al_prblty_stations}}, \code{\link{ep_prblty_stations}},
#'     \code{\link{cp_prblty_stations}}
#' @source \url{http://www.nhc.noaa.gov/about/pdf/About_Windspeed_Probabilities.pdf}
#' @export
get_wndprb <- function(link, msg = FALSE) {

    # Check status of link(s)
    valid.link <- sapply(link, status)
    valid.link <- na.omit(valid.link)
    if (length(valid.link) == 0)
        stop("No valid links.")

    products <- purrr::map(valid.link, get_products) %>% purrr::flatten_chr()

    products.wndprb <- purrr::map(filter_wndprb(products), wndprb)

    wndprb <- purrr::map_df(products.wndprb, dplyr::bind_rows)

    return(wndprb)
}

#' @title wndprb
#' @description Parse wind probability products
#' @details Given a direct link to a wind probability product, parse and return
#' dataframe of values.
#' @param link Link to a storm's specific wind probability product.
#' @param msg Display each link as being worked; default is FALSE
#' @return Dataframe
#' @seealso \code{\link{get_wndprb}}
#' @export
wndprb <- function(link, msg = FALSE) {

    contents <- scrape_contents(link, msg = msg)

    # Make sure this is a wndprb advisory product
    if (!any(stringr::str_count(contents, c("MIAPWSAT", "MIAPWSEP"))))
        stop(sprintf("Invalid Wind Probability link. %s", link))

    key <- scrape_header(contents, ret = "key")
    adv <- scrape_header(contents, ret = "adv")
    date <- scrape_header(contents, ret = "date")

    ## ---- * Wind Speed Probabilities for Selected Locations ------------------

    ptn <- paste0("(?<=\n)", # Look-behind
                  # Location - first value must be capital letter.
                  "([:upper:]{1}[[:alnum:][:blank:]]{14})",
                  # Wind
                  "([[:digit:]]{2})",
                  # Wind12
                  "[:blank:]+([:digit:]{1,2}|X)",
                  # Delim
                  "[:blank:]+",
                  # Wind24
                  "([:digit:]{1,2}|X)",
                  # Wind24 cumulative
                  "+\\(([:blank:][:digit:]{1,2}|[:blank:]X)\\)",
                  # Delim
                  "[:blank:]+",
                  # Wind36
                  "([:digit:]{1,2}|X)",
                  # Wind36 cumulative
                  "+\\(([:blank:][:digit:]{1,2}|[:blank:]X)\\)",
                  # Delim
                  "[:blank:]+",
                  # Wind48
                  "([:digit:]{1,2}|X)",
                  # Wind48 cumulative
                  "+\\(([:blank:][:digit:]{1,2}|[:blank:]X)\\)",
                  # Delim
                  "[:blank:]+",
                  # Wind72
                  "([:digit:]{1,2}|X)",
                  # Wind72 cumulative
                  "+\\(([:blank:][:digit:]{1,2}|[:blank:]X)\\)",
                  # Delim
                  "[:blank:]+",
                  # Wind96
                  "([:digit:]{1,2}|X)",
                  # Wind96 cumulative
                  "+\\(([:blank:][:digit:]{1,2}|[:blank:]X)\\)",
                  # Delim
                  "[:blank:]+",
                  # Wind120
                  "([:digit:]{1,2}|X)",
                  # Wind120 cumulative
                  "+\\(([:blank:][:digit:]{1,2}|[:blank:]X)\\)",
                  # End
                  "[[:blank:]\n]+")

    matches <- stringr::str_match_all(contents, pattern = ptn)

    # Load matches into dataframe
    wndprb <- tibble::as_data_frame(matches[[1]][,2:16])

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
    wndprb <- dplyr::mutate_at(.tbl = wndprb,
                               .cols = c(2:15),
                               .funs = "as.numeric")

    # Add Key, Adv, Date and rearrange.
    wndprb <- wndprb %>%
        dplyr::mutate("Key" = key,
               "Adv" = adv,
               "Date" = date) %>%
        dplyr::select_("Key:Date", "Location:Wind120Cum") %>%
        dplyr::arrange_("Key", "Date", "Adv")

    return(wndprb)
}