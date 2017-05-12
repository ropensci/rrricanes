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

#' @title create_df_wndprb
#' @description Template for wind probabilities dataframe
#' @return empty dataframe
#' @seealso \code{\link{get_wndprb}}
#' @keywords internal
create_df_wndprb <- function() {
    df <- tibble::data_frame("Status" = character(),
                             "Name" = character(),
                             # Allow for intermediate advisories, i.e., "1A", "2", "2A"...
                             "Adv" = character(),
                             "Date" = as.POSIXct(character(), tz = "UTC"),
                             "Contents" = character())

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
#' @source \link{http://www.nhc.noaa.gov/about/pdf/About_Windspeed_Probabilities.pdf}
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
#' @keywords internal
wndprb <- function(link, msg = FALSE) {

    contents <- scrape_contents(link, msg = msg)

    # Make sure this is a wndprb advisory product
    if (!any(stringr::str_count(contents, c("MIAPWSAT", "MIAPWSEP"))))
        stop(sprintf("Invalid Wind Probability link. %s", link))

    df <- create_df_wndprb()

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