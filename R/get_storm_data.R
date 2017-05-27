#' @title get_products
#' @description Get list of all products from a storm's archive page
#' @param link URL to storm's archive page.
#' @export
get_products <- function(link) {

    # Get year
    year <- extract_year_archive_link(link)

    products <- get_storm_content(link) %>%
        rvest::html_attr("href") %>%
        na.omit()

    nhc_url <- get_nhc_link(withTrailingSlash = FALSE)

    # Depending on the year these URL's are formatted various ways.
    if (year == 1998) {
        products <- stringr::str_c(nhc_url, '/archive/', year, '/', products)
    } else {
        products <- stringr::str_c(nhc_url, products)
    }

    return(products)
}

#' @title get_storm_content
#' @description Extract content from a storm's archive page
#' @param link of archive page
#' @return page content of archive page
#' @keywords internal
get_storm_content <- function(link) {

    # Get year
    year <- extract_year_archive_link(link)

    # There are different layouts for various years. Have to work through it...
    if (year <= 2001) {
        page <- xml2::read_html(link) %>%
            rvest::html_nodes('body') %>%
            rvest::html_nodes('table') %>%
            rvest::html_nodes('td') %>%
            rvest::html_children()

    } else {
        page <- xml2::read_html(link) %>%
            rvest::html_nodes('.center') %>%
            rvest::html_nodes('.content') %>%
            rvest::html_nodes('table') %>%
            rvest::html_nodes('td') %>%
            rvest::html_children()
    }

    return(page)
}

#' @title get_storm_data
#' @description Retrieve data from products.
#' @details \code{get_storm_data} is a wrapper function to make it more
#'     convenient to access the various storm products.
#'
#' Types of products:
#' \describe{
#'   \item{discus}{Storm Discussions. This is technical information on the
#'     cyclone such as satellite presentation, forecast model evaluation, etc.}
#'   \item{fstadv}{Forecast/Advisory. These products contain the meat of an
#'     advisory package. Current storm information is available as well as
#'     structural design and forecast data.}
#'   \item{posest}{Position Estimate. Issued generally when a storm is
#'     threatening; provides a brief update on location and winds.}
#'   \item{public}{Public Advisory. Issued for public knowledge; more often for
#'     Atlantic than East Pacific storms. Contains general information.}
#'   \item{prblty}{Strike Probability. Discontinued after the 2005 hurricane
#'     season, strike probabilities list the chances of x-force winds in a
#'     particular city.}
#'   \item{update}{Cyclone Update. Generally issued when a significant change
#'     occurs in the cyclone.}
#'   \item{windprb}{Wind Probability. Replace strike probabilities beginning in
#'     the 2006 season. Nearly identical.}
#' }
#'
#' Progress bars are displayed by default. These can be turned off by setting
#' the dplyr.show_progress to FALSE. Additionally, you can display messages for
#' each advisory being worked by setting the rrricanes.working_msg to TRUE.
#'
#' @param link to storm's archive page.
#' @param products Products to retrieve; discus, fstadv, posest, public,
#'     prblty, update, and windprb.
#' @return list of dataframes for each of the products.
#' @examples
#' \dontrun{
#' ## Get public advisories for first storm of 2016 Atlantic season.
#' get_storms(year = 2016, basin = "AL") %>%
#'     slice(1) %>%
#'     .$Link %>%
#'     get_storm_data(products = "public")
#' ## Get public advisories and storm discussions for first storm of 2017 Atlantic season.
#' get_storms(year = 2017, basin = "AL") %>%
#'     slice(1) %>%
#'     .$Link %>%
#'     get_storm_data(products = c("discus", "public"))
#' }
#' @export
get_storm_data <- function(link, products = c("discus", "fstadv", "posest",
                                              "public", "prblty", "update",
                                              "wndprb")) {

    if (!(all(products %in% c("discus", "fstadv", "posest", "public", "prblty",
                              "update", "wndprb"))) | length(products) == 0)
        stop(paste0("Invalid products included. Only discus, fstadv, posest, ",
                    "public, prblty, update, wndprb are valid options.",
                    "See ?get_storm_data for more info."))

    if (is.null(link))
        stop("No link provided.")

    ds <- purrr::map(products, .f = function(x) {
        sprintf("get_%s", x) %>%
            purrr::invoke_map(.x = list(link = link)) %>%
            purrr::flatten_df()})
    return(ds)
}
