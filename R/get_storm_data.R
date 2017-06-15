#' @title get_products
#' @description Get list of all products from a storm's archive page
#' @param link URL to storm's archive page.
#' @keywords internal
get_products <- function(link) {

    # Get year
    year <- extract_year_archive_link(link)

    products <- get_storm_content(link) %>%
        rvest::html_attr("href") %>%
        stats::na.omit()

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
    contents <- get_url_contents(link) %>% rvest::html_nodes(xpath = "//td//a")
    return(contents)
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

    products <- match.arg(products, several.ok = TRUE)

    ds <- purrr::map(products, .f = function(x) {
        sprintf("get_%s", x) %>%
            purrr::invoke_map(.x = list(link = link)) %>%
            purrr::flatten_df()})
    names(ds) <- products
    return(ds)
}

#' @title load_storm_data
#' @description Load storm and year data from data repository.
#' @param dataset A dataset to return
#' @details This function is designed to give quicker access to post-scraped
#' storm data and may be modified in future releases.
#' \describe{
#'     \item{adv}{This data file contains base information for every storm
#'     advisory issued by the National Hurricane Center for the Atlantic and
#'     northeast Pacific oceans.}
#'     \item{discus}{Storm discussion text}
#'     \item{fcst}{Forecast positions of tropical cyclones}
#'     \item{fcst_wr}{Forecast wind radii data for each forecast observation in
#'     `fcst`}
#'     \item{posest}{Position estimates}
#'     \item{prblty}{Strike probabilities for a given location. This product was
#'     deprecated after the 2005 hurricane season.}
#'     \item{public}{Public advisory text}
#'     \item{storms}{Summary for all storms in this dataset.}
#'     \item{update}{Update text}
#'     \item{wndprb}{Wind Speed Probabilities. Probability of a location
#'     experiencing minimum wind-speed values within a given forecast period.
#'     This product replaces Strike Probabilities after the 2005 hurricane
#'     season. May not exist for all cyclones.}
#'     \item{wr}{Current wind radius, if available, for a cyclone.}
#' }
#'
#' Datasets 'discus', 'posest', 'public' and 'update' are not included as of
#' this writing but will be added as soon as possible.
#'
#' 'adv', 'fcst', 'fcst_wr', and 'wr' are tidied data of
#' \code{\link{get_fstadv}} using functions \code{\link{tidy_fcst}},
#' \code{\link{tidy_fcst_wr}}, \code{\link{tidy_fstadv}} and
#' \code{\link{tidy_wr}}.
#'
#' See \url{https://timtrice.github.io/rrricanes/articles/articles/data_world.html}
#' for a third alternative to access datasets with the ability to filter data
#' without loading entire datasets into your environment.
#' @seealso \url{https://timtrice.github.io/rrricanes/articles/articles/data_world.html}
#' @export
load_storm_data <- function(dataset = c("adv", "discus", "fcst", "fcst_wr",
                                        "posest", "prblty", "public", "storms",
                                        "update", "wndprb", "wr")) {
    dataset <- match.arg(dataset)
    base_url <- "https://github.com/timtrice/rrricanesdata/blob/master/"
    link <- paste0(base_url, dataset, ".csv?raw=true")
    df <- readr::read_csv(link)
    return(df)
}
