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
    return(xml2::read_html(link) %>% rvest::html_nodes(xpath = "//td//a"))
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
#' @details This function is designed to give quicker access to storm data.
#' Using any of the product functions scrapes data from the NHC archives.
#' However, this can be somewhat time-consuming. Using this function just takes
#' a different route to datasets that have already been scraped. So obtaining
#' data is significantly faster.
#'
#' I will not guarantee current storms will be up-to-date. But I will work on a
#' setup that does so as quickly as possible.
#'
#' @param years Numeric vector of one or multiple years between 1998 and current
#' year.
#' @param basins One or both of AL and EP.
#' @param products NULL, one or many of discus, fstadv, posest, prblty, public, update
#' and wndprb. See \link{get_storm_data} for a description of each of the
#' products. If products is NULL then a summary table of the year's storms is
#' returned. If a product is requested but not returned then the product does
#' not exist. For example, prblty does not exist for storms after 2005. And
#' short-lived storms may not have wndprb products.
#' @examples
#' \dontrun{
#' # Load year summary data for 2017, both basins
#' load_storm_data(years = 2017)
#'
#' # Load multiple years, forecast/advisory data for AL storms.
#' load_storm_data(years = 2015:2016, basins = "AL", products = "fstadv")
#'
#' # Load fstadv and wndprb for 2015 Atlantic storms.
#' load_storm_data(years = 2015, basins = "AL", products = c("fstadv", "wndprb"))
#' }
#' @export
load_storm_data <- function(years, basins = c("AL", "EP"), products = NULL) {

    if (!(all(dplyr::between(years, 1998, as.numeric(strftime(Sys.Date(), "%Y"))))))
        stop("years must be between 1998 and current year")

    if (!(all(basins %in% c("AL", "EP"))))
        stop("basins must be one or both of AL, EP")

    if (!is.null(products) & !(all(products %in% c("discus", "fstadv", "posest",
                                               "prblty", "public", "update",
                                               "wndprb"))))
        stop("products must either be NULL or have at least one valid product")

    base_url <- "https://github.com/timtrice/rrricanesdata/blob/master/"
    purrr::map(.x = years, .f = function(year) {
        purrr::map(.x = basins, .f = function(basin) {
            if (purrr::is_empty(products)) {
                # Load year summary data
                data_url <- sprintf("%s/%s/%s%s.Rda", year, basin, basin, year)
                link <- paste0(base_url, data_url, "?raw=true")
                if (!httr::http_error(link))
                    load(url(link), envir = .GlobalEnv)
            } else {
                purrr::map(.x = products, .f = function(product) {
                    data_url <- sprintf("%s/%s/%s%s_%s.Rda", year, basin,
                                        basin, year, product)
                    link <- paste0(base_url, data_url, "?raw=true")
                    if (!httr::http_error(link))
                        load(url(link), envir = .GlobalEnv)
                })
            }
        })
    })
    return(TRUE)
}
