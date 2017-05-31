#' @title gis_advisory
#' @description Advisory Forecast Track, Cone of Uncertainty, and Watches/Warnings
#' @param key Key of storm (i.e., AL012008, EP092015)
#' @param advisory Advisory number. If NULL, all advisories are returned. Intermediate
#' advisories are acceptable.
#' @export
gis_advisory <- function(key, advisory = as.character()) {

    if (is.null(key))
        stop("Please provide storm key")

    key <- stringr::str_to_lower(key)

    if (!grepl("^[[:lower:]]{2}[[:digit:]]{6}$", key))
        stop("Invalid key")

    key <- stringr::str_match(key, pattern = "([:lower:]{2})([:digit:]{2})([:digit:]{4})")
    names(key) <- c("original", "basin", "year_num", "year")

    # Get list of GIS forecast zips for storm and download
    url <- sprintf("http://www.nhc.noaa.gov/gis/archive_forecast_results.php?id=%s%s&year=%s",
                   key[["basin"]], key[["year_num"]], key[["year"]])
    contents <- readr::read_lines(url)

    # Match zip files. If advisory is empty then need to pull all zip files for
    # the storm. Otherwise, pull only selected advisory.
    if (purrr::is_empty(advisory)) {
        ptn <- sprintf(".+(forecast/archive/%s.*?\\.zip).+", stringr::str_to_lower(key[["original"]]))
    } else {
        advisory <- stringr::str_match(advisory, "([:digit:]{1,3})([:alpha:]*)")
        names(advisory) <- c("original", "advisory", "int_adv")
        ptn <- sprintf(".+(forecast/archive/%s.*?%s%s\\.zip).+", stringr::str_to_lower(key["original"]), stringr::str_pad(string = advisory[["advisory"]], width = 3, side = "left", pad = "0"), advisory[["int_adv"]])
    }

    matches <- contents[stringr::str_detect(contents, pattern = ptn)]
    # Extract link to zip files. Error gracefully if no matches.
    tryCatch(links <- stringr::str_match(matches, pattern = ptn)[,2],
             error = function(c) {
                 c$message <- "No data avaialable for requested storm/advisory"
                 stop(c$message, call. = FALSE)
             })
    # Create sub directories for each zip file
    subdirs <- stringr::str_match(links, pattern = "forecast/archive/(.+)\\.zip")[,2]
    return(subdirs)
}

#' @title gis_outlook
#' @description Tropical Weather Outlook
#' @param destdir Directory to save shapefile data. Saved to tmp dir by default.
#' @export
gis_outlook <- function(destdir = tempdir()) {
    url <- "http://www.nhc.noaa.gov/xgtwo/gtwo_shapefiles.zip"
    utils::download.file(file.path(url), z <- tempfile())
    utils::unzip(z, exdir = destdir)
    shp_files <- list.files(path = destdir, pattern = ".+shp$")
    shp_file_names <- stringr::str_match(shp_files, "^(.+)\\.shp$")[,2]
    ds <- purrr::map2(.x = shp_files, .y = destdir, .f = function(f, d) {
        f <- stringr::str_match(f, "^(.+)\\.shp$")[,2]
        tryCatch(shp <- rgdal::readOGR(dsn = d, layer = f),
                 error = function(c) "error",
                 warning = function(c) "warning",
                 message = function(c) "message",
                 finally = {
                     shp@data$id <- rownames(shp@data)
                     shp.points <- broom::tidy(shp, region = "id")
                     df <- dplyr::left_join(shp.points, shp@data, by = "id")
                     return(df)
                 })
    })
    names(ds) <- shp_file_names
    return(ds)
}

#' @title gis_prob_storm_surge
#' @description Probabilistic Storm Surge
#' @param key Key of storm (i.e., AL012008, EP092015)
#' @param products list of products and associated n values; psurge (0:20) or
#'     esurge (10, 20, 30, 40, 50).
#' @param datetime Datetime in \%Y\%m\%d\%H format.
#' @param nobs specific observations to retrieve by formula or numeric vector.
#'     By default, only the last observation for each product is returned.
#' @details Probabilistic Storm Surge Forecasts
#' @section Products:
#' \describe{
#'     \item{esurge}{The Tropical Cyclone Storm Surge Exceedences (P-Surge 2.5)
#'           data shows the probability, in percent, of a specified storm surge,
#'           including tides, exceeding the specified height, in feet, during
#'           the forecast period indicated. The 10 percent exceedence height,
#'           for example, is the storm surge height, including tides, above
#'           ground level (AGL) such that there is a 10 percent chance of
#'           exceeding it. The product is based upon an ensemble of Sea, Lake,
#'           and Overland Surge from Hurricanes (SLOSH) model runs using the
#'           National Hurricane Center (NHC) official advisory and accounts for
#'           track, size, and intensity errors based on historical errors and
#'           astronomical tide. Valid values are 10, 20, 30, 40 or 50.}
#'     \item{psurge}{The Tropical Cyclone Storm Surge Probabilities (P-Surge
#'           2.5) data shows the probability, in percent, of a specified storm
#'           surge occurring during the forecast period indicated. The product
#'           is based upon an ensemble of Sea, Lake, and Overland Surge from
#'           Hurricanes (SLOSH) model runs using the National Hurricane Center
#'           (NHC) official advisory and accounts for track, size, and intensity
#'           errors based on historical errors and astronomical tide. Valid
#'           values are 0:20.}
#' }
#'
#' @section Subsetting datasets:
#' Unlike other GIS data, these datasets do not contain advisory numbers but
#' are grouped by date/time values in \%Y\%m\%d\%H format. Typically, valid
#' hours will be 00, 06, 12 and 18 UTC though this may not always be the case.
#' To help you get the dataset you need you can use datetime and nobs together.
#' There are two parameters you can pass to help narrow down the dataset you
#' want.
#' \describe{
#'     \item{datetime}{The datetime in character formatted as \%Y\%m\%d\%H; i.e.,
#'         "2016083000".  If you pass this value then the GIS dataset for August
#'         30, 2016, 00:00 UTC will be returned. If you leave off the hour then
#'         all data for Aug 30, 2016 will be returned; leave off the hour and
#'         day then all data for August will be returned. You cannot drop values
#'         on the left side; only from the right. This is to help ensure you get
#'         the dataset you are requesting.}
#'     \item{nobs}{A formula or numeric vector specifying the range of datasets
#'         to retrieve.}
#' }
#' See examples for more guidance on using datetime and nobs together.
#' @seealso \href{http://www.nhc.noaa.gov/surge/psurge.php}{Tropical Cyclone Storm Surge Probabilities}
#' @examples
#' \dontrun{
#' # Return the last psurge0 product for storm AL092016
#' gis_storm_surge("AL092016", products = list("psurge" = 0))
#'
#' # Return the first psurge0 and esurge10 product for storm AL092016
#' gis_storm_surge("AL092016", products = list("psurge" = 0, "esurge" = 10), nobs = 1)
#'
#' # Return all psurge0 products for September 3, 2016, storm AL092016
#' gis_storm_surge("AL092016", products = list("psurge" = 0),
#'                 datetime = "20160903", nobs = ~c(1:length(x)))
#'
#' # Last five esurge40 and esurge50 products for month of September, storm AL092016
#' gis_storm_surge("AL092016", products = list("esurge" = c(40, 50)),
#'                 datetime = "201609", nobs = ~c((length(x)-10):length(x)))
#' }
#' @export
gis_prob_storm_surge <- function(key, products, datetime = NULL, nobs = ~ length(x)) {

    if (is.null(key))
        stop("Please provide storm key")

    # Validate products
    if (!(all(names(products) %in% c("psurge", "esurge"))))
        stop("Invalid product. Must be psurge and/or esurge")

    if (!is.null(products[["psurge"]]))
        if (!(all(dplyr::between(products[["psurge"]], 0, 20))))
            stop("psurge values must be between 0 and 20")

    if (!is.null(products[["esurge"]]))
        if (!(all(products[["esurge"]] %in% seq(10, 50, by = 10))))
            stop("esurge values must be 10, 20, 30, 40 or 50")

    key <- stringr::str_to_lower(key)

    if (!grepl("^[[:lower:]]{2}[[:digit:]]{6}$", key))
        stop("Invalid key")

    key <- stringr::str_match(key, pattern = "([:lower:]{2})([:digit:]{2})([:digit:]{4})")
    names(key) <- c("original", "basin", "year_num", "year")

    # Get list of GIS forecast zips for storm and download
    url <- sprintf("http://www.nhc.noaa.gov/gis/archive_psurge_results.php?id=%s%s&year=%s",
                   key[["basin"]], key[["year_num"]], key[["year"]])
    contents <- readr::read_lines(url)

    # Build product pattern
    ptn_product <- names(products) %>%
        purrr::map(.f = function(x) paste0(x, products[[x]])) %>%
        purrr::flatten_chr()

    # Build datetime pattern
    if (is.null(datetime)) {
        ptn_datetime <- "[:digit:]+"
    } else {
        # If x$datetime is 10 digits, then user is looking for specific datetime
        # value. Pattern must be that value.
        if (grepl("[[:digit:]]{10}", datetime)) {
            ptn_datetime <- datetime
        } else {
            # Otherwise, x$datetime is beginning of pattern with wildcard at end.
            ptn_datetime <- paste0(datetime, "[:digit:]+")
        }
    }

    # Match zip files.
    ptn <- sprintf(".+(storm_surge/%s_(%s)_(%s)\\.zip).+",
                   stringr::str_to_lower(key[["original"]]),
                   paste(ptn_product, collapse = "|"),
                   ptn_datetime)

    ds <- contents[stringr::str_detect(contents, pattern = ptn)]

    # Filter datasets, if first, last or nth is not NULL
    if (is.language(nobs)) {
        ds <- ptn_product %>% purrr::map(.f = ~ ds[grepl(.x, ds)]) %>%
            purrr::map(~ .x[lazyeval::f_eval(nobs, data = list(x = .x))]) %>%
            purrr::flatten_chr()
    } else {
        ds <- ptn_product %>% purrr::map(.f = ~ ds[grepl(.x, ds)]) %>%
            purrr::map(~ .x[nobs]) %>%
            purrr::flatten_chr()
    }

    return(ds)
    # # Extract link to zip files. Error gracefully if no matches.
    # tryCatch(links <- stringr::str_match(matches, pattern = ptn)[,2],
    #          error = function(c) {
    #              c$message <- "No data available for requested storm/advisory"
    #              stop(c$message, call. = FALSE)
    #          })
    # # Create sub directories for each zip file
    # subdirs <- stringr::str_match(links, pattern = "storm_surge/(.+)\\.zip")[,2]
    # return(subdirs)
}
