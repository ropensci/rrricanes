#' @title gis_advisory
#' @description Advisory Forecast Track, Cone of Uncertainty, and
#' Watches/Warnings
#' @param key Key of storm (i.e., AL012008, EP092015)
#' @param advisory Advisory number. If NULL, all advisories are returned.
#' Intermediate advisories are acceptable.
#' @seealso \code{\link{gis_download}}
#' @export
gis_advisory <- function(key, advisory = as.character()) {

    if (is.null(key))
        stop("Please provide storm key")

    key <- stringr::str_to_lower(key)

    if (!grepl("^[[:lower:]]{2}[[:digit:]]{6}$", key))
        stop("Invalid key")

    key <- stringr::str_match(key,
                              pattern = paste0("([:lower:]{2})([:digit:]{2})",
                                               "([:digit:]{4})"))
    names(key) <- c("original", "basin", "year_num", "year")

    # Get list of GIS forecast zips for storm and download
    url <- sprintf("http://www.nhc.noaa.gov/gis/archive_forecast_results.php?id=%s%s&year=%s",
                   key[["basin"]], key[["year_num"]], key[["year"]])
    contents <- readr::read_lines(url)

    # Match zip files. If advisory is empty then need to pull all zip files for
    # the storm. Otherwise, pull only selected advisory.
    if (purrr::is_empty(advisory)) {
        ptn <- sprintf(".+(forecast/archive/%s.*?\\.zip).+",
                       stringr::str_to_lower(key[["original"]]))
    } else {
        advisory <- stringr::str_match(advisory, "([:digit:]{1,3})([:alpha:]*)")
        names(advisory) <- c("original", "advisory", "int_adv")
        ptn <- sprintf(".+(forecast/archive/%s.*?%s%s\\.zip).+",
                       stringr::str_to_lower(key["original"]),
                       stringr::str_pad(string = advisory[["advisory"]],
                                        width = 3, side = "left", pad = "0"),
                       advisory[["int_adv"]])
    }

    matches <- contents[stringr::str_detect(contents, pattern = ptn)]
    # Extract link to zip files. Error gracefully if no matches.
    tryCatch(links <- stringr::str_match(matches, pattern = ptn)[,2],
             error = function(c) {
                 c$message <- "No data avaialable for requested storm/advisory"
                 stop(c$message, call. = FALSE)
             })
    # Append website domain to links
    links <- paste0("http://www.nhc.noaa.gov/gis/", links)
    return(links)
}

#' @title gis_breakpoints
#' @description Return link to breakpoints shapefile by year
#' @param year Default is current year. Breakpoints only available >= 2008.
#' @details Coastal areas placed under tropical storm and hurricane watches and
#' warnings are identified through the use of "breakpoints." A tropical cyclone
#' breakpoint is defined as an agreed upon coastal location that can be chosen
#' as one of two specific end points or designated places between which a
#' tropical storm/hurricane watch/warning is in effect. The U.S. National
#' Weather Service designates the locations along the U.S. East, Gulf, and
#' California coasts, Puerto Rico, and Hawaii. These points are listed in NWS
#' Directive 10-605 (PDF). Individual countries across the Caribbean, Central
#' America, and South America provide coastal locations for their areas of
#' responsibility to the U.S. National Weather Service for the National
#' Hurricane Center's use in tropical cyclone advisories when watches/warnings
#' are issued by international partners. The National Hurricane Center maintains
#' a list of pre-arranged breakpoints for the U.S. Atlantic and Gulf coasts,
#' Mexico, Cuba and the Bahamas. Other sites are unofficial and sites not on the
#' list can be selected if conditions warrant.
#' @export
gis_breakpoints <- function(year = as.numeric(strftime(Sys.Date(), "%Y"))) {

    # xpath pattern
    xp <- "//a"

    links <- httr::POST("http://www.nhc.noaa.gov/gis/archive_breakpoints.php",
                        body = list(year = year), encode = "form") %>%
        httr::content(as = "parsed", encoding = "UTF-8") %>%
        rvest::html_nodes(xpath = xp) %>%
        rvest::html_attr("href") %>%
        stringr::str_extract(sprintf("Breakpoints_%s\\.zip$", year)) %>%
        .[stats::complete.cases(.)]

    if (purrr::is_empty(links))
        return(NULL)

    links <- paste0("http://www.nhc.noaa.gov/gis/breakpoints/archive/", links)

    return(links)
}

#' @title gis_download
#' @description Get GIS data for storm.
#' @param url link to GIS dataset to download.
#' @export
gis_download <- function(url) {

    destdir <- tempdir()

    utils::download.file(file.path(url), zip_file <- tempfile())

    zip_contents <- utils::unzip(zip_file, list = TRUE)$Name

    utils::unzip(zip_file, exdir = destdir)

    shp_files <- stringr::str_match(zip_contents, pattern = ".+shp$") %>%
        .[!is.na(.)]

    ds <- purrr::map2(.x = shp_files, .y = destdir, .f = function(f, d) {
        shp_file <- stringr::str_match(f, "^(.+)\\.shp$")[,2]
        sp_object <- rgdal::readOGR(dsn = d, layer = shp_file,
                                    encoding = "UTF-8",
                                    stringsAsFactors = FALSE,
                                    use_iconv = TRUE)
        return(sp_object)
    })

    names(ds) <- stringr::str_match(shp_files, "^(.+)\\.shp$")[,2] %>%
        stringr::str_replace_all("[[:punct:][:space:]]", "_")

    # clean up
    x <- unlink(c(paste(destdir, zip_contents, sep = "/"), zip_file))

    return(ds)
}

#' @title gis_latest
#' @description Latest GIS datasets for active cyclones
#' @param basins AL and/or EP.
#' @export
gis_latest <- function(basins = c("AL", "EP")) {

    if (!(all(basins %in% c("AL", "EP"))))
        stop("Invalid basin")

    urls <- list("AL" = "http://www.nhc.noaa.gov/gis-at.xml",
                 "EP" = "http://www.nhc.noaa.gov/gis-ep.xml")

    gis_zips <- purrr::map(basins, ~ xml2::read_xml(urls[[.x]])) %>%
        purrr::map(~ xml2::xml_find_all(.x, xpath = ".//link") %>%
                       xml2::xml_text()) %>%
        purrr::map(stringr::str_match, ".+\\.zip$") %>%
        purrr::flatten_chr() %>%
        .[!is.na(.)]

    if (!purrr::is_empty(gis_zips)) {
        ds <- purrr::map(gis_zips, gis_download)
        return(ds)
    }
    return(FALSE)
}

#' @title gis_outlook
#' @description Tropical Weather Outlook
#' @seealso \code{\link{gis_download}}
#' @export
gis_outlook <- function() {
    url <- "http://www.nhc.noaa.gov/xgtwo/gtwo_shapefiles.zip"
    return(url)
}

#' @title gis_prob_storm_surge
#' @description Probabilistic Storm Surge
#' @param key Key of storm (i.e., AL012008, EP092015)
#' @param products list of products and associated n values; psurge (0:20) or
#'     esurge (10, 20, 30, 40, 50).
#' @param datetime Datetime in \%Y\%m\%d\%H format.
#' @details Probabilistic Storm Surge Forecasts
#' @section Products:
#' \describe{
#'     \item{esurge}{The Tropical Cyclone Storm Surge Exceedances (P-Surge 2.5)
#'           data shows the probability, in percent, of a specified storm surge,
#'           including tides, exceeding the specified height, in feet, during
#'           the forecast period indicated. The 10 percent exceedance height,
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
#' @seealso \href{http://www.nhc.noaa.gov/surge/psurge.php}{Tropical Cyclone Storm Surge Probabilities}
#' @seealso \code{\link{gis_download}}
#' @examples
#' \dontrun{
#' # Return the last psurge0 product for storm AL092016
#' gis_prob_storm_surge("AL092016", products = list("psurge" = 0))
#'
#' # Return the psurge0 and esurge10 products for storm AL092016
#' gis_prob_storm_surge("AL092016", products = list("psurge" = 0, "esurge" = 10))
#'
#' # Return all psurge0 products for Sep 2, 2016, storm AL092016
#' gis_prob_storm_surge("AL092016", products = list("psurge" = 0),
#'                      datetime = "20160902")
#' }
#' @export
gis_prob_storm_surge <- function(key, products, datetime = NULL) {

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

    key <- stringr::str_match(key, pattern = paste0("([:lower:]{2})([:digit:]",
                                                    "{2})([:digit:]{4})"))
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

    # Extract link to zip files. Error gracefully if no matches.
    tryCatch(links <- stringr::str_match(ds, pattern = ptn)[,2],
             error = function(c) {
                 c$message <- "No data available for requested storm/advisory"
                 stop(c$message, call. = FALSE)
             })
    # Prepend domains to links
    links <- paste0("http://www.nhc.noaa.gov/gis/", links)
    return(links)
}

#' @title gis_storm_surge_flood
#' @description Potential Storm Surge Flooding (Inundation)
#' @param key Key of storm (i.e., AL012008, EP092015)
#' @param advisory Advisory number. If NULL, all available advisories are
#' returned.
#' @param products indundation or tidalmask
#' @seealso \code{\link{gis_download}}
#' @keywords internal
gis_storm_surge_flood <- function(key, advisory = as.numeric(),
                                  products = c("inundation", "tidalmask")) {
    warning("These are raster files, not shapefiles.")
    if (is.null(key))
        stop("Please provide storm key")

    key <- stringr::str_to_upper(key)

    if (!grepl("^[[:alpha:]]{2}[[:digit:]]{6}$", key))
        stop("Invalid key")

    if (!(any(products %in% c("inundation", "tidalmask"))))
        stop("Invalid products")

    key <- stringr::str_match(key, pattern = paste0("([:alpha:]{2})([:digit:]",
                                                    "{2})([:digit:]{4})"))
    names(key) <- c("original", "basin", "year_num", "year")

    # Get list of GIS zips for storm and download
    url <- sprintf("http://www.nhc.noaa.gov/gis/archive_inundation_results.php?id=%s%s&year=%s",
                   key[["basin"]], key[["year_num"]], key[["year"]])
    contents <- readr::read_lines(url)

    if (purrr::is_empty(advisory)) {
        ptn <- sprintf(".+(inundation/forecasts/%s%s%s_[:digit:]{1,2}_(%s)\\.zip).+",
                       key[["basin"]],
                       key[["year_num"]],
                       stringr::str_sub(key[["year"]], start = 3L, end = 4L),
                       paste(products, collapse = "|"))
    } else {
        ptn <- sprintf(".+(inundation/forecasts/%s%s%s_%s_(%s)\\.zip).+",
                       key[["basin"]],
                       key[["year_num"]],
                       stringr::str_sub(key[["year"]], start = 3L, end = 4L),
                       stringr::str_pad(advisory, width = 2, side = "left",
                                        pad = "0"),
                       paste(products, collapse = "|"))
    }

    matches <- contents[stringr::str_detect(contents, pattern = ptn)]
    # Extract link to zip files. Error gracefully if no matches.
    tryCatch(links <- stringr::str_match(matches, pattern = ptn)[,2],
             error = function(c) {
                 c$message <- "No data avaialable for requested storm/advisory"
                 stop(c$message, call. = FALSE)
             })
    # Create sub directories for each zip file
    links <- paste0("http://www.nhc.noaa.gov/gis/", links)
    return(links)
}

#' @title gis_windfield
#' @description Advisory Wind Field and Forecast Wind Radii
#' @param key Key of storm (i.e., AL012008, EP092015)
#' @param advisory Advisory number. If NULL, all advisories are returned.
#' Intermediate advisories are acceptable.
#' @details Tropical Cyclone Advisory Wind Field
#' http://www.nhc.noaa.gov/gis/archive_forecast_info_results.php?id=al14&year=2016
#' http://www.nhc.noaa.gov/gis/forecast/archive/
#' Example file name: al012017_fcst_001.zip
#' [basin]{2}[year_num]{2}[year]{4}_fcst_[advisory]{3}.zip
#' Many storms do not appear to have this data; especially earlier.
#'
#' Not all advisories will be available for storms. For example,
#' \href{http://www.nhc.noaa.gov/gis/archive_forecast_info_results.php?id=al14&year=2016}{Hurricane Matthew (AL142016)}
#' is missing several advisories.
#' @seealso \code{\link{gis_download}}
#' @export
gis_windfield <- function(key, advisory = as.character()) {

    if (is.null(key))
        stop("Please provide storm key")

    key <- stringr::str_to_lower(key)

    if (!grepl("^[[:lower:]]{2}[[:digit:]]{6}$", key))
        stop("Invalid key")

    key <- stringr::str_match(key, pattern = paste0("([:lower:]{2})([:digit:]",
                                                    "{2})([:digit:]{4})"))
    names(key) <- c("original", "basin", "year_num", "year")

    # Get list of GIS forecast zips for storm and download
    url <- sprintf("http://www.nhc.noaa.gov/gis/archive_forecast_info_results.php?id=%s%s&year=%s",
                   key[["basin"]], key[["year_num"]], key[["year"]])
    contents <- readr::read_lines(url)

    # Match zip files. If advisory is empty then need to pull all zip files for
    # the storm. Otherwise, pull only selected advisory.
    if (purrr::is_empty(advisory)) {
        ptn <- sprintf(".+(forecast/archive/%s.*?\\.zip).+",
                       stringr::str_to_lower(key[["original"]]))
    } else {
        advisory <- stringr::str_match(advisory, "([:digit:]{1,3})([:alpha:]*)")
        names(advisory) <- c("original", "advisory", "int_adv")
        ptn <- sprintf(".+(forecast/archive/%s.*?%s%s\\.zip).+",
                       stringr::str_to_lower(key["original"]),
                       stringr::str_pad(string = advisory[["advisory"]],
                                        width = 3, side = "left", pad = "0"),
                       advisory[["int_adv"]])
    }

    matches <- contents[stringr::str_detect(contents, pattern = ptn)]
    # Extract link to zip files. Error gracefully if no matches.
    tryCatch(links <- stringr::str_match(matches, pattern = ptn)[,2],
             error = function(c) {
                 c$message <- "No data avaialable for requested storm/advisory"
                 stop(c$message, call. = FALSE)
             })
    links <- paste0("http://www.nhc.noaa.gov/gis/", links)
    return(links)
}

#' @title gis_wsp
#' @description Wind Speed Probabilities
#' @param datetime Datetime in \%Y\%m\%d\%H format. \%m, \%d and \%H are
#' optional but will return more datasets.
#' @param res Resolution as a numeric vector; 5, 0.5, 0.1.
#' @details Probability winds affecting an area within a forecast period.
#' Datasets contain windfields for 34kt, 50kt and 64kt. Resolution is at 5km,
#' 0.5 degrees and 0.1 degrees. Not all resolutions may be available for all
#' storms. Not all windfields will be available for all advisories.
#' @seealso \code{\link{gis_download}}
#' @examples
#' \dontrun{
#' # Return datasets for January 1, 2016 with resolution of 0.5 degrees
#' gis_wndprb("20160101", res = 0.5)
#'
#' # Return wsp of 0.1 and 0.5 degree resolution, July, 2015
#' gis_wndprb("201507", res = c(0.5, 0.1))
#' }
#' @export
gis_wsp <- function(datetime, res = c(5, 0.5, 0.1)) {

    if (!grepl("[[:digit:]]{4,10}", datetime))
        stop("Invalid datetime")

    if (!(all(res %in% c(5.0, 0.5, 0.1))))
        stop("Invalid resolution")

    res <- as.character(res)
    res <- stringr::str_replace(res, "^5$", "5km")
    res <- stringr::str_replace(res, "^0.5$", "halfDeg")
    res <- stringr::str_replace(res, "^0.1$", "tenthDeg")

    year <- stringr::str_sub(datetime, 0L, 4L)

    request <- httr::POST("http://www.nhc.noaa.gov/gis/archive_wsp.php",
                          body = list(year = year), encode = "form")
    contents <- httr::content(request, as = "parsed", encoding = "UTF-8")

    ds <- rvest::html_nodes(contents, xpath = "//a") %>%
        rvest::html_attr("href") %>%
        stringr::str_extract(".+\\.zip$") %>%
        .[stats::complete.cases(.)]

    if (nchar(datetime) < 10) {
        ptn_datetime <- paste0(datetime, "[:digit:]+")
    } else {
        ptn_datetime <- datetime
    }

    ptn_res <- paste(res, collapse = "|")

    ptn <- sprintf("%s_wsp_[:digit:]{1,3}hr(%s)", ptn_datetime, ptn_res)
    links <- ds[stringr::str_detect(ds, ptn)]
    links <- paste0("http://www.nhc.noaa.gov/gis/", links)
    return(links)
}

#' @title shp_to_df
#' @description Convert shapefile object to dataframe
#' @param obj Spatial object to convert. See details.
#' @details Takes a SpatialLinesDataFrame object or SpatialPolygonsDataFrame
#' object and converts into a dataframe that can be plotted in ggplot2.
#' @export
shp_to_df <- function(obj) {
    if (class(obj) %in% c("SpatialLinesDataFrame", "SpatialPolygonsDataFrame")) {
        obj@data$id <- rownames(obj@data)
        obj <- dplyr::left_join(broom::tidy(obj, region = "id"),
                                obj@data, by = "id") %>%
            tibble::as_data_frame()
    }

    return(obj)
}
