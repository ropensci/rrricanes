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
