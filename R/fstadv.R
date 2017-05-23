#' @title create_df_fstadv
#' @description Template for public advisory dataframe
#' @return empty dataframe
#' @seealso \code{\link{get_fstadv}}
#' @keywords internal
create_df_fstadv <- function() {
    df <- tibble::data_frame(
        "Key" = character(),
        # Allow for intermediate advisories,
        # i.e., "1A", "2", "2A"...
        "Adv" = character(),
        "Date" = as.POSIXct(character(), tz = "UTC"),
        "Name" = character(),
        "Status" = character(),
        'Lat' = numeric(),
        'Lon' = numeric(),
        'Wind' = numeric(),
        'Gust' = numeric(),
        'Pressure' = numeric(),
        'PosAcc' = numeric(),
        'FwdDir' = numeric(),
        'FwdSpeed' = numeric(),
        'Eye' = numeric(),
        "NE34" = numeric(),
        "SE34" = numeric(),
        "SW34" = numeric(),
        "NW34" = numeric(),
        "NE50" = numeric(),
        "SE50" = numeric(),
        "SW50" = numeric(),
        "NW50" = numeric(),
        "NE64" = numeric(),
        "SE64" = numeric(),
        "SW64" = numeric(),
        "NW64" = numeric(),
        "SeasNE" = numeric(),
        "SeasSE" = numeric(),
        "SeasSW" = numeric(),
        "SeasNW" = numeric(),
        "Hr12FcstDate" = as.POSIXct(character(), tz = "UTC"),
        "Hr12Lat" = numeric(),
        "Hr12Lon" = numeric(),
        "Hr12Wind" = numeric(),
        "Hr12Gust" = numeric(),
        "Hr12NE34"  = numeric(),
        "Hr12SE34"  = numeric(),
        "Hr12SW34"  = numeric(),
        "Hr12NW34"  = numeric(),
        "Hr12NE50"  = numeric(),
        "Hr12SE50"  = numeric(),
        "Hr12SW50"  = numeric(),
        "Hr12NW50"  = numeric(),
        "Hr12NE64"  = numeric(),
        "Hr12SE64"  = numeric(),
        "Hr12SW64"  = numeric(),
        "Hr12NW64"  = numeric(),
        "Hr24FcstDate" = as.POSIXct(character(), tz = "UTC"),
        "Hr24Lat" = numeric(),
        "Hr24Lon" = numeric(),
        "Hr24Wind" = numeric(),
        "Hr24Gust" = numeric(),
        "Hr24NE34"  = numeric(),
        "Hr24SE34"  = numeric(),
        "Hr24SW34"  = numeric(),
        "Hr24NW34"  = numeric(),
        "Hr24NE50"  = numeric(),
        "Hr24SE50"  = numeric(),
        "Hr24SW50"  = numeric(),
        "Hr24NW50"  = numeric(),
        "Hr24NE64"  = numeric(),
        "Hr24SE64"  = numeric(),
        "Hr24SW64"  = numeric(),
        "Hr24NW64"  = numeric(),
        "Hr36FcstDate" = as.POSIXct(character(), tz = "UTC"),
        "Hr36Lat" = numeric(),
        "Hr36Lon" = numeric(),
        "Hr36Wind" = numeric(),
        "Hr36Gust" = numeric(),
        "Hr36NE34"  = numeric(),
        "Hr36SE34"  = numeric(),
        "Hr36SW34"  = numeric(),
        "Hr36NW34"  = numeric(),
        "Hr36NE50"  = numeric(),
        "Hr36SE50"  = numeric(),
        "Hr36SW50"  = numeric(),
        "Hr36NW50"  = numeric(),
        "Hr36NE64"  = numeric(),
        "Hr36SE64"  = numeric(),
        "Hr36SW64"  = numeric(),
        "Hr36NW64"  = numeric(),
        "Hr48FcstDate" = as.POSIXct(character(), tz = "UTC"),
        "Hr48Lat" = numeric(),
        "Hr48Lon" = numeric(),
        "Hr48Wind" = numeric(),
        "Hr48Gust" = numeric(),
        "Hr48NE34"  = numeric(),
        "Hr48SE34"  = numeric(),
        "Hr48SW34"  = numeric(),
        "Hr48NW34"  = numeric(),
        "Hr48NE50"  = numeric(),
        "Hr48SE50"  = numeric(),
        "Hr48SW50"  = numeric(),
        "Hr48NW50"  = numeric(),
        "Hr48NE64"  = numeric(),
        "Hr48SE64"  = numeric(),
        "Hr48SW64"  = numeric(),
        "Hr48NW64"  = numeric(),
        "Hr72FcstDate" = as.POSIXct(character(), tz = "UTC"),
        "Hr72Lat" = numeric(),
        "Hr72Lon" = numeric(),
        "Hr72Wind" = numeric(),
        "Hr72Gust" = numeric(),
        "Hr72NE34"  = numeric(),
        "Hr72SE34"  = numeric(),
        "Hr72SW34"  = numeric(),
        "Hr72NW34"  = numeric(),
        "Hr72NE50"  = numeric(),
        "Hr72SE50"  = numeric(),
        "Hr72SW50"  = numeric(),
        "Hr72NW50"  = numeric(),
        "Hr72NE64"  = numeric(),
        "Hr72SE64"  = numeric(),
        "Hr72SW64"  = numeric(),
        "Hr72NW64"  = numeric(),
        # No wind radius for 96 hours
        "Hr96FcstDate" = as.POSIXct(character(), tz = "UTC"),
        "Hr96Lat" = numeric(),
        "Hr96Lon" = numeric(),
        "Hr96Wind" = numeric(),
        "Hr96Gust" = numeric(),
        # No wind radius for 120 hours
        "Hr120FcstDate" = as.POSIXct(character(), tz = "UTC"),
        "Hr120Lat" = numeric(),
        "Hr120Lon" = numeric(),
        "Hr120Wind" = numeric(),
        "Hr120Gust" = numeric())

    return(df)
}

#' @title get_fstadv
#' @description Return dataframe of forecast/advisory data.
#' @param link URL to storm's archive page.
#' @param msg Show link currently being worked. Default, FALSE.
#' @seealso \code{\link{get_storms}}, \code{\link{public}}
#' @export
get_fstadv <- function(link, msg = FALSE) {

    # Check status of link(s)
    valid.link <- sapply(link, status)
    valid.link <- na.omit(valid.link)
    if (length(valid.link) == 0)
        stop("No valid links.")

    products <- purrr::map(valid.link, get_products) %>% purrr::flatten_chr()

    products.fstadv <- purrr::map(filter_fstadv(products), fstadv)

    fstadv <- purrr::map_df(products.fstadv, dplyr::bind_rows)

    return(fstadv)

}

#' @title fstadv
#' @description Extrapolate data from FORECAST/ADVISORY products.
#' @details Given a direct link to a forecast/advisory product, parse and
#' return dataframe of values.
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane,
#'     etc.}
#'   \item{Name}{Name of storm}
#'   \item{Adv}{Advisory Number}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Key}{Unique identifier for the storm}
#'   \item{Lat}{Latitude, location of the storm. Positive is northern
#'     hemisphere. Negative is southern hemisphere.}
#'   \item{Lon}{Longitude, location of the storm. Positive is eastern
#'     hemisphere. Negative is western hemisphere.}
#'   \item{Wind}{Current wind speed in knots.}
#'   \item{Gust}{Current maximum wind gusts in knots.}
#'   \item{Pressure}{Central barometric pressure in millibars.}
#'   \item{PosAcc}{Position accuracy of storm in nautical miles.}
#'   \item{FwdDir}{
#'       Forward direction of storm on a compass scale (0-359 or NA).
#'   }
#'   \item{FwdSpeed}{
#'       Forward speed of storm in knots or NA if stationary/drifting.
#'   }
#'   \item{Eye}{Size of the eye in nautical miles, if available, or NA.}
#' }
#' @param link URL of a specific FORECAST/ADVISORY product
#' @param msg Display each link as being worked; default is FALSE
#' @return Dataframe
#' @seealso \code{\link{get_fstadv}}
#' @keywords internal
fstadv <- function(link, msg = FALSE) {

    contents <- scrape_contents(link, msg = msg)

    # Make sure this is a public advisory product
    if (!any(stringr::str_count(contents,
                                c("MIATCMAT", "MIATCMEP", "[W]*TPA", "TCMAT",
                                  "WTPZ"))))
        stop(sprintf("Invalid Forecast/Advisory link. %s", link))

    df <- create_df_fstadv()

    status <- scrape_header(contents, ret = "status")
    name <- scrape_header(contents, ret = "name")
    adv <- scrape_header(contents, ret = "adv")
    date <- scrape_header(contents, ret = "date")
    key <- scrape_header(contents, ret = "key")
    lat <- fstadv_lat(contents)
    lon <- fstadv_lon(contents)
    posacc <- fstadv_pos_accuracy(contents)
    fwd_dir <- fstadv_fwd_dir(contents)
    fwd_speed <- fstadv_fwd_speed(contents)
    pressure <- fstadv_pressure(contents)
    eye <- fstadv_eye(contents)
    wind <- fstadv_winds(contents)
    gust <- fstadv_gusts(contents)

    df <- df %>%
        tibble::add_row("Status" = status,
                        "Name" = name,
                        "Adv" = adv,
                        "Date" = date,
                        "Key" = key,
                        'Lat' = lat,
                        'Lon' = lon,
                        'Wind' = wind,
                        'Gust' = gust,
                        'Pressure' = pressure,
                        'PosAcc' = posacc,
                        'FwdDir' = fwd_dir,
                        'FwdSpeed' = fwd_speed,
                        'Eye' = eye)

    # Add current wind radius
    wind_radius <- fstadv_wind_radius(contents, wind)

    df[, names(wind_radius)] <- wind_radius[, names(wind_radius)]

    # Add current sea radius
    seas <- fstadv_seas(contents, wind)

    df[, names(seas)] <- seas[, names(seas)]

    # Add forecast positions and wind radii
    forecasts <- fstadv_forecasts(contents, date)

    df[, names(forecasts)] <- forecasts[, names(forecasts)]

    return(df)
}

#' @title fstadv_eye
#' @description Get eye diameter, if available
#' @param contents text contents of FORECAST/ADVISORY
#' @return numeric
#' @keywords internal
fstadv_eye <- function(contents) {

    ptn <- paste0('EYE DIAMETER[ ]+',
                  '([0-9]{2,3})', # Eye diameter, integer
                  '[ ]+NM')
    eye <- stringr::str_match(contents, ptn)[,2]
    return(as.numeric(eye))
}

#' @title fstadv_extract_forecasts
#' @description Extract all forecast data from FORECAST/ADVISORY product#'
#' @param content text of FORECAST/ADVISORY product
#' @return list of forecasts
#' @keywords internal
fstadv_extract_forecasts <- function(content) {

    # First I want to get the groups of each FORECAST or OUTLOOK. I'll do some
    # text manipulations to make pattern-finding easier.

    # In some texts there is a space character between the returns. In others,
    # none.
    a <- stringr::str_replace_all(content, '\n[:blank:]*?\n', '\t')

    # Now any newline characters I'll replace with a simple space
    b <- stringr::str_replace_all(a, '[\n]{1}', ' ')

    # Put the tab character back to newline
    c <- stringr::str_replace_all(b, '\t', '\n')

    # Remove any ellipses
    d <- stringr::str_replace_all(c, '\\.\\.\\.', ' ')

    # Some products have periods in them after KT or NW. Some don't. To be
    # consistent, I'll remove them, too. But have to be careful not to remove
    # those in Lat and Lon
    e <- stringr::str_replace_all(d, 'KT\\.', 'KT')
    f <- stringr::str_replace_all(e, 'NW\\.', 'NW')

    # Extract all lines that begin with FORECAST VALID or OUTLOOK VALID and end
    # with newline character
    g <- stringr::str_match_all(f, (paste0('[:space:]*(?:FORECAST|OUTLOOK) VALID ',
                                           '[[:alnum:] /\\.-]+')))

    # at this point we *should* have all of our forecasts.
    h <- unlist(g)

    # Replace any extra verbiage like INLAND or EXTRATROPICAL
    i <- stringr::str_replace_all(h, '([E|W])[A-Z ]+(MAX WIND)', '\\1 \\2')

    return(i)

}

#' @title fstadv_fcst
#' @description Retrieve forecast data from FORECAST/ADVISORY products. Loads into
#'   respective dataframes (df_forecasts, df_forecast_winds)
#' @param content text content of FORECAST/ADVISORY
#' @param date Date value of current forecast/advisory product.
#' @return boolean
#' @keywords internal
fstadv_forecasts <- function(content, date) {

    # At this point all I'm doing is extracting every subset that begins with
    # either FORECAST VALID or OUTLOOK VALID
    ds <- fstadv_extract_forecasts(content)

    # Break forecasts into a list by observation time; Extracts date/time to
    # gusts (keys 2:9) and then wind fields, if avail (key 10)
    ds <- fstadv_parse_forecasts(ds)

    # If no forecasts, exit gracefully
    if (all(purrr::map_lgl(ds, purrr::is_empty)))
        return(NULL)

    # Reformat to load into dataframe
    ds <- lapply(ds, data.frame, stringsAsFactors = FALSE)

    # Load only from X2 (Date) to X10 (Gust)
    df <- data.table::rbindlist(ds)[,2:11]
    # Rename
    data.table::setattr(df,
                        'names',
                        c('d', 'h', 'mi', 'lat', 'lath', 'lon', 'lonh', 'w',
                          'g', 'wf'))

    # Start cleaning up
    # ObDate
    # Get current observation date. Will need month
    ob_year <- lubridate::year(date)
    ob_month <- lubridate::month(date)
    ob_day <- lubridate::day(date)

    df$d <- as.numeric(df$d) # Day
    df$h <- as.numeric(df$h) # Hour
    df$mi <- as.numeric(df$mi) # Minute
    df$lat <- as.numeric(df$lat) # Latitude
    df$lon <- as.numeric(df$lon) # Longitude
    df$w <- as.numeric(df$w) # Wind
    df$g <- as.numeric(df$g) # Gust

    # Here I account for the possibility of the forecast date crossing months
    # or even years
    df <- df %>%
        dplyr::mutate(mo = ifelse(d < ob_day, ob_month + 1, ob_month),
                      y = ifelse(mo < ob_month, ob_year + 1, ob_year),
                      FcstDate = lubridate::ymd_hm(paste(paste(y, mo, d, sep = '-'),
                                                         paste(h, mi, sep = ':'),
                                                         sep = ' ')))

    # Clean up lat, lath, lon, lonh. Though no storms should exist in southern
    # hemisphere there will be some crossing from western hemi to east.
    # Southern/Western hemi lat/lon are negative. Northern/Eastern are positive.
    df <- df %>%
        dplyr::mutate(Lat = ifelse(lath == 'S', lat * -1, lat), # Lat reformatted
                      Lon = ifelse(lonh == 'W', lon * -1, lon)) # Lon reformatted

    # Finish renaming/reclassifying vars Wind and Gust, add Key, Adv, ObDate
    df <- df %>%
        dplyr::mutate(Wind = as.numeric(w), # reformatted
                      Gust = as.numeric(g)) # reformatted

    # Build forecasts dataframe into wide format
    tmp <- df %>%
        dplyr::select(FcstDate, Lat, Lon, Wind, Gust, wf)

    n <- nrow(tmp)

    # Make any wf vars that are not valid wf values NA
    tmp$wf[tmp$wf %in% c("", "DISSIPATING", "DISSIPATED")] <- NA

    tmp <- tmp %>% split(.$FcstDate)

    tmp <- tmp %>% purrr::map2(1:n, function(a, b) {
        # Here, send variable `wf` to function to spread across multiple cols.
        if (!is.na(tmp[[b]]$wf)) {
            wf <- fstadv_forecast_wind_radius(tmp[[b]]$wf)
            tmp[[b]] <- dplyr::bind_cols(tmp[[b]], wf)
        }
        tmp[[b]]$wf <- NULL
        if (b <= 4) {
            newnames <- paste0("Hr", b * 12, names(tmp[[b]]))
        } else if (b == 5) {
            newnames <- paste0("Hr72", names(tmp[[b]]))
        } else if (b == 6) {
            newnames <- paste0("Hr96", names(tmp[[b]]))
        } else if (b == 7) {
            newnames <- paste0("Hr120", names(tmp[[b]]))
        }
        stats::setNames(tmp[[b]], newnames)
    })

    df_forecasts <- dplyr::bind_cols(tmp)


    return(df_forecasts)

}

#' @title fstadv_forecast_wind_radius
#' @description Separate wind field columns to columns by quadrant.
#' @keywords internal
fstadv_forecast_wind_radius <- function(df) {
    # There are three fields of four quadrants each: 34KT, 50KT, and 64KT. The
    # maximum wind field will always be listed first. For example, this may be
    # in one product:
    #
    # 34 KT...60NE  60SE  60SW  60NW.
    #
    # And this may be in another:
    #
    # 50 KT...30NE  30SE  20SW  30NW.
    # 34 KT...70NE  70SE  60SW  70NW.
    #
    # There will also be a 64KT field for hurricanes.
    #
    # The quadrants will be consistent.
    #
    # When we get to this function we expect a dataframe of 1x1 with a
    # character value, using the examples above, of:
    #
    # 34 KT...60NE  60SE  60SW  60NW.
    #
    # 50 KT...30NE  30SE  20SW  30NW. 34 KT...70NE  70SE  60SW  70NW.
    #
    # Each quadrant value must be it's own variable with the name of the
    # variable identifying the field it is for:
    #
    # NE34 SE34 SW34 NW34 NE50 SE50 SW50 NW50 NE64 SE64 SW64 NW64

    # Example: 34 KT 60NE  60SE  60SW  60NW
    ptn <- paste0("([:digit:]{2})",                   # "34"
                  "[[:alpha:][:blank:][:punct:]]+",   # " KT "
                  "([:digit:]{1,3})",                 # "60"
                  "[[:alpha:][:blank:]]+",            # "NE "
                  "([:digit:]{1,3})",                 # "60"
                  "[[:alpha:][:blank:]]+",            # "SE "
                  "([:digit:]{1,3})",                 # "60"
                  "[[:alpha:][:blank:]]+",            # "SW"
                  "([:digit:]{1,3})",                 # "60"
                  "[[:alpha:][:blank:][:punct]]+")    # "NW"

    x <- stringr::str_match_all(df, ptn)

    # [[1]]
    # [,1]                            [,2] [,3] [,4] [,5] [,6]
    # [1,] "50 KT 30NE  30SE  20SW  30NW " "50" "30" "30" "20" "30"
    # [2,] "34 KT 70NE  70SE  60SW  70NW"  "34" "70" "70" "60" "70"

    # Convert to dataframe, making values numeric
    y <- tibble::data_frame("V1" = x[[1]][,2],
                            "V2" = x[[1]][,3],
                            "V3" = x[[1]][,4],
                            "V4" = x[[1]][,5],
                            "V5" = x[[1]][,6]) %>%
        purrr::map_df(as.numeric) %>%
        dplyr::rename_("WF" = "V1",
                       "NE" = "V2",
                       "SE" = "V3",
                       "SW" = "V4",
                       "NW" = "V5")

    # Split by column
    z <- y %>% split(.$WF)

    # Assign names to columns
    a <- z %>% purrr::map2(1:length(z), function(a, b) {
        newnames <- paste0(names(z[[b]]), z[[b]][[1]])
        stats::setNames(z[[b]], newnames)
    })

    b <- dplyr::bind_cols(a) %>% dplyr::select(-dplyr::starts_with("WF"))

    return(b)
}

#' @title fstadv_fwd_dir
#' @description Extract forward direction from forecast/advisory product
#' @param contents Contents of forecast/advisory product.
#' @return integer or NA
#' @keywords internal
fstadv_fwd_dir <- function(contents) {
    fwd_dir <- fstadv_fwd_mvmt(contents, what = 'fwd_dir')
    return(fwd_dir)
}

#' @title fstadv_fwd_mvmt
#' @description Get forward movement direction and speed
#' @details If STATIONARY should return NA
#' @param contents text contents of FORECAST/ADVISORY
#' @param what is being retrieved
#' \itemize{
#'   \item fwd_dir integer azimuth direction of movement (0 - 360)
#'   \item fwd_speed integer speed of movement in kts
#' }
#' @return numeric
#' @keywords internal
fstadv_fwd_mvmt <- function(contents, what = NULL) {

    if (!is.character(what))
        stop('\'what\' must contain \'fwd_dir\' or \'fwd_speed\'')

    ptn <- paste0('PRESENT MOVEMENT TOWARD[A-Z- ]+',
                  '([0-9]{1,3})', # Forward direction
                  '[ ]+DEGREES AT[ ]+',
                  '([0-9]{1,3})', # Forward speed
                  ' KT')

    if (what == 'fwd_dir') {
        return(as.numeric(stringr::str_match(contents, ptn)[,2]))
    } else if (what == 'fwd_speed') {
        return(as.numeric(stringr::str_match(contents, ptn)[,3]))
    } else {
        return(NA)
    }

}

#' @title fstadv_fwd_speed
#' @description Extract forward speed from forecast/advisory product
#' @param contents Contents of forecast/advisory product.
#' @return integer or NA
#' @keywords internal
fstadv_fwd_speed <- function(contents) {
    fwd_speed <- fstadv_fwd_mvmt(contents, what = 'fwd_speed')
    return(fwd_speed)
}

#' @title fstadv_gusts
#' @description Extract wind gusts from a forecast/advisory product.
#' @param contents Contents of forecast/advisory product.
#' @return integer or NA
#' @keywords internal
fstadv_gusts <- function(contents) {
    gust <- fstadv_winds_gusts(contents, what = 'gust')
    return(gust)
}

#' @title fstadv_parse_forecasts
#' @description Breaks forecast field strings into multidimensional list.
#' @details Expects input like:
#'
#'   [1] "FORECAST VALID 10/1200Z 22.6N  85.0W MAX WIND  40 KT GUSTS  50 KT 34 KT 110NE   0SE   0SW   0NW"
#'
#'   [2] "FORECAST VALID 11/0000Z 25.0N  86.3W MAX WIND  45 KT GUSTS  55 KT 34 KT 120NE  70SE   0SW  70NW"
#'
#'   [3] "FORECAST VALID 11/1200Z 27.7N  87.9W MAX WIND  50 KT GUSTS  60 KT 50 KT  25NE  25SE   0SW  25NW 34 KT 130NE  80SE   0SW  80NW"
#'
#'   [4] "FORECAST VALID 12/0000Z 30.5N  88.6W NEAR MS/AL COAST MAX WIND  55 KT GUSTS  65 KT 50 KT  35NE  35SE   0SW  25NW 34 KT 130NE 100SE  50SW  80NW"
#'
#'   [5] "FORECAST VALID 13/0000Z 36.0N  87.0W MAX WIND  20 KT GUSTS  25 KT"
#'
#'   [6] "OUTLOOK VALID 14/0000Z 41.0N  82.5W MAX WIND  15 KT GUSTS  20 KT"
#'
#'   [7] "OUTLOOK VALID 15/0000Z DISSIPATED INLAND"
#'
#' The date, time, lat, lat hemi, lon, lon hemi, max wind, gusts and wind field,
#' if avialable, will be grouped. This function will return a list with each
#' field as it's own variable; for example:
#'
#' [[1]]
#'
#' [,1]                                                                                                [,2] [,3]   [,4]   [,5] [,6]
#'
#' [1,] "FORECAST VALID 10/1200Z 22.6N  85.0W MAX WIND  40 KT GUSTS  50 KT 34 KT 110NE   0SE   0SW   0NW" "10" "1200" "22.6" "N"  "85.0"
#'
#' [,7] [,8] [,9] [,10]
#'
#' [1,] "W"  "40" "50" " 34 KT 110NE   0SE   0SW   0NW"
#'
#' So you can access date in x[[1]][,2], time in x[[1]][,2], etc.
#'
#' There are some cases such as AL051999, Adv 1 that has a 50KT wind field but
#' not a 34KT wind field which is unusual.
#'
#' @param content is a list of forecasts/outlooks extracted from the advisory
#' product.
#' @keywords internal
fstadv_parse_forecasts <- function(content) {

    ptn <- paste0('^[:blank:]*[\n[:alpha:]]+[ ]+VALID[ ]+',
                  '([:digit:]{2})/([:digit:]{2})([:digit:]{2})Z', # Date/Hour/Minute
                  '[ ]+([[:digit:]\\.]{3,4})([N|S])', # Lat
                  '[ ]+([[:digit:]\\.]{3,5})([E|W])', # Lon
                  # Following is option text; don't collect, e.g.:
                  # POST-TROP/EXTRATROP
                  '[:blank:]*(?:[[:alpha:]-/]+)*[:blank:]*',
                  '[:blank:]MAX WIND[ ]+([:digit:]{2,3})[ ]+KT', # Winds
                  '[:blank:]+GUSTS[ ]+([:digit:]{2,3})[ ]+KT', # Gusts
                  '[:blank:]*(?:EXTRATROPICAL)*[:blank:]*', # Optional text; don't collect
                  '[:blank:]*((?:[[:digit:]{2}[ ]+KT[ ]+[:alnum:][:blank:]]+)?$)') # Wind field, if avail

    x <- stringr::str_match_all(content, ptn)

    # Some data has forward and trailing blanks. Remove them
    x <- lapply(x, trimws)

    return(x)

}

#' @title fstadv_pos_accuracy()
#' @description Get position accuracy
#' @param contents text contents of FORECAST/ADVISORY
#' @return numeric
#' @keywords internal
fstadv_pos_accuracy <- function(contents) {
    ptn <- paste0('POSITION ACCURATE WITHIN[ ]+',
                  '([0-9]{2,3})',
                  '[ ]+NM')
    pos_acc <- stringr::str_match(contents, ptn)[,2]
    return(as.numeric(pos_acc))

}

#' @title fstadv_pressure
#' @description Return current minimum central pressure of storm in
#'     millibars (mb)
#' @param contents text contents of FORECAST/ADVISORY product
#' @return numeric
#' @keywords internal
fstadv_pressure <- function(contents) {

    ptn <- paste0('MINIMUM CENTRAL PRESSURE[ ]+',
                  '([0-9]{3,4})', # Pressure
                  '[ ]+MB')
    pressure <- stringr::str_match(contents, ptn)[,2]
    return(as.numeric(pressure))

}

#' @title fstadv_lat
#' @description Extract latitude from forecast/advisory product
#' @param contents Content of forecast/advisory product
#' @return numeric, positive if in northern hemisphere, negative for southern.
#' @keywords internal
fstadv_lat <- function(contents) {
    lat <- fstadv_lat_lon(contents, what = 'lat')
    return(lat)
}

#' @title fstadv_lat_lon
#' @description Returns numeric for latitude or longitude; negative if in
#'     southern or eastern hemisphere
#' @details Helper function to take character latitude or longitude and,
#' depending on the value of hemisphere return a positive or negative numeric,
#' or NA if not found.
#' @param contents text contents of FORECAST/ADVISORY
#' @param what What are we returning? c("lat", "lon")
#' @return numeric
#' @keywords internal
fstadv_lat_lon <- function(contents, what = NULL) {

    if (!is.character(what)) {stop('\'what\' must contain \'lat\' or \'lon\'')}

    ptn <- paste0('[CENTER LOCATED | DISSIPATING] NEAR[ ]+',
                  '([0-9\\.]{3,4})', # Latitude can be 9.9N or 99.9N
                  '([N | S]{1})', # Norhtern meisphere
                  '[ ]+([0-9\\.]{4,5})', #Longitude can be 0 to 180
                  '([E | W]){1}', # Hemisphere
                  '[ ]+')

    x <- stringr::str_match(contents, ptn)

    if (!is.na(x[,2]) & !is.na(x[,3])) {
        if (what == 'lat') {
            lat <- convert_lat_lon(as.numeric(x[,2]), x[,3])
            return(lat)
        } else if (what == 'lon') {
            lon <- convert_lat_lon(as.numeric(x[,4]), x[,5])
            return(lon)
        }
    } else {
        return(NA)
    }

}

#' @title fstadv_lon
#' @description Extract longitude from forecast/advisory product
#' @param contents Content of forecast/advisory product
#' @return numeric, positive if in eastern hemisphere, negative for western.
#' @keywords internal
fstadv_lon <- function(contents) {
    lon <- fstadv_lat_lon(contents, what = 'lon')
    return(lon)
}

#' @title fstadv_seas
#' @description There is only one line of sea data, 12FT seas in each quadrant. So this
#'   should go easier than the wind fields
#' @param content text of product
#' @param wind Wind value of current forecast/advisory product.
#' @return boolean
#' @keywords internal
fstadv_seas <- function(content, wind) {

    # 12 FT SEAS..125NE  90SE  90SW 175NW.
    ptn <- paste0('12 FT SEAS[\\.[:blank:]]+',
                  '([0-9]{1,3})NE',
                  '[ ]+([0-9]{1,3})SE',
                  '[ ]+([0-9]{1,3})SW',
                  '[ ]+([0-9]{1,3})NW')

    x <- stringr::str_match_all(content, ptn)

    # If there is Seas data, continue, otherwise ignore
    if (length(x[[1]]) > 0) {

        # Load into dataframe and get advisory's Key, Adv and ObDate
        #    tmp <- as.data.frame(x[[1]][,2:5])
        df_seas <- tibble::as_data_frame(x[[1]])

        # Rename V2:V5
        df_seas <- df_seas %>%
            dplyr::rename(SeasNE = V2,
                          SeasSE = V3,
                          SeasSW = V4,
                          SeasNW = V5)

        df_seas <- df_seas %>%
            dplyr::select(SeasNE, SeasSE, SeasSW, SeasNW)

        # Reclass vars
        df_seas$SeasNE <- as.numeric(df_seas$SeasNE)
        df_seas$SeasSE <- as.numeric(df_seas$SeasSE)
        df_seas$SeasSW <- as.numeric(df_seas$SeasSW)
        df_seas$SeasNW <- as.numeric(df_seas$SeasNW)

    } else {
        df_seas <- tibble::tibble("SeasNE" = NA,
                                  "SeasSE" = NA,
                                  "SeasSW" = NA,
                                  "SeasNW" = NA)
    }

    return(df_seas)

}

#' @title fstadv_split
#' @description Takes the fstadv dataframe and splits into four relational
#'     dataframes; one for base data, one for wind radius data for the current
#'     product, one dataframe for each forecast position and one dataframe for
#'     wind radius data for each forecast position (within 72 hours).
#' @details The default dataframe for the \code{\link{get_fstadv}} contains 125
#'     columns. This function will take that dataframe and split it out to four
#'     narrow dataframes.
#' @param df Name (character) of dataframe holding default forecast/advisory
#'     data.
#' @param remove Remove original dataframe after cleanup. Default TRUE.
#' @export
fstadv_split <- function(df, remove = TRUE) {

    data <- get(df)

    if (!is.data.frame(data))
        stop("Expecting a dataframe.")

    # Start with extracting base data for forecast/advisory product
    fstadv <- dplyr::select_(data, "Key:Eye", "SeasNE:SeasNW")

    # Collapse wind radius fields to narrow dataframe then expand on the four
    # quadrants, keeping WindField as a variable.
    v <- c("NE", "SE", "SW", "NW")

    fstadv.wr <- purrr::map_df(
        .x = c(34, 50, 64),
        .f = function(y) {
            dplyr::select_(data,
                           .dots = c("Key",
                                     "Adv",
                                     "Date",
                                     paste0(v, y))) %>%
                dplyr::rename_(
                    .dots = list("Key" = "Key",
                                 "Adv" = "Adv",
                                 "Date" = "Date",
                                 "NE" = paste0("NE", y),
                                 "SE" = paste0("SE", y),
                                 "SW" = paste0("SW", y),
                                 "NW" = paste0("NW", y))) %>%
                dplyr::mutate_("WindField" = y)
        }) %>%
        dplyr::select_(.dots = c("Key", "Adv", "Date", "WindField","NE:NW")) %>%
        # Order by Date then Adv since Adv is character. Results as expected.
        dplyr::arrange_("Key", "Date", "Adv", "WindField")

    # Remove NA rows for windfield quadrants
    fstadv.wr <- fstadv.wr[complete.cases(fstadv.wr$NE,
                                          fstadv.wr$SE,
                                          fstadv.wr$SW,
                                          fstadv.wr$NW),]

    # Build forecasts dataframe with base data for each forecast position. This
    # does not include wind radius data; that comes next. This will be similar
    # to fstadv (without seas and some other data points which are never
    # forecast).

    # Extract child dataframe for forecasts date, position, wind and gust
    v <- c("FcstDate", "Lat", "Lon", "Wind", "Gust")

    fstadv.fst <- purrr::map_df(.x = c(12, 24, 36, 48, 72, 96, 120),
                                .f = function(y) {
        dplyr::select_(data,
                       .dots = c("Key", "Adv", "Date", paste0("Hr", y, v))) %>%
            dplyr::rename_("Key" = "Key", "Adv" = "Adv", "Date" = "Date",
                           "FcstDate" = paste0("Hr", y, "FcstDate"),
                           "Lat" = paste0("Hr", y, "Lat"),
                           "Lon" = paste0("Hr", y, "Lon"),
                           "Wind" = paste0("Hr", y, "Wind"),
                           "Gust" = paste0("Hr", y, "Gust"))}) %>%
        dplyr::arrange_("Key", "Date", "Adv", "FcstDate")

    # Remove NA rows
    fstadv.fst <- fstadv.fst[complete.cases(fstadv.fst$FcstDate, fstadv.fst$Lat,
                                            fstadv.fst$Lon, fstadv.fst$Wind,
                                            fstadv.fst$Gust),]

    # Build wind radius dataframe for each forecast position (12:72 hours; 96
    # and 120 hours are never forecasted). This dataframe will be similar to
    # fstadv.wr with the exception of FcstDate.

    v <- c("NE", "SE", "SW", "NW")

    fstadv.fst.wr <- purrr::map_df(.x = c(12, 24, 36, 48, 72),
                                   .f = function(x) {
        y <- purrr::map_df(.x = c(34, 50, 64), .f = function(z) {
            dplyr::select_(data, .dots = c("Key", "Adv", "Date",
                                        paste0("Hr", x, "FcstDate"),
                                        paste0("Hr", x, v, z))) %>%
                dplyr::rename_(.dots = list("Key" = "Key",
                                            "Adv" = "Adv",
                                            "Date" = "Date",
                                            "FcstDate" = paste0("Hr", x,
                                                                "FcstDate"),
                                            "NE" = paste0("Hr", x, "NE", z),
                                            "SE" = paste0("Hr", x, "SE", z),
                                            "SW" = paste0("Hr", x, "SW", z),
                                            "NW" = paste0("Hr", x, "NW", z))) %>%
                dplyr::mutate_("WindField" = z) %>%
                dplyr::select_(.dots = c("Key", "Adv", "Date", "FcstDate",
                                         "WindField", "NE:NW"))})
        return(y)
    })

    fstadv.fst.wr <- fstadv.fst.wr %>% dplyr::arrange_("Key", "Date", "Adv",
                                                       "FcstDate", "WindField")

    fstadv.fst.wr <- fstadv.fst.wr[complete.cases(fstadv.fst.wr$NE,
                                                  fstadv.fst.wr$SE,
                                                  fstadv.fst.wr$SW,
                                                  fstadv.fst.wr$NW),]

    # At this point have all dataframes. Want to return them to global
    # environment while keeping name of original dataframe. So prefix "fstadv"
    # will be replaced by value of df.
    assign(df, fstadv, envir = .GlobalEnv)
    assign(paste0(df, ".fst"), fstadv.fst, envir = .GlobalEnv)
    assign(paste0(df, ".fst.wr"), fstadv.fst.wr, envir = .GlobalEnv)
    assign(paste0(df, ".wr"), fstadv.wr, envir = .GlobalEnv)

    return(TRUE)
}

#' @title fstadv_wind_radius
#' @description Parse wind radius data from product, if exists. This is somewhat tricky as the
#'   wind fields are 64KT, 50KT and 34KT and are listed in descending order. So,
#'   the first line will not always be 64KT, 50KT or even 34KT depending on
#'   strength of storm. What I do here is just extract the entire blob and work
#'   through it. I'll continue to look for ways to improve it.
#'
#' Complimentary to fstadv_get_wind_radius
#'
#' @param contents text of product
#' @return dataframe
#' @keywords internal
fstadv_wind_radius <- function(content, wind) {

    text <- fstadv_wind_radius_regex(content)

    # move to dataframe
    df <- data.frame("text" = text)

    # split out df$text with generic cols for now.
    df <- df %>%
        tidyr::separate(text, into = c(paste0('x', c(1:15))), sep = '\t',
                        fill = 'right') # Suppress warnings

    df[,1:15] <- as.numeric(df[,1:15])

    # The wind fields vary in order with the strongest always being listed
    # first. So 64kt wind field will always be above the 50kt wind field. But
    # if a storm does not have 64 kt winds then 50 kt will be listed first. The
    # easiest way I can think to do this is with if/else statements. This whole
    # function needs to be written anyway. Consider it all brute-force for now.
    # Keep 64kt winds on the left, 50kt in the middle, 34kt on the right.
    if (is.na(wind)) {
        df <- df
    } else if (wind >= 64) {
        df <- df[,c(11:15, 6:10, 1:5)]
    } else if (wind >= 50) {
        df <- df[,c(6:10, 1:5, 11:15)]
    } else if (wind >= 34) {
        df <- df[,c(1:5, 11:15, 6:10)]
    }

    names(df) <- c("WindField34", "NE34", "SE34", "SW34", "NW34",
                   "WindField50", "NE50", "SE50", "SW50", "NW50",
                   "WindField64", "NE64", "SE64", "SW64", "NW64")

    df <- df %>% dplyr::select(-WindField34, -WindField50, -WindField64)

    return(df)

}

#' @title fstadv_wind_radius_regex
#' @description Extra current wind radius from Forecast/Advisory product.
#' @keywords internal
fstadv_wind_radius_regex <- function(content) {
    ptn <- paste0("MAX SUSTAINED WINDS",
                  "[[:blank:][:digit:]]+KT ",
                  "WITH GUSTS TO[[:blank:][:digit:]]+KT",
                  "[\\.[:space:]]*([A-Z0-9\\. \n]+)12 FT SEAS")

    # Do some reformatting to make extraction easier
    a <- stringr::str_replace_all(content, '\n \n', '\t')
    b <- stringr::str_replace_all(a, '\n', ' ')
    c <- stringr::str_replace_all(b, '\t', '\n')
    d <- stringr::str_replace_all(c, '\\.\\.\\.', ' ')
    e <- unlist(stringr::str_match_all(d, ptn))
    # Isolate on key 2
    f <- stringr::str_replace_all(e[2], '\\.', '')
    g <- stringr::str_replace_all(f, '[KT|NE|SE|SW|NW]', '')
    h <- stringr::str_replace_all(trimws(g), '[ ]+', '\t')
    return(h)
}

#' @title fstadv_winds
#' @description Extract current maximum sustained winds from contents
#' @param contents text contents of FORECAST/ADVISORY product
#' @return numeric
#' @keywords internal
fstadv_winds <- function(contents) {
    wind <- fstadv_winds_gusts(contents, what = 'wind')
    return(wind)
}

#' @title fstadv_winds_gusts
#' @description Get winds or gusts in knots (KT)
#' @param contents text contents of FORECAST/ADVISORY product
#' @param what return wind or gust?
#' @return numeric
#' @keywords internal
fstadv_winds_gusts <- function(contents, what = NULL) {

    if (!is.character(what))
        stop('\'what\' must contain \'wind\' or \'gust\'')

    ptn <- paste0('MAX SUSTAINED WINDS[ ]+',
                  '([0-9]{2,3})', # Winds
                  '[ ]+KT WITH GUSTS TO[ ]+',
                  '([0-9]{2,3})', # Gusts
                  '[ ]+KT')

    if (what == 'wind') {
        return(as.numeric(stringr::str_match(contents, ptn)[,2]))
    } else if (what == 'gust') {
        return(as.numeric(stringr::str_match(contents, ptn)[,3]))
    } else {
        return(NA)
    }

}
