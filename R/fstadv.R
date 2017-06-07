#' @title get_fstadv
#' @description Return dataframe of forecast/advisory data.
#' @details To disable the progress bar set option dplyr.show_progress to
#'     FALSE. You can also enable messages to show the storm and advisory
#'     currentlby being worked by setting the rrricanes.working_msg to TRUE.
#' @param link URL to storm's archive page.
#' @seealso \code{\link{get_storms}}, \code{\link{public}}
#' @export
get_fstadv <- function(link) {

    # Check status of link(s)
    valid.link <- sapply(link, status)
    valid.link <- stats::na.omit(valid.link)

    # Get all products for the current storm
    products <- purrr::map(valid.link, get_products) %>% purrr::flatten_chr()

    # Filter out fstadv products
    products <- filter_fstadv(products)

    # Set progress bar
    p <- dplyr::progress_estimated(n = length(products))

    # Work products
    products.fstadv <- purrr::map(products, fstadv, p)

    # Build final dataframe
    df <- purrr::map_df(products.fstadv, dplyr::bind_rows)

    return(df)

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
#' @param p dplyr::progress_estimate.
#' @return Dataframe
#' @seealso \code{\link{get_fstadv}}
#' @keywords internal
fstadv <- function(link, p) {

    p$pause(0.5)$tick()$print()

    contents <- scrape_contents(link)

    # Replace all carriage returns with empty string.
    contents <- stringr::str_replace_all(contents, "\r", "")

    # Make sure this is a public advisory product
    if (!any(stringr::str_count(contents,
                                c("MIATCMAT", "MIATCMEP", "[W]*TPA", "TCMAT",
                                  "WTPZ", "HFOTCMEP", "HFOTCMCP"))))
        stop(sprintf("Invalid Forecast/Advisory link. %s", link))

    status <- scrape_header(contents, ret = "status")
    name <- scrape_header(contents, ret = "name")
    adv <- scrape_header(contents, ret = "adv")
    date <- scrape_header(contents, ret = "date")

    if (getOption("rrricanes.working_msg"))
        message(sprintf("Working %s %s Forecast/Advisory #%s (%s)",
                        status, name, adv, date))

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

    df <- tibble::data_frame("Status" = status, "Name" = name, "Adv" = adv,
                             "Date" = date, "Key" = key, "Lat" = lat,
                             "Lon" = lon, "Wind" = wind, "Gust" = gust,
                             "Pressure" = pressure, "PosAcc" = posacc,
                             "FwdDir" = fwd_dir, "FwdSpeed" = fwd_speed,
                             "Eye" = eye)

    # Add current wind radius
    wind_radius <- fstadv_wind_radius(contents, wind)

    df[, names(wind_radius)] <- wind_radius[, names(wind_radius)]

    # Add current sea radius
    seas <- fstadv_seas(contents, wind)
    # AL161999 has two rows of Seas data for advisory 5. The second row is
    # within the forecast area where typically does not exist. Assumption is a
    # typo. Would like to save this into attributes or something...
    if (all(!is.null(seas), nrow(seas) > 1)) {
        warning(sprintf("Too many rows of sea data for %s %s #%s.\n%s",
                        status, name, adv, seas[2:nrow(seas),]),
                call.= FALSE)
        seas <- seas[1,]
    }

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

#' @title fstadv_fcst
#' @description Retrieve forecast data from FORECAST/ADVISORY products. Loads
#'   into respective dataframes (df_forecasts, df_forecast_winds)
#' @param content text content of FORECAST/ADVISORY
#' @param date Date value of current forecast/advisory product.
#' @return boolean
#' @keywords internal
fstadv_forecasts <- function(content, date) {

    ptn <- paste0("([:digit:]{2})/([:digit:]{2})([:digit:]{2})Z",
                  "[:blank:]+([:digit:]{1,2}\\.[:digit:])([N|S])",
                  "[:blank:]+([:digit:]{1,3}\\.[:digit:]{1})([E|W])",
                  "[[:space:][:punct:][:alpha:]]+",
                  "MAX WIND[:blank:]+([:digit:]{1,3})[:blank:]*KT",
                  "[[:blank:][:punct:]]+GUSTS[:blank:]+",
                  "([:digit:]{1,3})[:blank:]*KT[[:space:][:punct:]]+",
                  "(?:64 KT[[:blank:][:punct:]]+",
                  "([:digit:]{1,3})NE",
                  "[:blank:]+([:digit:]{1,3})SE",
                  "[:blank:]+([:digit:]{1,3})SW",
                  "[:blank:]+([:digit:]{1,3})NW",
                  "[[:punct:][:space:]]+)?",
                  "(?:50 KT[[:blank:][:punct:]]+",
                  "([:digit:]{1,3})NE",
                  "[:blank:]+([:digit:]{1,3})SE",
                  "[:blank:]+([:digit:]{1,3})SW",
                  "[:blank:]+([:digit:]{1,3})NW",
                  "[[:punct:][:space:]]+)?",
                  "(?:34 KT[[:blank:][:punct:]]+",
                  "([:digit:]{1,3})NE",
                  "[:blank:]+([:digit:]{1,3})SE",
                  "[:blank:]+([:digit:]{1,3})SW",
                  "[:blank:]+([:digit:]{1,3})NW",
                  "[[:punct:][:space:]]+)?")

    ds <- stringr::str_match_all(content, pattern = ptn)

    # If no forecasts, exit gracefully
    if (all(purrr::map_lgl(ds, purrr::is_empty)))
        return(NULL)

    df <- purrr::map_df(ds, tibble::as_data_frame)

    df$V1 <- NULL

    quads <- c("NE", "SE", "SW", "NW")

    df_names <- c("Date", "Hour", "Minute", "Lat", "LatHemi", "Lon", "LonHemi",
                  "Wind", "Gust", paste0(quads, "64"), paste0(quads, "50"),
                  paste0(quads, "34"))

    names(df) <- df_names

    df <- df %>%
        dplyr::mutate_at(.cols = dplyr::vars(Date:Lat, Lon, Wind:NW34),
                         .funs = as.numeric)

    # Since we have no month or year must do some calculations. If forecast day
    # is lower than current day then advance month by 1. If forecast month is
    # less than current month, then advance year by one **and subtract 12 from
    # month**. Otherwise, month will be number 13 which of course is invalid.

    # Get current observation date
    ob_year <- lubridate::year(date)
    ob_month <- lubridate::month(date)
    ob_day <- lubridate::day(date)

    # Here I account for the possibility of the forecast date crossing months
    # or even years
    df <- df %>%
        dplyr::mutate(Month = dplyr::if_else(Date < ob_day,
                                             ob_month + 1,
                                             ob_month),
                      Year = dplyr::if_else(Month < ob_month,
                                            ob_year + 1,
                                            ob_year))

    # In case of storms like Zeta (AL302005) where storm existed across multiple
    # years, make sure the calculation above does not give a month greater than
    # 12. If so, substract 12. In otherwords, going from December to January.
    df <- df %>% dplyr::mutate(Month = dplyr::if_else(Month > 12,
                                                      Month - 12,
                                                      Month))

    # Build FcstDate variable
    df <- df %>%
        dplyr::mutate(FcstDate = lubridate::ymd_hm(paste(paste(Year, Month, Date,
                                                               sep = '-'),
                                                         paste(Hour, Minute,
                                                               sep = ':'),
                                                         sep = ' ')))

    # Clean up Latitude, Longitude
    df <- df %>%
        dplyr::mutate(Lat = ifelse(LatHemi == 'S', Lat * -1, Lat),
                      Lon = ifelse(LonHemi == 'W', Lon * -1, Lon))

    # Rearrange, drop some vars
    df <- df %>%
        dplyr::select_(.dots = dplyr::vars(FcstDate, Lat, Lon, Wind:NW34))

    df_names <- names(df)

    fcst_periods <- paste0("Hr", c(12, 24, 36, 48, 72, 96, 120))
    # Modify fcst_periods to lengt of df
    fcst_periods <- fcst_periods[1:nrow(df)]

    df <- df %>% split(.$FcstDate)

    df <- purrr::map2(1:length(df),
                      fcst_periods,
                      function(a, b) {
                          stats::setNames(df[[a]], paste0(b, names(df[[a]])))
                          })

    df <- dplyr::bind_cols(df)

    return(df)
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

    ptn <- paste0("PRESENT MOVEMENT TOWARD[[:alpha:][:punct:][:space:]]+",
                  "([:digit:]{1,3})[:blank:]+DEGREES AT[:blank:]+",
                  "([:digit:]{1,3})[:blank:]KT")

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

#' @title fstadv_pos_accuracy()
#' @description Get position accuracy
#' @param contents text contents of FORECAST/ADVISORY
#' @return numeric
#' @keywords internal
fstadv_pos_accuracy <- function(contents) {
    ptn <- paste0("POSITION ACCURATE WITHIN[:blank:]+([0-9]{2,3})[:blank:]+NM")
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
    ptn <- paste0('MINIMUM CENTRAL PRESSURE[:blank:]+([:digit:]{3,4})[:blank:]*MB')
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

    ptn <- paste0("[CENTER LOCATED | DISSIPATING] NEAR[:blank:]+",
                  "([0-9\\.]{3,4})", # Latitude can be 9.9N or 99.9N
                  "([N|S]{1})", # Northern meisphere
                  "[:blank:]+([0-9\\.]{4,5})", #Longitude can be 0 to 180
                  "([E|W]){1}", # Hemisphere
                  "[:blank:]+")

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
    ptn <- paste0("12 FT SEAS",
                  "[[:punct:][:blank:]]+([0-9]{1,3})NE",
                  "[:blank:]+([0-9]{1,3})SE",
                  "[:blank:]+([0-9]{1,3})SW",
                  "[:blank:]+([0-9]{1,3})NW")

    x <- stringr::str_match_all(content, ptn)
    # If there is Seas data, continue, otherwise ignore
    if (purrr::is_empty(x[[1]])) return(NULL)
    df <- tibble::as_data_frame(x[[1]])
    df_names <- paste0("Seas", c("NE", "SE", "SW", "NW"))
    names(df)[2:5] <- df_names
    df <- df %>%
        dplyr::mutate_at(df_names, .funs = as.numeric) %>%
        dplyr::select_(.dots = df_names)
    return(df)
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

    # Bail if no wind data
    if (purrr::is_empty(text)) return(NULL)

    df <- tibble::as_tibble(t(text))

    names(df) <- c("WindField64", "NE64", "SE64", "SW64", "NW64",
                   "WindField50", "NE50", "SE50", "SW50", "NW50",
                   "WindField34", "NE34", "SE34", "SW34", "NW34")

    df <- df %>% dplyr::select(-WindField34, -WindField50, -WindField64)

    return(df)

}

#' @title fstadv_wind_radius_regex
#' @description Extra current wind radius from Forecast/Advisory product.
#' @keywords internal
fstadv_wind_radius_regex <- function(content) {
    ptn <- paste0("MAX SUSTAINED WINDS[:blank:]+[:digit:]{1,3} KT ",
                  "WITH GUSTS TO[:blank:]+[:digit:]{1,3} KT[[:punct:][:space:][:upper:]]+",
                  "(?:(64) KT[[:blank:][:punct:]]+([:digit:]{1,3})NE[:blank:]+([:digit:]{1,3})SE[:blank:]+([:digit:]{1,3})SW[:blank:]+([:digit:]{1,3})NW[[:punct:][:space:]]+)?",
                  "(?:(50) KT[[:blank:][:punct:]]+([:digit:]{1,3})NE[:blank:]+([:digit:]{1,3})SE[:blank:]+([:digit:]{1,3})SW[:blank:]+([:digit:]{1,3})NW[[:punct:][:space:]]+)?",
                  "(?:(34) KT[[:blank:][:punct:]]+([:digit:]{1,3})NE[:blank:]+([:digit:]{1,3})SE[:blank:]+([:digit:]{1,3})SW[:blank:]+([:digit:]{1,3})NW[[:punct:][:space:]]+)?")
    x <- stringr::str_match_all(content, ptn)
    return(as.numeric(x[[1]][,2:16]))
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

#' @title tidy_fstadv
#' @description Tidy current details of a fstadv dataframe object.
#' @param df fstadv dataframe object
#' @details Use Key, Adv and Date to join with other tidy dataframes.
#' @export
tidy_fstadv <- function(df) {
    if (!is.data.frame(df))
        stop("Expecting a dataframe.")
    df <- dplyr::select_(df, "Key", "Adv:Date", "Status:Name", "Lat:Eye",
                         "SeasNE:SeasNW")
    return(df)
}

#' @title tidy_wr
#' @description Tidy current wind radius of a fstadv dataframe object.
#' @param df fstadv dataframe object
#' @details Takes current wind radius variables in a fstadv dataframe object,
#' tidys the data and removes rows where all values are NA. Use Key, Adv and
#' Date to join with other tidy dataframes.
#' @export
tidy_wr <- function(df) {
    if (!is.data.frame(df))
        stop("Expecting a dataframe.")

    # Collapse wind radius fields to narrow dataframe then expand on the four
    # quadrants, keeping WindField as a variable.
    v <- c("NE", "SE", "SW", "NW")

    df <- purrr::map_df(
        .x = c(34, 50, 64),
        .f = function(y) {
            dplyr::select_(df,
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
    df <- df[complete.cases(df$NE, df$SE, df$SW, df$NW),]

    return(df)
}

#' @title tidy_fcst
#' @description Tidy forecasts of a fstadv dataframe object.
#' @param df fstadv dataframe object
#' @details Gathers all forecast points, converts to narrow dataframe and
#' removes rows that are NA. Use Key, Adv and Date to join with other tidy
#' dataframes.
#' @export
tidy_fcst <- function(df) {
    if (!is.data.frame(df))
        stop("Expecting a dataframe.")

    # Build forecasts dataframe with base data for each forecast position. This
    # does not include wind radius data; that comes next. This will be similar
    # to fstadv (without seas and some other data points which are never
    # forecast).

    # Extract child dataframe for forecasts date, position, wind and gust
    v <- c("FcstDate", "Lat", "Lon", "Wind", "Gust")

    df <- purrr::map_df(.x = c(12, 24, 36, 48, 72, 96, 120),
                                .f = function(y) {
                                    dplyr::select_(df,
                                                   .dots = c("Key", "Adv", "Date", paste0("Hr", y, v))) %>%
                                        dplyr::rename_("Key" = "Key", "Adv" = "Adv", "Date" = "Date",
                                                       "FcstDate" = paste0("Hr", y, "FcstDate"),
                                                       "Lat" = paste0("Hr", y, "Lat"),
                                                       "Lon" = paste0("Hr", y, "Lon"),
                                                       "Wind" = paste0("Hr", y, "Wind"),
                                                       "Gust" = paste0("Hr", y, "Gust"))}) %>%
        dplyr::arrange_("Key", "Date", "Adv", "FcstDate")

    # Remove NA rows
    df <- df[complete.cases(df$FcstDate, df$Lat, df$Lon, df$Wind, df$Gust),]
    return(df)
}

#' @title tidy_fcst_wr
#' @description Tidy forecast wind radii of a fstadv dataframe object
#' @param df fstadv dataframe object
#' @details Converts forecast wind radii fields to narrow, tidy dataframe. Use
#' Key, Adv and Date to join with other tidy dataframes.
#' @export
tidy_fcst_wr <- function(df) {
    if (!is.data.frame(df))
        stop("Expecting a dataframe.")

    # Build wind radius dataframe for each forecast position (12:72 hours; 96
    # and 120 hours are never forecasted). This dataframe will be similar to
    # fstadv.wr with the exception of FcstDate.

    v <- c("NE", "SE", "SW", "NW")

    df <- purrr::map_df(.x = c(12, 24, 36, 48, 72),
                                   .f = function(x) {
                                       y <- purrr::map_df(.x = c(34, 50, 64), .f = function(z) {
                                           dplyr::select_(df, .dots = c("Key", "Adv", "Date",
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

    df <- df %>% dplyr::arrange_("Key", "Date", "Adv",
                                                       "FcstDate", "WindField")

    df <- df[complete.cases(df$NE, df$SE, df$SW, df$NW),]
    return(df)
}