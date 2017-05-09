#' @title create_df_fstadv
#' @description Template for public advisory dataframe
#' @return empty dataframe
#' @seealso \code{\link{get_fstadv}}
#' @keywords internal
create_df_fstadv <- function() {
    df <- tibble::data_frame("Status" = character(),
                             "Name" = character(),
                             # Allow for intermediate advisories,
                             # i.e., "1A", "2", "2A"...
                             "Adv" = character(),
                             "Date" = as.POSIXct(character(), tz = "UTC"),
                             "Key" = character(),
                             'Lat' = numeric(),
                             'Lon' = numeric(),
                             'Wind' = numeric(),
                             'Gust' = numeric(),
                             'Pressure' = numeric(),
                             'PosAcc' = numeric(),
                             'FwdDir' = numeric(),
                             'FwdSpeed' = numeric(),
                             'Eye' = numeric())

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
    if (!any(stringr::str_count(contents, c("MIATCMAT", "MIATCMEP"))))
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

    # Add current wind radius
    wind_radius <- fstadv_wind_radius(contents, key, adv, date, wind)

    # Add current sea radius

    # Add forecast positions

    # Add forecast wind radius

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
    # Bind wind_radius
    df <- dplyr::left_join(df, wind_radius,
                           by = c("Key" = "Key", "Adv" = "Adv", "Date" = "Date"))
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
fstadv_wind_radius <- function(content, key, adv, date, wind) {

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

    # move to dataframe
    df <- data.frame('text' = h)

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

    # Add vars to df
    df$Key = key
    df$Adv = adv
    df$Date = date

    names(df) <- c("WindField34", "NE34", "SE34", "SW34", "NW34",
                   "WindField50", "NE50", "SE50", "SW50", "NW50",
                   "WindField64", "NE64", "SE64", "SW64", "NW64",
                   "Key", "Adv", "Date")

    df <- df %>% dplyr::select(-WindField34, -WindField50, -WindField64)

    return(df)

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
