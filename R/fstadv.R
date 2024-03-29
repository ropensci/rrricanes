#' @title get_fstadv
#' @description Return dataframe of forecast/advisory data.
#' @param links URL to storms' archive page.
#' @details Returns a wide dataframe of most the data available in a cyclones
#' forecast/advisory product (watches and warnings are not included at this
#' time).
#'
#' Overall structure of the dataframe is listed below. Note the following
#' clarifications:
#'
#' \enumerate{
#'   \item The value of `n` in `Hr\{n\}` variables is the forecast period.
#'     Up to 2002, forecast periods are 12, 24, 36, 48 and 72 hours. After
#'     2002, forecast periods were extended to 96 and 120 hours. Not all
#'     forecast periods will be available for every cyclone advisory (e.g.,
#'     if it is dissipating or expected to dissipate.)
#'   \item Wind radius data is not included 96 and 120 hour forecast periods.
#'   \item Forecast dates are not truly 12, 24, ..., 120 hours from the
#'     date/time of the advisory. The NHC issues two positions in these
#'     products; one for current and one for three hours prior. It is the
#'     latter position the forecast date/times are based.
#' }
#'
#' \describe{
#'  \item{Status}{Classification of cyclone}
#'  \item{Name}{Name of cyclone}
#'  \item{Adv}{Advisory number}
#'  \item{DateTime}{Date and time of advisory}
#'  \item{StormKey}{Unique identifier of cyclone}
#'  \item{Lat}{Latitude of cyclone center}
#'  \item{Lon}{Longitude of cyclone center}
#'  \item{Wind}{Maximum sustained one-minute winds in knots}
#'  \item{Gust}{Maximum sustained one-minute gusts in knots}
#'  \item{Pressure}{Minimum central pressure in millibars}
#'  \item{PosAcc}{Position accuracy of cyclone in nautical miles}
#'  \item{FwdDir}{Compass angle of forward motion}
#'  \item{FwdSpeed}{Forward speed in miles per hour}
#'  \item{Eye}{Size of eye in nautical miles}
#'  \item{NE64}{Radius of >=64kt winds in northeast quadrant}
#'  \item{SE64}{Radius of >=64kt winds in southeast quadrant}
#'  \item{SW64}{Radius of >=64kt winds in southwest quadrant}
#'  \item{NW64}{Radius of >=64kt winds in northwest quadrant}
#'  \item{NE50}{Radius of >=50kt winds in northeast quadrant}
#'  \item{SE50}{Radius of >=50kt winds in southeast quadrant}
#'  \item{SW50}{Radius of >=50kt winds in southwest quadrant}
#'  \item{NW50}{Radius of >=50kt winds in northwest quadrant}
#'  \item{NE34}{Radius of >=34kt winds in northwest quadrant}
#'  \item{SE34}{Radius of >=34kt winds in southeast quadrant}
#'  \item{SW34}{Radius of >=34kt winds in southwest quadrant}
#'  \item{NW34}{Radius of >=34kt winds in northwest quadrant}
#'  \item{Hr\{n\}FcstDate}{Forecast valid date}
#'  \item{Hr\{n\}Lat}{Forecast latitude in `n` hours}
#'  \item{Hr\{n\}Lon}{Forecast longitude in `n` hours}
#'  \item{Hr\{n\}Wind}{Forecast maximum wind in `n` hours}
#'  \item{Hr\{n\}Gust}{Forecast maximum gust in `n` hours}
#'  \item{Hr\{n\}NE64}{Forecast wind radius in `n` hours}
#'  \item{Hr\{n\}SE64}{Forecast wind radius in `n` hours}
#'  \item{Hr\{n\}SW64}{Forecast wind radius in `n` hours}
#'  \item{Hr\{n\}NW64}{Forecast wind radius in `n` hours}
#'  \item{Hr\{n\}NE50}{Forecast wind radius in `n` hours}
#'  \item{Hr\{n\}SE50}{Forecast wind radius in `n` hours}
#'  \item{Hr\{n\}SW50}{Forecast wind radius in `n` hours}
#'  \item{Hr\{n\}NW50}{Forecast wind radius in `n` hours}
#'  \item{Hr\{n\}NE34}{Forecast wind radius in `n` hours}
#'  \item{Hr\{n\}SE34}{Forecast wind radius in `n` hours}
#'  \item{Hr\{n\}SW34}{Forecast wind radius in `n` hours}
#'  \item{Hr\{n\}NW34}{Forecast wind radius in `n` hours}
#'  \item{SeasNE}{Radius of 12ft seas in northeast quadrant}
#'  \item{SeasSE}{Radius of 12ft seas in southeast quadrant}
#'  \item{SeasSW}{Radius of 12ft seas in southwest quadrant}
#'  \item{SeasNW}{Radius of 12ft seas in northwest quadrant}
#' }
#' @seealso \code{\link{tidy_adv}}, \code{\link{tidy_wr}},
#' \code{\link{tidy_fcst}}, \code{\link{tidy_fcst_wr}}
#' @examples
#' \dontrun{
#' # Return dataframe of forecast/advisories for Tropical Storm Alex (AL011998)
#' get_fstadv("http://www.nhc.noaa.gov/archive/1998/1998ALEXadv.html")
#' }
#' @export
get_fstadv <- function(links) {
  get_product(links = links, products = "fstadv")
}

#' @title fstadv
#' @description Extrapolate data from FORECAST/ADVISORY products.
#' @details Given a direct link to a forecast/advisory product, parse and
#'    return data frame of values.
#' @param contents URL of a specific FORECAST/ADVISORY product
#' @return Data frame of values
#' @keywords internal
fstadv <- function(contents) {

  status <- scrape_header(
    contents = contents,
    # The "SPECIAL" pattern has to be left here; moving it under
    # `scrape_header` will break posest and update products.
    ptn_product_title = "(?:\n?SPECIAL\\s+)?(?:FORECAST/|MARINE\\s+)?(?:ADVISO
    RY)?"
  )

  issue_date <- scrape_date(contents)
  key <- scrape_key(contents)
  lat_lon <- fstadv_lat_lon(contents)
  posacc <- fstadv_pos_accuracy(contents)
  fwd_mvmt <- fstadv_fwd_mvmt(contents)
  pressure <- fstadv_pressure(contents)
  eye <- fstadv_eye(contents)
  winds_gusts <- fstadv_winds_gusts(contents)
  wind_radius <- fstadv_wind_radius(contents)
  prev_pos <- fstadv_prev_pos(contents, issue_date)
  seas <- fstadv_seas(contents)

  forecasts <- fstadv_forecasts(contents, key, status[3], issue_date)
  fstadv.data <-
    tibble::tibble(
      Status = status[1],
      Name = status[2],
      Adv = as.numeric(status[3]),
      Date = issue_date,
      StormKey = key,

      Adv = as.numeric(adv),
      AdvDate = adv_date,
      Forecasts = forecasts
    ) |>
    tidyr::unnest() |>
    dplyr::group_by(.data$StormKey, .data$Adv) |>

    # If the date of the forecast is less than that of the advisory, the forecast
    # period runs into the next month; so need to account for that. Otherwise,
    # the month should be the same.
    #
    # Additionally, though rare, we need to account for storms that generate one
    # year but degenerate the next. There is one instance of an EP cyclone doing
    # this but I cannot recall which one. Oops... So, check for the year
    #as well.
    dplyr::mutate(
      # Add var for forecast periods, limited to size of each group
      FcstPeriod <- forecast_periods[1:dplyr::n()],
      FcstMonth <- dplyr::case_when(
        as.numeric(.data$Date) < lubridate::day(.data$AdvDate) ~
              lubridate::month(.data$AdvDate) + 1,
        TRUE ~ lubridate::month(.data$AdvDate)
      ),
      FcstYear = dplyr::case_when(
        FcstMonth < lubridate::month(.data$AdvDate) ~
          lubridate::year(.data$AdvDate) + 1,
        TRUE                                  ~ lubridate::year(.data$AdvDate)
      ),
      FcstDate = lubridate::ymd_hms(
        strftime(
          paste(
            paste(.data$FcstYear, .data$FcstMonth, .data$Date, sep = "-"),
            paste(.data$Hour, .data$Minute, "00", sep = ":"),
            sep = " "
          ),
          format = "%Y-%m-%d %H:%M:%S"
        )
      ),
      # If Lat is in southern hemisphere (unlikely, but possible), make negative
      Lat = dplyr::case_when(
        LatHemi == "S" ~ as.numeric(.data$Lat) * -1,
        TRUE           ~ as.numeric(.data$Lat)
      ),
      # If Lon in western hemisphere (most likely), make negative.
      Lon = dplyr::case_when(
        LonHemi == "W" ~ as.numeric(.data$Lon) * -1,
        TRUE           ~ as.numeric(.data$Lon)
      )
    ) |>
    # Make Wind, Gust, relative wind/gust vars and sea vars all numeric
    dplyr::mutate_at(dplyr::vars(.data$Wind:.data$NW34), .funs = as.numeric)

  df <- rebuild_forecasts(12, df = df_forecasts)

  for (hr in forecast_periods[2:7]) {
    df <-
      df |>
      dplyr::left_join(
        rebuild_forecasts(hr, df = df_forecasts), by = c("StormKey", "Adv")
      )
  }


  df |>
    dplyr::ungroup() |>
    dplyr::select(-c(.data$StormKey, .data$Adv)) |>

    split(seq(nrow(.)))

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

  ptn <- stringr::str_c("PRESENT MOVEMENT TOWARD[[:alpha:][:punct:][:space:]]+",
                        "([:digit:]{1,3})[:blank:]+DEGREES AT[:blank:]+",
                        "([:digit:]{1,3})[:blank:]KT")

  matches <- stringr::str_match(contents, ptn)
  matrix(data = c(as.numeric(matches[,2]), as.numeric(matches[,3])), ncol = 2L)

}

#' @title fstadv_pos_accuracy()
#' @description Get position accuracy
#' @param contents text contents of FORECAST/ADVISORY
#' @return numeric
#' @keywords internal
fstadv_pos_accuracy <- function(contents) {
  ptn <- stringr::str_c("POSITION ACCURATE WITHIN[:blank:]+([0-9]{2,3})[:blank
                        :]+NM")
  as.numeric(stringr::str_match(contents, ptn)[,2])
}

#' @title fstadv_pressure
#' @description Return current minimum central pressure of storm in
#'   millibars (mb)
#' @param contents text contents of FORECAST/ADVISORY product
#' @return numeric
#' @keywords internal
fstadv_pressure <- function(contents) {
  ptn <- stringr::str_c("MINIMUM CENTRAL PRESSURE[:blank:]+",
                        "([:digit:]{3,4})[:blank:]*MB")
  as.numeric(stringr::str_match(contents, ptn)[,2])
}

#' @title fstadv_prev_pos
#' @description Get storm's previous position
#' @keywords internal
fstadv_prev_pos <- function(contents, adv_date) {

  ptn <- "AT \\d\\d/\\d{4}Z CENTER WAS LOCATED NEAR (\\d\\d\\.\\d)(\\w)\\s+(\\
  d{1,3}\\.\\d)(\\w)"
  matches <- stringr::str_match(contents, ptn)[,2:5]

  prev_pos_date <- adv_date - lubridate::hours(3)
  prev_pos_lat <- ifelse(matches[,2] == "S",
                         as.numeric(matches[,1]) * -1,
                         as.numeric(matches[,1]))
  prev_pos_lon <- ifelse(matches[,4] == "W",
                         as.numeric(matches[,3]) * -1,
                         as.numeric(matches[,3]))
  tibble::tibble(
    PrevPosDate = prev_pos_date,
    PrevPosLat = prev_pos_lat,
    PrevPosLon = prev_pos_lon) |>
    split(seq(nrow(.)))
}

#' @title fstadv_lat_lon
#' @description Returns numeric for latitude or longitude; negative if in
#'   southern or eastern hemisphere
#' @details Helper function to take character latitude or longitude and,
#' depending on the value of hemisphere return a positive or negative numeric,
#' or NA if not found.
#' @param contents text contents of FORECAST/ADVISORY
#' @return numeric
#' @keywords internal
fstadv_lat_lon <- function(contents) {

  ptn <- stringr::str_c("[CENTER LOCATED | DISSIPATING] NEAR[:blank:]+",
                        "([0-9\\.]{3,4})", # Latitude can be 9.9N or 99.9N
                        "([N|S]{1})", # Northern meisphere
                        "[:blank:]+([0-9\\.]{4,5})", #Longitude can be 0 to 180
                        "([E|W]){1}", # Hemisphere
                        "[:blank:]+")

  matches <- stringr::str_match(contents, ptn)

  lat <- ifelse(matches[, 3] == "S",
                as.numeric(matches[, 2]) * -1,
                as.numeric(matches[, 2]) * 1)

  lon <- ifelse(matches[, 5] == "W",
                as.numeric(matches[, 4]) * -1,
                as.numeric(matches[, 4]) * 1)

  matrix(data = c(lat, lon), ncol = 2)
}

#' @title fstadv_seas
#' @description There is only one line of sea data, 12FT seas in each quadrant.
#' So this should go easier than the wind fields
#' @param content text of product
#' @return boolean
#' @keywords internal
fstadv_seas <- function(content) {

  # 12 FT SEAS..125NE  90SE  90SW 175NW.
  ptn <- stringr::str_c("12 FT SEAS",
                        "[[:punct:][:blank:]]+([0-9]{1,3})NE",
                        "[:blank:]+([0-9]{1,3})SE",
                        "[:blank:]+([0-9]{1,3})SW",
                        "[:blank:]+([0-9]{1,3})NW")

  stringr::str_match(content, ptn)[,2:5] |>
    apply(MARGIN = 2L, FUN = as.numeric) |>
    tibble::as_tibble(.name_repair = "minimal") |>
    rlang::set_names(nm = stringr::str_c("Seas", c("NE", "SE", "SW", "NW"))) |>
    split(seq(nrow(.)))
}

#' @title fstadv_wind_radius
#' @description Parse wind radius data from product, if exists. This is somewhat
#' tricky as the wind fields are 64KT, 50KT and 34KT and are listed in
#' descending order. So the first line will not always be 64KT, 50KT or even
#' 34KT depending on strength of storm. What I do here is just extract the
#' entire blob and work through it. I'll continue to look for ways to improve
#' it.
#'
#' Complimentary to fstadv_get_wind_radius
#'
#' @param contents text of product
#' @return dataframe
#' @keywords internal
fstadv_wind_radius <- function(content) {

  ptn <- stringr::str_c("MAX SUSTAINED WINDS[:blank:]+[:digit:]{1,3} KT ",
                        "WITH GUSTS TO[:blank:]+[:digit:]{1,3} ",
                        "KT[[:punct:][:space:][:upper:]]+",
                        "(?:(64) KT[[:blank:][:punct:]]+([:digit:]{1,3})",
                        "NE[:blank:]+([:digit:]{1,3})",
                        "SE[:blank:]+([:digit:]{1,3})",
                        "SW[:blank:]+([:digit:]{1,3})",
                        "NW[[:punct:][:space:]]+)?",
                        "(?:(50) KT[[:blank:][:punct:]]+([:digit:]{1,3})",
                        "NE[:blank:]+([:digit:]{1,3})",
                        "SE[:blank:]+([:digit:]{1,3})",
                        "SW[:blank:]+([:digit:]{1,3})",
                        "NW[[:punct:][:space:]]+)?",
                        "(?:(34) KT[[:blank:][:punct:]]+([:digit:]{1,3})",
                        "NE[:blank:]+([:digit:]{1,3})",
                        "SE[:blank:]+([:digit:]{1,3})",
                        "SW[:blank:]+([:digit:]{1,3})",
                        "NW[[:punct:][:space:]]+)?")

  stringr::str_match(content, ptn)[,2:16] |>
    apply(MARGIN = 2L, FUN = as.numeric) |>
    tibble::as_tibble(.name_repair = "minimal") |>
    rlang::set_names(nm = c("WindField64", "NE64", "SE64", "SW64", "NW64",
                            "WindField50", "NE50", "SE50", "SW50", "NW50",
                            "WindField34", "NE34", "SE34", "SW34", "NW34")) |>
    dplyr::select(-tidyselect::starts_with("WindField")) |>
    split(seq(nrow(.)))
}

#' @title fstadv_winds_gusts
#' @description Get winds or gusts in knots (KT)
#' @param contents text contents of FORECAST/ADVISORY product
#' @return numeric
#' @keywords internal
fstadv_winds_gusts <- function(contents) {

  ptn <- stringr::str_c('MAX SUSTAINED WINDS[ ]+',
                        '([0-9]{2,3})', # Winds
                        '[ ]+KT WITH GUSTS TO[ ]+',
                        '([0-9]{2,3})', # Gusts
                        '[ ]+KT')

  matches <- stringr::str_match(contents, ptn)

  matrix(data = c(as.numeric(matches[,2]), as.numeric(matches[,3])), ncol = 2L)

}

