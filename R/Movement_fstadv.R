
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
  ptn <- stringr::str_c("POSITION ACCURATE WITHIN[:blank:]+([0-9]{2,3})[:blank:]+NM")
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

  ptn <- "AT \\d\\d/\\d{4}Z CENTER WAS LOCATED NEAR (\\d\\d\\.\\d)(\\w)\\s+(\\d{1,3}\\.\\d)(\\w)"
  matches <- stringr::str_match(contents, ptn)[,2:5]

  prev_pos_date <- adv_date - lubridate::hours(3)
  prev_pos_lat <- ifelse(matches[2] == "S",
                         as.numeric(matches[1]) * -1,
                         as.numeric(matches[1]))
  prev_pos_lon <- ifelse(matches[4] == "W",
                         as.numeric(matches[3]) * -1,
                         as.numeric(matches[3]))
  prev_pos_date<-tibble::tibble(
    PrevPosDate = prev_pos_date,
    PrevPosLat = prev_pos_lat,
    PrevPosLon = prev_pos_lon)

    split(prev_pos_date, seq(nrow(prev_pos_date)))
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

  df <- stringr::str_match(content, ptn)[,2:5]
  df <- tibble::as_tibble(df, .name_repair = ~paste0("Seas", quads))
  dplyr::mutate(df, dplyr::across(.cols = everything(), as.numeric))
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
fstadv_wind_radius <- function(contents) {

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

  df <- stringr::str_match(contents, ptn) #[2:16]

  df <- df[,2:16] |>
    tibble::as_tibble(.name_repair =
                        ~c("WindField64","NE64", "SE64", "SW64", "NW64",
                           "WindField50", "NE50", "SE50", "SW50", "NW50",
                           "Windfield34", "NE34", "SE34", "SW34", "NW34"))
  df <- df |> dplyr::mutate(dplyr::across(.cols = everything(), as.numeric)) |>
              dplyr::select(-tidyselect::starts_with("WindField"))
print(df)
  df
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

#' @title fstadv_eye
#' @description Get eye diameter, if available
#' @param contents text contents of FORECAST/ADVISORY
#' @return numeric
#' @keywords internal
fstadv_eye <- function(contents) {
  ptn <- stringr::str_c('EYE DIAMETER[ ]+',
                        '([0-9]{2,3})', # Eye diameter, integer
                        '[ ]+NM')
  as.numeric(stringr::str_match(contents, ptn)[,2])
}

#' @title fstadv_forecasts
#' @description Retrieve forecast data from FORECAST/ADVISORY products. Loads
#'   into respective dataframes (df_forecasts, df_forecast_winds)
#' @param content text content of FORECAST/ADVISORY
#' @param key Storm ID
#' @param adv Advisory Number
#' @param adv_date Date value of forecast/advisory product.
#' @return boolean
#' @keywords internal
fstadv_forecasts <- function(contents, key, adv, adv_date) {

  # https://www.nhc.noaa.gov/help/tcm.shtml

  #  Filter forecast dataframe, renaming variables with forecast period as
  #  prefix, eliminate some vars where necessary, and return a filtered
  #  dataframe.
  rebuild_forecasts <- function(hr, df) {

    df <-
      df |>
      dplyr::filter(.data$FcstPeriod == hr) |>
      dplyr::select(
        .data$StormKey,
        .data$Adv,
        .data$FcstDate,
        .data$Lat,
        .data$Lon,
        .data$Wind,
        .data$Gust,
        tidyselect::ends_with("64"),
        tidyselect::ends_with("50"),
        tidyselect::ends_with("34")
      )
    rlang::set_names(
      df,
      # Prepend forecast variables with "Hr", the value of `hr`, and the
      # variable name.
      nm = c(names(df)[1:2], stringr::str_c("Hr", hr, names(df)[3:19]))
    )

    # 64 knot wind radius forecasts are never provided beyond 48 hours.
    # No wind radii data are provided for 96 and 120 hours
    df <-  if (hr %in% c(48, 72)){
      dplyr::select(df, -tidyselect::ends_with("64"))
    } else if (hr %in% c(96, 120)) {
      dplyr::select(
        df, -c(
          tidyselect::ends_with("64"),
          tidyselect::ends_with("50"),
          tidyselect::ends_with("34")
        )
      )
    } else {
      df
    }

  }

  ptn <- stringr::str_c("([:digit:]{2})/([:digit:]{2})([:digit:]{2})Z",
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

  # Extract all forecasts from every text product. Some text products may have
  # multiple forecasts (at 12 hours, 24, 36, 48, 72, and, for more recent years,
  # 96 and 120 hours). Some text products may have no forecasts at all (if the
  # storm is expected to degenerate or already has).


  forecasts <- contents |>
    stringr::str_match_all(pattern = ptn)

  # # Get only the columns needed excluding the matched string
  forecasts <-purrr::map(forecasts, `[`, , 2:22)

  # If any storm has 0 forecasts (i.e., the list element is empty), populate
  # all columns with NA
  forecasts <- purrr::modify_if(forecasts, .p = purrr::is_empty,
                                .f = ~matrix(data = NA_character_, ncol = 21))

  # Convert to tibble cause God I hate working with lists like this though I
  # know I need the practice...)
  forecasts <- forecasts |>
    purrr::map(tibble::as_tibble, .name_repair = ~
                 c( "FcstDate", "Hour", "Minute",
                          "Lat", "LatHemi", "Lon", "LonHemi",
                          "Wind", "Gust", stringr::str_c(quads, "64"),
                        stringr::str_c(quads, "50"),
                        stringr::str_c(quads, "34"))
  )


  forecast_periods <- c(12, 24, 36, 48, 72, 96, 120)

  # The `FcstDate`` conversion in the call below is just inaccurate. The math
  # is wrong!!!

  # Take `key`, `adv`, `adv_date` and add nested tibble `forecasts` as a new
  # dataframe/tibble.
  df_forecasts <-
    tibble::tibble(
      StormKey = key,
      Adv = as.numeric(adv),
      AdvDate = adv_date,
      Forecasts = forecasts
    )
   df_forecasts <-  tidyr::unnest(df_forecasts, cols = c(.data$Forecasts))
   df_forecasts <-  dplyr::group_by(df_forecasts, .data$StormKey, .data$Adv)

    # If the date of the forecast is less than that of the advisory, the forecast
    # period runs into the next month; so need to account for that. Otherwise,
    # the month should be the same.
    #
    # Additionally, though rare, we need to account for storms that generate one
    # year but degenerate the next. There is one instance of an EP cyclone doing
    # this but I cannot recall which one. Oops... So, check for the year as well.
   df_forecasts <-  dplyr::mutate(df_forecasts,
      # Add var for forecast periods, limited to size of each group
      FcstPeriod = forecast_periods[1:dplyr::n()],
      FcstMonth = dplyr::case_when(
        as.numeric(.data$FcstDate) < lubridate::day(.data$AdvDate) ~ lubridate::month(.data$AdvDate) + 1,
        TRUE                                       ~ lubridate::month(.data$AdvDate)
      ),
      FcstYear = dplyr::case_when(
        FcstMonth < lubridate::month(.data$AdvDate) ~ lubridate::year(.data$AdvDate) + 1,
        TRUE                                  ~ lubridate::year(.data$AdvDate)
      ),
      FcstDate = lubridate::ymd_hms(
        strftime(
          paste(
            paste(.data$FcstYear, .data$FcstMonth, .data$FcstDate, sep = "-"),
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
    )

    # Make Wind, Gust, relative wind/gust vars and sea vars all numeric
    df_forecasts <-df_forecasts |>
      dplyr::mutate(
         dplyr::across(c(NE34, SE34, SW34, NW34),
                     .fns = as.numeric)
         )

  df <- rebuild_forecasts(12, df = df_forecasts)

  for (hr in forecast_periods[2:7]) {
    df <-
      df |>
      dplyr::left_join(
        rebuild_forecasts(hr, df = df_forecasts), by = c("StormKey", "Adv")
      )
  }

  df
}

