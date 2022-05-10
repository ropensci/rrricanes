
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

  df <- stringr::str_match(content, ptn)[,2:5] |>
    apply(MARGIN = 2L, FUN = as.numeric) |>
    tibble::as_tibble(.name_repair = "minimal") |>
    rlang::set_names(nm = stringr::str_c("Seas", c("NE", "SE", "SW", "NW")))
  split(df, seq(nrow(df)))
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

  df <- stringr::str_match(content, ptn)[,2:16] |>
    apply(MARGIN = 2L, FUN = as.numeric) |>
    tibble::as_tibble(.name_repair = "minimal") |>
    rlang::set_names(nm = c("WindField64", "NE64", "SE64", "SW64", "NW64",
                            "WindField50", "NE50", "SE50", "SW50", "NW50",
                            "WindField34", "NE34", "SE34", "SW34", "NW34")) |>
    dplyr::select(-tidyselect::starts_with("WindField"))
  split(df, seq(nrow(df)))
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
