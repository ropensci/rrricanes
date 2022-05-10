#' @title tidy_adv
#' @description Tidy current details of a fstadv dataframe object.
#' @param df fstadv dataframe object
#' @details Returns current data only of a fstadv dataframe. Use Key, Adv and
#' Date to join with other tidy dataframes.
#' \describe{
#'  \item{Key}{Unique identifier of cyclone}
#'  \item{Adv}{Advisory number}
#'  \item{Date}{Date and time of advisory}
#'  \item{Status}{Classification of cyclone}
#'  \item{Name}{Name of cyclone}
#'  \item{Lat}{Latitude of cyclone center}
#'  \item{Lon}{Longitude of cyclone center}
#'  \item{Wind}{Maximum sustained one-minute winds in knots}
#'  \item{Gust}{Maximum sustained one-minute gusts in knots}
#'  \item{Pressure}{Minimum central pressure in millibars}
#'  \item{PosAcc}{Position accuracy of cyclone in nautical miles}
#'  \item{FwdDir}{Compass angle of forward motion}
#'  \item{FwdSpeed}{Forward speed in miles per hour}
#'  \item{Eye}{Size of eye in nautical miles}
#'  \item{SeasNE}{Radius of 12ft seas in northeast quadrant}
#'  \item{SeasSE}{Radius of 12ft seas in southeast quadrant}
#'  \item{SeasSW}{Radius of 12ft seas in southwest quadrant}
#'  \item{SeasNW}{Radius of 12ft seas in northwest quadrant}
#' }
#' @examples
#' \dontrun{
#' get_fstadv("http://www.nhc.noaa.gov/archive/1998/1998ALEXadv.html") %>%
#'   tidy_adv()
#' }
#' @export
tidy_adv <- function(df) {
  if (!is.data.frame(df))
    stop("Expecting a dataframe.")
  df <- df |>
    dplyr::select(
      "Key",
      .data$Adv:.data$Date,
      .data$Status:.data$Name,
      .data$Lat:.data$Eye,
      dplyr::starts_with("Seas"))
  return(df)
}

#' @title tidy_adv
#' @description \code{tidy_adv} will be deprecated in 0.2.2
#' @rdname tidy_adv
#' @export
tidy_fstadv <- function(df) {
  .Deprecated("tidy_adv",
              msg = "`tidy_fstadv is deprecated and will be removed in v0.2.2")
  tidy_adv(df)
}

#' @title tidy_wr
#' @description Tidy current wind radius of a fstadv dataframe object.
#' @param df fstadv dataframe object
#' @details Returns tidy dataframe of current wind radius values for a cyclone.
#' Returns only complete.cases (based on quadrants).
#' \describe{
#'  \item{Key}{Unique identifier of cyclone}
#'  \item{Adv}{Advisory number}
#'  \item{Date}{Date and time of advisory}
#'  \item{Windfield}{Minimum wind speed expected}
#'  \item{NE}{Radius of `Windfield` in the northeast quadrant}
#'  \item{SE}{Radius of `Windfield` in the southeast quadrant}
#'  \item{SW}{Radius of `Windfield` in the southwest quadrant}
#'  \item{NW}{Radius of `Windfield` in the northwest quadrant}
#' }
#' @examples
#' \dontrun{
#' get_fstadv("http://www.nhc.noaa.gov/archive/1998/1998ALEXadv.html") %>%
#'   tidy_wr()
#' }
#' @export
tidy_wr <- function(df) {
  if (!is.data.frame(df))
    stop("Expecting a dataframe.")

  # Collapse wind radius fields to narrow dataframe then expand on the four
  # quadrants, keeping WindField as a variable.
  v <- c("NE", "SE", "SW", "NW")

  wr <- purrr::map_df(
    .x = c(34, 50, 64),
    .f = function(y) {

      df <-  dplyr::select(df, c("Key", "Adv", "Date", paste0(v, y)))
      df <-   dplyr::rename(
          "Key" = "Key",
          "Adv" = "Adv",
          "Date" = "Date",
          "NE" = paste0("NE", y),
          "SE" = paste0("SE", y),
          "SW" = paste0("SW", y),
          "NW" = paste0("NW", y))
        df <- dplyr::mutate("WindField" = y)
    }) |>
    dplyr::select(c(
      "Key", "Adv", "Date", "WindField", .data$NE:.data$NW
    )) |>
    # Order by Date then Adv since Adv is character. Results as expected.
    dplyr::arrange(.data$Key, .data$Date, .data$Adv, .data$WindField)

  # Remove NA rows for windfield quadrants
  wr <- wr[stats::complete.cases(wr$NE, wr$SE, wr$SW, wr$NW),]

  return(wr)
}

#' @title tidy_fcst
#' @description Tidy forecasts of a fstadv dataframe object.
#' @param df fstadv dataframe object
#' @details Gathers all forecast points, tidies dataframe to make one row per
#' forecast position. Complete cases only. Use Key, Adv and Date to join with
#' other tidy dataframes.
#'
#' \describe{
#'  \item{Key}{Unique identifier of cyclone}
#'  \item{Adv}{Advisory number}
#'  \item{Date}{Date and time of advisory}
#'  \item{FcstDate}{Forecast date and time in UTC}
#'  \item{Lat}{Forecast latitude}
#'  \item{Lon}{Forecast Longitude}
#'  \item{Wind}{Forecast wind in knots}
#'  \item{Gust}{Forecast gust in knots}
#' }
#' @examples
#' \dontrun{
#' get_fstadv("http://www.nhc.noaa.gov/archive/1998/1998ALEXadv.html") %>%
#'   tidy_fcst()
#' }
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

  # What forecast periods are in the current dataset?
  # #107 Modified regex pattern to look for Hr120, as well.
  fcst_periods <- as.list(names(df)) |>
    stringr::str_match(pattern = "Hr([:digit:]{2,3})FcstDate")
   fcst_periods <- fcst_periods[,2]
   fcst_periods <- as.numeric(fcst_periods[!rlang::are_na(.)] )

  forecasts <- purrr::map_df(
    .x = fcst_periods,
    .f = function(y) {
      df <-
        dplyr::select(df, c("Key", "Adv", "Date", paste0("Hr", y, v)))
       df <-  dplyr::rename(df, "Key" = "Key", "Adv" = "Adv", "Date" = "Date",
                      "FcstDate" = paste0("Hr", y, "FcstDate"),
                      "Lat" = paste0("Hr", y, "Lat"),
                      "Lon" = paste0("Hr", y, "Lon"),
                      "Wind" = paste0("Hr", y, "Wind"),
                      "Gust" = paste0("Hr", y, "Gust"))}) |>
    dplyr::arrange(.data$Key, .data$Date, .data$Adv, .data$FcstDate)

  # Remove NA rows
  forecasts <- forecasts[stats::complete.cases(
    forecasts$FcstDate, forecasts$Lat, forecasts$Lon, forecasts$Wind,
    forecasts$Gust),]
  return(forecasts)
}

#' @title tidy_fcst_wr
#' @description Tidy forecast wind radii of a fstadv dataframe object
#' @param df fstadv dataframe object
#' @details Tidies forecast wind radius for each forecast position. Complete
#' cases only (by quadrants). Use Key, Adv and Date to join with other tidy
#' dataframes.
#'
#' \describe{
#'  \item{Key}{Unique identifier of cyclone}
#'  \item{Adv}{Advisory number}
#'  \item{Date}{Date and time of advisory}
#'  \item{FcstDate}{Forecast date and time in UTC}
#'  \item{WindField}{Minimum sustained wind field for quadrants}
#'  \item{NE}{Radius in nautical miles for northeast quadrant}
#'  \item{SE}{Radius in nautical miles for southeast quadrant}
#'  \item{SW}{Radius in nautical miles for southwest quadrant}
#'  \item{NW}{Radius in nautical miles for northwest quadrant}
#' }
#' @examples
#' \dontrun{
#' get_fstadv("http://www.nhc.noaa.gov/archive/1998/1998ALEXadv.html") %>%
#'   tidy_fcst_wr()
#' }
#' @export
tidy_fcst_wr <- function(df) {

  if (!is.data.frame(df))
    stop("Expecting a dataframe.")

  # Build wind radius dataframe for each forecast position (12:72 hours; 96
  # and 120 hours are never forecasted). This dataframe will be similar to
  # fstadv.wr with the exception of FcstDate.

  v <- c("NE", "SE", "SW", "NW")

  # What forecast periods are in the current dataset?
  fcst_periods <- as.list(names(df)) %>%
    stringr::str_match(pattern = "Hr([:digit:]{2})FcstDate") %>%
  fcst_periods <-fcst_periods[,2]
  fcst_periods <- as.numeric(fcst_periods[!rlang::are_na(.)] )


  fcst_wr <- purrr::map_df(
    .x = fcst_periods,
    .f = function(x) {
      if (x %in% c(12, 24, 36)) fcst_wind_radii <- c(34, 50, 64)
      if (x %in% c(48, 72)) fcst_wind_radii <- c(34, 50)
      if (x %in% c(96, 120)) return(NULL)
      y <- purrr::map_df(.x = fcst_wind_radii, .f = function(z) {


         df <-  dplyr::select(df, c(
            "Key", "Adv", "Date", paste0("Hr", x, "FcstDate"),
            paste0("Hr", x, v, z)
          ))
         df <-  dplyr::rename(df,
            "Key" = "Key",
            "Adv" = "Adv",
            "Date" = "Date",
            "FcstDate" = paste0("Hr", x, "FcstDate"),
            "NE" = paste0("Hr", x, "NE", z),
            "SE" = paste0("Hr", x, "SE", z),
            "SW" = paste0("Hr", x, "SW", z),
            "NW" = paste0("Hr", x, "NW", z))
         df <- dplyr::mutate(df, "WindField" = z)
         df <- dplyr::select(df, c(
            .data$Key:.data$FcstDate,
            "WindField",
            .data$NE:.data$NW))
      })
      return(y)
    })

  fcst_wr <- dplyr::arrange(
    fcst_wr, .data$Key, .data$Date, .data$Adv, .data$FcstDate, .data$WindField
  )

  fcst_wr <- fcst_wr[stats::complete.cases(
    fcst_wr$NE, fcst_wr$SE, fcst_wr$SW, fcst_wr$NW),]

  fcst_wr

}
