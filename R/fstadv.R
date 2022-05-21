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
#'  \item{Date}{Date and time of advisory}
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
  get_product(links = links, product = "fstadv")
}

#' @title fstadv
#' @description Extrapolate data from FORECAST/ADVISORY products.
#' @details Given a direct link to a forecast/advisory product, parse and
#'    return data frame of values.
#' @param contents URL of a specific FORECAST/ADVISORY product
#' @return Data frame of values
#' @export
fstadv <- function(contents) {

  status <- scrape_header(
    contents = contents,
    # The "SPECIAL" pattern has to be left here; moving it under
    # `scrape_header` will break posest and update products.
    ptn_product_title = "(?:\n?SPECIAL\\s+)?(?:FORECAST/|MARINE\\s+)?(?:ADVISORY)?"
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
      Lat = lat_lon[1],
      Lon = lat_lon[2],
      Wind = winds_gusts[,1],
      Gust = winds_gusts[,2],
      Pressure = pressure,
      PosAcc = posacc,
      FwdDir = fwd_mvmt[,1],
      FwdSpeed = fwd_mvmt[,2],
      Eye = eye #,
      #Seas = seas,
      #WindRadius = wind_radius,
      #Forecast = forecasts
  ) |> bind_rows(list(seas,
       wind_radius,
      forecasts))

   fstadv.data
}

