#' @title extract_year_archive_link
#' @description Extracts the year from the archive link.
#' @param link URL of archive page
#' @return year 4-digit numeric
#' @keywords internal
extract_year_archive_link <- function(link) {
  # Year is listed in link towards the end surrounded by slashes.
  as.numeric(stringr::str_match(link, '/([:digit:]{4})/')[,2])
}

#' @title get_nhc_link
#' @description Return root link of NHC archive pages.
#' @param withTrailingSlash True, by default. False returns URL without
#' trailing slash.
#' @param protocol https or http
#' @keywords internal
get_nhc_link <- function(withTrailingSlash = TRUE, protocol = "https") {
  if (withTrailingSlash)
    return(sprintf("%s://www.nhc.noaa.gov/", protocol))
  sprintf("%s://www.nhc.noaa.gov", protocol)
}

#' @title get_nhc_ftp_link
#' @description Return root of NHC FTP server
#' @inheritParams get_nhc_link
#' @keywords internal
get_nhc_ftp_link <- function(withTrailingSlash = TRUE) {
  if (withTrailingSlash)
    return("ftp://ftp.nhc.noaa.gov/")
  "ftp://ftp.nhc.noaa.gov"
}

#' @title knots_to_mph
#' @description convert knots (kt) to miles per hour (mph)
#' @param x wind speed in knots
#' @return x in miles per hour
#' @examples
#' knots_to_mph(65)
#' @export
knots_to_mph <- function(x) {
  x * 1.15077945
}

#' @title mb_to_in
#' @description convert millibars (mb) to inches of mercury (in)
#' @param x barometric pressure in mb
#' @return x in inches
#' @examples
#' mb_to_in(999)
#' @export
mb_to_in <- function(x) {
  x * 0.029529983071
}

#' @title month_str_to_num
#' @description Convert three-character month abbreviation to integer
#' @param m Month abbreviated (SEP, OCT, etc.)
#' @return numeric 1-12
#' @keywords internal
month_str_to_num <- function(m) {
  abbr <- which(month.abb == stringr::str_to_title(m))
  if (purrr::is_empty(abbr))
    stop(sprintf("%s is not a valid month abbreviation.", m))
  abbr
}

#' @title nm_to_sm
#' @description Convert nautical miles to survey miles
#' @param x Nautical miles
#' @examples
#' nm_to_sm(c(50, 100, 150))
#' @export
nm_to_sm <- function(x) {
  x * 1.15078
}

#' @title saffir
#' @description Return category of tropical cyclone based on wind. Saffir-
#' Simpson Hurricane Scale does not apply to non-tropical cyclones.
#' @param x Vector of wind speed values.
#' @examples
#' saffir(c(32, 45, 70, 90, 110, 125, 140))
#' @export
saffir <- function(x) {
  cut(x, breaks = c(0, 34, 65, 84, 96, 114, 135, 500),
      labels = c("TD","TS", "HU1","HU2", "HU3","HU4","HU5"),
      ordered_result = TRUE)
}

#' @title status_abbr_to_str
#' @description Convert Status abbreviation to string
#' @param x character vector of status abbreviations
#' @details Status abbreviations
#' \describe{
#'   \item{DB}{Disturbance (of any intensity)}
#'   \item{EX}{Extratropical cyclone (of any intensity)}
#'   \item{HU}{Tropical cyclone of hurricane intensity (> 64 knots)}
#'   \item{LO}{A low that is neither a tropical cyclone, a subtropical
#'         cyclone, nor an extratropical cyclone (of any intensity)}
#'   \item{SD}{Subtropical cyclone of subtropical depression intensity
#'         (< 34 knots)}
#'   \item{SS}{Subtropical cyclone of subtropical storm intensity
#'         (> 34 knots)}
#'   \item{TD}{Tropical cyclone of tropical depression intensity (< 34 knots)}
#'   \item{TS}{Tropical cyclone of tropical storm intensity (34-63 knots)}
#'   \item{WV}{Tropical Wave (of any intensity)}
#' }
#' @return character vector of strings
#' @seealso \url{http://www.aoml.noaa.gov/hrd/hurdat/newhurdat-format.pdf}
#' @examples
#' # Extratropical Cyclone
#' status_abbr_to_str("EX")
#'
#' # Hurricane
#' status_abbr_to_str("HU")
#' @export
status_abbr_to_str <- function(x) {
  abbr_to_str <- c("TD" = "Tropical Depression",
                   "TS" = "Tropical Storm",
                   "HU" = "Hurricane",
                   "EX" = "Extratropical Cyclone",
                   "SD" = "Subtropical Depression",
                   "SS" = "Subtropical Storm",
                   "LO" = "Low",
                   "WV" = "Tropical Wave",
                   "DB" = "Disturbance")
  unname(abbr_to_str[x])
}
