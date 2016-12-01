#' @title get_fstadv
#' @description Return dataframe of forecast/advisory data.
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane, 
#'     etc.}
#'   \item{Name}{Name of storm}
#'   \item{Adv}{Advisory Number}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Contents}{Text contents of product}
#' }
#' @param link URL to storm's archive page.
#' @seealso \code{\link{get_storms}}, \code{\link{public}}
#' @export
get_fstadv <- function(link) {

  # Check status of link(s)
  valid.link <- sapply(link, .status)
  valid.link <- na.omit(valid.link)
  if(length(valid.link) == 0)
    stop("No valid links.")
  
  products <- unlist(sapply(valid.link, get_products))

  products.fstadv <- lapply(filter_forecast_advisories(products), fstadv)
  
  fstadv <- data.table::rbindlist(products.fstadv)
  
  return(fstadv)
  
}

#' @title fstadv
#' @description Extrapolate data from FORECAST/ADVISORY products. 
#' @details Given a direct link to a forecast/advisory product, parse and 
#' return dataframe of values.
#' @param link URL of a specific FORECAST/ADVISORY product
#' @param display_link Display each link as being worked; default is TRUE.
#' @return Dataframe
#' @seealso \code{\link{get_fstadv}}
#' @export
fstadv <- function(link, display_link = TRUE) {
  
  valid.link <- sapply(link, .status)
  valid.link <- na.omit(valid.link)
  if(length(valid.link) == 0)
    stop("No valid links.")
  
  if(display_link)
    message(sprintf("Working %s", valid.link))
  
  contents <- valid.link %>% 
    xml2::read_html() %>% 
    rvest::html_nodes("pre") %>% 
    rvest::html_text()
  
  # Make sure this is a public advisory product
  if(!any(stringr::str_count(contents, c("MIATCMAT", "MIATCMEP"))))
    stop(sprint("Invalid Forecast/Advisory link. %s", l))
  
  df <- .create_df_fstadv()

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
  
  return(df)
}

#' @title fstadv_eye
#' @description Get eye diameter, if available
#' @param contents text contents of FORECAST/ADVISORY
#' @return numeric
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
fstadv_fwd_mvmt <- function(contents, what = NULL) {
  
  if(!is.character(what)) {stop('\'what\' must contain \'fwd_dir\' or \'fwd_speed\'')}
  
  ptn <- paste0('PRESENT MOVEMENT TOWARD[A-Z- ]+', 
                '([0-9]{1,3})', # Forward direction
                '[ ]+DEGREES AT[ ]+', 
                '([0-9]{1,3})', # Forward speed
                ' KT')
  
  if(what == 'fwd_dir') {
    return(as.numeric(stringr::str_match(contents, ptn)[,2]))
  } else if (what == 'fwd_speed') {
    return(as.numeric(stringr::str_match(contents, ptn)[,3]))
  } else {
    return(NA)
  }
  
}

#' @title fstadv_fwd_speed
#' @description Extract forward speed from forecast/advisory product
#' @param Contents of forecast/advisory product.
#' @return integer or NA
fstadv_fwd_speed <- function(contents) {
  fwd_speed <- fstadv_fwd_mvmt(contents, what = 'fwd_speed')
  return(fwd_speed)
}

#' @title fstadv_gusts
#' @description Extract wind gusts from a forecast/advisory product.
#' @param Contents of forecast/advisory product.
#' @return integer or NA
fstadv_gusts <- function(contents) {
  gust <- fstadv_winds_gusts(contents, what = 'gust')
  return(gust)
}

#' @title fstadv_pos_accuracy()
#' @description Get position accuracy
#' @param contents text contents of FORECAST/ADVISORY
#' @return numeric
fstadv_pos_accuracy <- function(contents) {
  ptn <- paste0('POSITION ACCURATE WITHIN[ ]+', 
                '([0-9]{2,3})', 
                '[ ]+NM')
  pos_acc <- stringr::str_match(contents, ptn)[,2]
  return(as.numeric(pos_acc))
  
}

#' @title fstadv_pressure
#' @description Return current minimum central pressure of storm in millibars (mb)
#' @param contents text contents of FORECAST/ADVISORY product
#' @return numeric
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
fstadv_lat <- function(contents) {
  lat <- fstadv_lat_lon(contents, what = 'lat')
  return(lat)
}

#' @title fstadv_lat_lon
#' @description Returns numeric for latitude or longitude; negative if in southern or eastern hemisphere
#' @details Helper function to take character latitude or longitude and, 
#' depending on the value of hemisphere return a positive or negative numeric, 
#' or NA if not found.
#' @param contents text contents of FORECAST/ADVISORY
#' @param what What are we returning? c("lat", "lon")
#' @return numeric
fstadv_lat_lon <- function(contents, what = NULL) {
  
  if(!is.character(what)) {stop('\'what\' must contain \'lat\' or \'lon\'')}
  
  ptn <- paste0('[CENTER LOCATED | DISSIPATING] NEAR[ ]+', 
                '([0-9\\.]{3,4})', # Latitude can be 9.9N or 99.9N
                '([N | S]{1})', # Norhtern meisphere
                '[ ]+([0-9\\.]{4,5})', #Longitude can be 0 to 180
                '([E | W]){1}', # Hemisphere
                '[ ]+')
  
  x <- stringr::str_match(contents, ptn)
  
  if(!is.na(x[,2]) & !is.na(x[,3])) {
    if(what == 'lat') {
      lat <- convert_lat_lon(as.numeric(x[,2]), x[,3])
    } else if (what == 'lon') {
      lon <- convert_lat_lon(as.numeric(x[,4]), x[,5])
    }
  } else {
    return(NA)
  }
  
}

#' @title fstadv_lon
#' @description Extract longitude from forecast/advisory product
#' @param contents Content of forecast/advisory product
#' @return numeric, positive if in eastern hemisphere, negative for western.
fstadv_lon <- function(contents) {
  lon <- fstadv_lat_lon(contents, what = 'lon')
  return(lon)
}

#' @title fstadv_winds
#' @description Extract current maximum sustained winds from contents
#' @param contents text contents of FORECAST/ADVISORY product
#' @return numeric
fstadv_winds <- function(contents) {
  wind <- fstadv_winds_gusts(contents, what = 'wind')
  return(wind)
}

#' @title fstadv_winds_gusts
#' @description Get winds or gusts in knots (KT)
#' @param contents text contents of FORECAST/ADVISORY product
#' @param what return wind or gust?
#' @return numeric
fstadv_winds_gusts <- function(contents, what = NULL) {
  
  if(!is.character(what)) {stop('\'what\' must contain \'wind\' or \'gust\'')}
  
  ptn <- paste0('MAX SUSTAINED WINDS[ ]+', 
                '([0-9]{2,3})', # Winds
                '[ ]+KT WITH GUSTS TO[ ]+', 
                '([0-9]{2,3})', # Gusts
                '[ ]+KT')
  
  if(what == 'wind') {
    return(as.numeric(stringr::str_match(contents, ptn)[,2]))
  } else if (what == 'gust') { 
    return(as.numeric(stringr::str_match(contents, ptn)[,3]))
  } else {
    return(NA)
  }
  
}
