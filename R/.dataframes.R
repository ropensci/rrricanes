#' @title create_df_advisories
#' @description Create a dataframe to hold advisory data. 
#' @details \strong{Do not run prior to collecting storm data}. Already built-in. 
#'   Advisory data contains 'basic' information:
#' \itemize{
#'   \item Key - Unique identifier for the storm. Ex: AL011995 means that for the 
#'     Atlantic basin (AL) this is the first storm (01) for the year 1995. 
#'   \item Name - Given name as previously assigned by the World Meteorological 
#'     Organization. Names for a year are assigned years in advance. Tropical 
#'     depressions are given 
#'     \href{https://en.wikipedia.org/wiki/Cardinal_number_(linguistics)}{cardinal numbers}. 
#'     Once a storm becomes a tropical storm (winds > 34kt or 39mph) it receives 
#'     a name. 
#'   \item Adv - Advisory number for an observation set.
#'   \item ObDate - Observation date for the specific Adv
#'   \item Status - Status at the time of the observation. Typically, but not 
#'     limited to: 
#'     \itemize{
#'       \item Tropical Depression
#'       \item Tropical Storm
#'       \item Hurricane
#'       \item Extratropical (Depression|Storm)
#'       \item Subtropical (Depression|Storm)
#'     }
#'   \item Lat - location of the storm north or south of the celestial equator. 
#'     If Lat is a negative numeric, it is in the southern hemisphere. Otherwise, 
#'     northern. 
#'   \item Lon - location of the storm east or west of the prime meridian. If 
#'     Lon is a negative numeric, it is in the western hemisphere. Otherwise, 
#'     eastern.
#'   \item Wind - Wind speed in knots. To conver to mph, use 
#'     \code{\link{knots_to_mph}}.
#'   \item Gust - Wind gusts in knots.
#'   \item Pressure - Central pressure of the storm in millibars. To convert 
#'     from millibars to inches, use \code{\link{mb_to_in}}.
#'   \item PosAcc - Position accuracy estimate of the center of the storm in 
#'     nautical miles.
#'   \item FwdDir - angular direction of the storms' forward movement from 1-360&deg;
#'   \item FwdSpeed - forward speed of the storm in knots.
#'   \item Eye - Size of the eye in nautical miles. Most always only available 
#'     for hurricanes and even then, can be NA.
#' }
#' @return Returns an empty dataframe.
create_df_advisories <- function() {
  
  df <- tibble::data_frame('Key' = character(), 
                           'Name' = character(), 
                           'Adv' = numeric(), 
                           'ObDate' = as.Date(character()), 
                           'Status' = character(), 
                           'Lat' = numeric(), 
                           'Lon' = numeric(), 
                           'Wind' = numeric(), 
                           'Gust' = numeric(), 
                           'Pressure' = numeric(), 
                           'PosAcc' = numeric(), 
                           'FwdDir' = numeric(), 
                           'FwdSpeed' = numeric(), 
                           'Eye' = numeric(), 
                           stringsAsFactors = FALSE)
  
  return(df)  
  
}

#' @title create_df_errors
#' @description Create dataframe to hold errors generated during parsing
create_df_errors <- function() {
  
  df <- tibble::data_frame('Key' = character(), 
                           'Adv' = numeric(), 
                           'ObDate' = as.Date(character()), 
                           'CurrTime' = as.Date(character()), 
                           'url' = character(), 
                           'error' = character(), 
                           stringsAsFactors = FALSE)
  
  return(df)
  
}

#' @title create_df_df_forecast_winds
#' @description Create dataframe to hold forecast wind coverage
#' @return dataframe
create_df_forecast_winds <- function() {
  
  #' This dataframe is same as df_winds only adding an additional column, 
  #' 'Forecast Date
  df <- create_df_winds()
  df <- df %>% 
    tibble::add_column('FcstDate' = as.Date(character()), .before = 4)
  return(df)
}

#' @title create_df_forecasts()
#' @description Create a dataframe to hold forecast data. This dataframe will be semi-
#' replica of df_advisories
#' @return dataframe
create_df_forecasts <- function() {
  
  df <- create_df_advisories()
  df <- df %>% 
    tibble::add_column('FcstDate' = as.Date(character()), .before = 4)
  df <- dplyr::select(df, Key, Adv, ObDate, FcstDate, Lat, Lon, Wind, Gust)
  
  return(df)  
  
}

#' @title create_df_seas()
#' @description Create a dataframe to hold sea data
#' @return dataframe
create_df_seas <- function() {
  
  #' The format of the dataframe is similar to df_winds
  df <- create_df_winds()  
  #' There's no need for a 'SeaField' since it's one line for 12ft seas only. 
  #' Remove 'WindField'
  df <- df %>% dplyr::select(-WindField)
  return(df)  
}

#' @title create_df_warnings
#' @description Create dataframe to hold warnings generated during parsing
create_df_warnings <- function() {
  
  df <- create_df_errors()
  df <- dplyr::rename(df, warning = error)
  return(df)
  
}

#' @title create_df_winds()
#' @description Create a dataframe to hold wind data
#' @return dataframe
create_df_winds <- function() {
  
  df <- tibble::data_frame('Key' = character(), 
                           'Adv' = numeric(), 
                           'ObDate' = as.Date(character()), 
                           'WindField' = numeric(), 
                           'NE' = numeric(), 
                           'SE' = numeric(), 
                           'SW' = numeric(), 
                           'NW' = numeric(), 
                           stringsAsFactors = FALSE)
  
  return(df)  
}

