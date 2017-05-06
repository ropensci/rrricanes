#' @title create_df_discus
#' @description Template for storm discussions dataframe
#' @return empty dataframe
#' @seealso \code{\link{get_discus}}
#' @keywords internal
create_df_discus <- function() {
  df <- tibble::data_frame("Status" = character(),
                           "Name" = character(),
                           # Allow for intermediate advisories, i.e., "1A", "2", "2A"...
                           "Adv" = character(),
                           "Date" = as.POSIXct(character(), tz = "UTC"),
                           "Contents" = character())

  return(df)
}

#' @title create_df_fstadv
#' @description Template for public advisory dataframe
#' @return empty dataframe
#' @seealso \code{\link{get_fstadv}}
#' @keywords internal
create_df_fstadv <- function() {
  df <- tibble::data_frame("Status" = character(),
                           "Name" = character(),
                           # Allow for intermediate advisories, i.e., "1A", "2", "2A"...
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

#' @title create_df_posest
#' @description Template for position estimates dataframe
#' @return empty dataframe
#' @seealso \code{\link{get_posest}}
#' @keywords internal
create_df_posest <- function() {
  df <- tibble::data_frame("Status" = character(),
                           "Name" = character(),
                           # Allow for intermediate advisories, i.e., "1A", "2", "2A"...
                           "Adv" = character(),
                           "Date" = as.POSIXct(character(), tz = "UTC"),
                           "Contents" = character())

  return(df)
}

#' @title create_df_prblty
#' @description Template for strike probabilities dataframe
#' @return empty dataframe
#' @seealso \code{\link{get_prblty}}
#' @keywords internal
create_df_prblty <- function() {
  df <- tibble::data_frame("Status" = character(),
                           "Name" = character(),
                           # Allow for intermediate advisories, i.e., "1A", "2", "2A"...
                           "Adv" = character(),
                           "Date" = as.POSIXct(character(), tz = "UTC"),
                           "Contents" = character())

  return(df)
}

#' @title create_df_public
#' @description Template for public advisory dataframe
#' @return empty dataframe
#' @seealso \code{\link{get_public}}
#' @keywords internal
create_df_public <- function() {
  df <- tibble::data_frame("Status" = character(),
                           "Name" = character(),
                           # Allow for intermediate advisories, i.e., "1A", "2", "2A"...
                           "Adv" = character(),
                           "Date" = as.POSIXct(character(), tz = "UTC"),
                           "Contents" = character())

  return(df)
}

#' @title create_df_updates
#' @description Template for cyclone updates dataframe
#' @return empty dataframe
#' @seealso \code{\link{get_updates}}
#' @keywords internal
create_df_updates <- function() {
  df <- tibble::data_frame("Status" = character(),
                           "Name" = character(),
                           # Allow for intermediate advisories, i.e., "1A", "2", "2A"...
                           "Adv" = character(),
                           "Date" = as.POSIXct(character(), tz = "UTC"),
                           "Contents" = character())

  return(df)
}

#' @title create_df_wndprb
#' @description Template for wind probabilities dataframe
#' @return empty dataframe
#' @seealso \code{\link{get_wndprb}}
#' @keywords internal
create_df_wndprb <- function() {
  df <- tibble::data_frame("Status" = character(),
                           "Name" = character(),
                           # Allow for intermediate advisories, i.e., "1A", "2", "2A"...
                           "Adv" = character(),
                           "Date" = as.POSIXct(character(), tz = "UTC"),
                           "Contents" = character())

  return(df)
}

#' @title drop_dataframes
#' @description Drop existing dataframes.
#' @keywords internal
drop_dataframes <- function() {

  if (exists('df_advisories'))
    rm(df_advisories, envir = .GlobalEnv)

  if (exists('df_forecasts'))
    rm(df_forecasts, envir = .GlobalEnv)

  if (exists('df_forecast_winds'))
    rm(df_forecast_winds, envir = .GlobalEnv)

  if (exists('df_winds'))
    rm(df_winds, envir = .GlobalEnv)

  if (exists('df_seas'))
    rm(df_seas, envir = .GlobalEnv)
}

