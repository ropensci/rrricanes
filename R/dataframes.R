#' @title .create_df_archives
#' @description Template for archives dataframe
#' \describe{
#'   \item{Year}{integer, year of storm}
#'   \item{Name}{character, name of storm}
#'   \item{Basin}{character, basin storm developed in}
#'   \item{Link}{character, link to storm's archive page}
#' }
#' @return dataframe
.create_df_archives <- function() {
  df <- data.frame("Year" = integer(), 
                   "Name" = character(), 
                   "Basin" = character(), 
                   "Link" = character())
  return(df)
}

#' @title .create_df_discus
#' @description Template for storm discussions dataframe
#' @return empty dataframe
#' @seealso \code{\link{get_public_advisories}}
.create_df_discus <- function() {
  df <- data.frame("Status" = character(), 
                   "Name" = character(),
                   # Allow for intermediate advisories, i.e., "1A", "2", "2A"...
                   "Adv" = character(), 
                   "Date" = character(), 
                   "Contents" = character())
  
  return(df)
}

#' @title .create_df_fstadv
#' @description Template for public advisory dataframe
#' @return empty dataframe
#' @seealso \code{\link{get_public_advisories}}
.create_df_fstadv <- function() {
  df <- data.frame("Status" = character(), 
                   "Name" = character(),
                   # Allow for intermediate advisories, i.e., "1A", "2", "2A"...
                   "Adv" = character(), 
                   "Date" = character(), 
                   "Key" = character(), 
                   "Contents" = character())
  
  return(df)
}

#' @title .create_df_public
#' @description Template for public advisory dataframe
#' @return empty dataframe
#' @seealso \code{\link{get_public_advisories}}
.create_df_public <- function() {
  df <- data.frame("Status" = character(), 
                   "Name" = character(),
                   # Allow for intermediate advisories, i.e., "1A", "2", "2A"...
                   "Adv" = character(), 
                   "Date" = character(), 
                   "Contents" = character())
  
  return(df)
}

#' @title .create_df_posest
#' @description Template for position estimates dataframe
#' @return empty dataframe
#' @seealso \code{\link{get_public_advisories}}
.create_df_posest <- function() {
  df <- data.frame("Status" = character(), 
                   "Name" = character(),
                   # Allow for intermediate advisories, i.e., "1A", "2", "2A"...
                   "Adv" = character(), 
                   "Date" = character(), 
                   "Contents" = character())
  
  return(df)
}

#' @title .create_df_prblty
#' @description Template for strike probabilities dataframe
#' @return empty dataframe
#' @seealso \code{\link{get_public_advisories}}
.create_df_prblty <- function() {
  df <- data.frame("Status" = character(), 
                   "Name" = character(),
                   # Allow for intermediate advisories, i.e., "1A", "2", "2A"...
                   "Adv" = character(), 
                   "Date" = character(), 
                   "Contents" = character())
  
  return(df)
}

#' @title .drop_dataframes
#' @description Drop existing dataframes.
.drop_dataframes <- function() {
  
  if(exists('df_advisories'))
    rm(df_advisories, envir = .GlobalEnv)
  
  if(exists('df_forecasts'))
    rm(df_forecasts, envir = .GlobalEnv)
  
  if(exists('df_forecast_winds'))
    rm(df_forecast_winds, envir = .GlobalEnv)
  
  if(exists('df_winds'))
    rm(df_winds, envir = .GlobalEnv)
  
  if(exists('df_seas'))
    rm(df_seas, envir = .GlobalEnv)
}

