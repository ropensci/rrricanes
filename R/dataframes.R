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

