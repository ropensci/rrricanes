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

