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
