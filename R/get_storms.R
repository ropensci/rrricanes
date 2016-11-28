#' @title .build_archive_df
#' @description Add storms for the given link, basin into dataframej.
#' @param page contents of page being parsed
#' @param basin c("AL", "EP")
#' @return dataframe 
.build_archive_df <- function(link, basin = c("AL", "EP")) {
  
  # Get year
  year <- .extract_year_archive_link(link)
  
  # Build archive URL  
  year_link <- .year_archives_link(year)
  
  # Slightly different URL 1998 data for some reason. Hold onto year_link and 
  # make archive_link to get page contents
  if(year == 1998) {
    archive_link <- paste0(year_link, year, 'archive.shtml')
  } else {
    archive_link <- year_link
  }
  
  # Test link
  if(!.status(archive_link)) {
    stop(sprintf("Invalid URL. %d", archive_link))
  }

  l <- lapply(basin, .extract_storms, link = archive_link)
  
  df <- data.table::rbindlist(l)
  
  return(df)
  
}

#' @title .extract_storms
#' @description Extract storms for the given basin
#' @param basin AL or EP
#' @param link URL to basin archive page
#' @return 4xN Dataframe
.extract_storms <- function(basin, link) {

  # Get year
year <- .extract_year_archive_link(link)

  if(basin == "AL") {
    xp = "//td[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]//a"
  } else if (basin == "EP") {
    xp = "//td[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]//a"
  } else {
    stop("No basin given.")
  }
  
  df <- .create_df_archives()
  
  s <- link %>% 
    xml2::read_html() %>% 
    rvest::html_nodes("table")
  
  col <- s %>% 
    rvest::html_nodes(
      xpath = xp)
  
  col.links <- paste0(
    .year_archives_link(year), 
    col %>% 
      rvest::html_attr("href"))
  
  col.names <- col %>% 
    rvest::html_text()
  
  df <- tibble::add_row(df, 
                        Year = year, 
                        Name = col.names, 
                        Basin = basin, 
                        Link = col.links)
  
  return(df)

}

#' @title get_storms
#' @description Returns storms and product link. 
#' @details By default returns all storms for the current year. If no storms 
#' have developed will return an empty dataframe. 
#' @format A 4xN dataframe
#' \describe{
#'   \item{Year}{Numeric, four-digit year of the storm}
#'   \item{Name}{Character, name of storm mixed-case}
#'   \item{Basin}{AL (Atlantic) or EP (East Pacific)}
#'   \item{Link}{URL to storms' product pages}
#' }
#' @param year numeric or vector, four digits (\%Y format)
#' @param basin One or both of c("AL", "EP")
#' @return Dataframe of storms.
#' @importFrom data.table rbindlist
#' @examples 
#' # Default. Get all storms, both basins, for current year.
#' storms <- get_storms(year = lubridate::year(Sys.Date()), basin = c("AL", "EP"))
#' 
#' # Get storms for two different years
#' storms.2010 <- get_storms(c(2010, 2015))
#' 
#' # Get storms for two consecutive years, Atlantic basin only
#' storms.al.2005 <- get_storms(2005:2007, basin = "AL")
#' @source \url{http://www.nhc.noaa.gov/archive/2016/}
#' @export
get_storms <- function(year = lubridate::year(Sys.Date()), 
                       basin = c("AL", "EP")) {
  
  year <- .validate_year(year)  
  
  # No archives earlier than 1998 for now
  if(any(year < 1998))
    stop('Archives currently only available for 1998 to current year.')
  
  if(!all(basin %in% c("AL", "EP")))
    stop("Basin must 'AL' and/or 'EP'")
  
  # Get archive pages for each year
  link <- lapply(year, .year_archives_link)
  
  # Get archive pages for each storm in year
  l <- lapply(link, .build_archive_df, basin)
  
  df <- data.table::rbindlist(l)
  
  # At this point we have a list of links pointing to each storm, each basin.
  return(df)
  
}

#' @title .year_archives_link
#' @description Returns link to a year's archive page
#' @param year 4-digit numeric
#' @export
.year_archives_link <- function(year) {
  nhc_link <- get_nhc_link()
  link <- sprintf(paste0(nhc_link, 'archive/%i/'), year)
  return(link)
}
