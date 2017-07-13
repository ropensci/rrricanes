#' @title extract_storms
#' @description Extract storms for the given basin
#' @param basin AL or EP
#' @param contents contents of basin archive page
#' @return 4xN Dataframe
#' @keywords internal
extract_storms <- function(basin, contents) {

  if (basin == "AL") {
    link_xpath <- "//td[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]//a"
  } else if (basin == "EP") {
    link_xpath <- "//td[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]//a"
  } else {
    stop("No basin")
  }

  contents <- purrr::map(contents, ~.$parse("UTF-8")) %>%
    purrr::map(xml2::read_html)

  years <- purrr::map(contents, rvest::html_nodes, xpath = "//title") %>%
    purrr::map(rvest::html_text) %>%
    stringr::str_sub(0L, 4L) %>%
    as.numeric()

  storms <- purrr::map(contents, rvest::html_nodes, xpath = link_xpath)
  names <- purrr::map(storms, rvest::html_text) %>%
    purrr::map(stringr::str_to_title)
  links <- purrr::map(storms, rvest::html_attr, name = "href") %>%
    purrr::map2(years, ~paste0(year_archives_link(.y), .x))
  basins <- purrr::map(names, purrr::rep_along, basin)
  years <- purrr::map2(names, years, purrr::rep_along)

  df <- tibble::data_frame("Year" = years %>% purrr::flatten_dbl(),
                           "Name" = names %>% purrr::flatten_chr(),
                           "Basin" = basins %>% purrr::flatten_chr(),
                           "Link" = links %>% purrr::flatten_chr())

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
#'
#' To disable the progress bar set option dplyr.show_progress to FALSE.
#'
#' @param years numeric or vector, four digits (\%Y format)
#' @param basins One or both of c("AL", "EP")
#' @return Dataframe of storms.
#' @examples
#' # Default. Get all storms, both basins, for last year.
#' \dontrun{
#' storms <- get_storms(year = 2016, basin = c("AL", "EP"))
#'
#' # Get storms for two different years
#' storms.2010 <- get_storms(c(2010, 2015))
#'
#' # Get storms for two consecutive years, Atlantic basin only
#' storms.al.2005 <- get_storms(2005:2007, basin = "AL")
#' }
#' @source \url{http://www.nhc.noaa.gov/archive/2016/}
#' @export
get_storms <- function(years = format(Sys.Date(), "%Y"),
                       basins = c("AL", "EP")) {

  years <- validate_year(years)

  # No archives earlier than 1998 for now
  if (any(years < 1998))
    stop('Archives currently only available for 1998 to current year.')

  if (!all(basins %in% c("AL", "EP")))
    stop("Basin must 'AL' and/or 'EP'")

  links <- purrr::map(years, .f = year_archives_link) %>%
    purrr::flatten_chr()

  # 1998 is only year with slightly different URL. Modify accordingly
  links[grep("1998", links)] <- paste0(links[grep("1998", links)],
                                       "1998archive.shtml")

  # There is an 80-hit/10-second limit to the NHC pages (note Issue #94), or 8
  # requests/second. The below request will process 4 links every 0.5 seconds.
  links <- split(links, ceiling(seq_along(links)/4))
  p <- dplyr::progress_estimated(n = length(links))
  contents <- purrr::map(links, .f = function(x) {get_url_contents(x, p)}) %>%
    purrr::flatten()

  storm_df <- purrr::map_df(basins, extract_storms, contents) %>%
    dplyr::group_by(Year, Basin) %>%
    dplyr::arrange(Year, Basin) %>%
    dplyr::ungroup()

  return(storm_df)
}

#' @title year_archives_link
#' @description Returns link to a year's archive page
#' @param year 4-digit numeric
#' @keywords internal
year_archives_link <- function(year) {
    nhc_link <- get_nhc_link()
    link <- sprintf(paste0(nhc_link, 'archive/%i/'), year)
    return(link)
}
