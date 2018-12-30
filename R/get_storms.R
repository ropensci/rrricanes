#' @title extract_storms
#' @description Extract storms for the given basin
#' @param basin AL or EP
#' @param contents contents of basin archive page
#' @return 4xN Dataframe
#' @keywords internal
extract_storms <- function(basin, contents) {

  xpaths <- list(
    "AL" = "//td[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]//a",
    "EP" = "//td[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]//a"
  )

  link_xpath <- xpaths[[basin]]

  contents <-
    contents %>%
    purrr::map(~.x$parse("UTF-8")) %>%
    purrr::map(xml2::read_html)

  years <-
    contents %>%
    purrr::map(rvest::html_nodes, xpath = "//title") %>%
    purrr::map(rvest::html_text) %>%
    stringr::str_sub(0L, 4L) %>%
    as.numeric()

  storms <- purrr::map(contents, rvest::html_nodes, xpath = link_xpath)

  names <-
    storms %>%
    purrr::map(rvest::html_text) %>%
    purrr::map(stringr::str_to_title)

  links <-
    storms %>%
    purrr::map(rvest::html_attr, name = "href") %>%
    purrr::map2(years, ~paste0(year_archives_link(.y), .x))

  basins <- purrr::map(names, purrr::rep_along, basin)

  years <- purrr::map2(names, years, purrr::rep_along)

  tibble::data_frame("Year" = purrr::flatten_dbl(years),
                     "Name" = purrr::flatten_chr(names),
                     "Basin" = purrr::flatten_chr(basins),
                     "Link" = purrr::flatten_chr(links))
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

  years <- as.integer(years)

  if (!all(years %in% 1998:lubridate::year(Sys.Date())))
    stop(sprintf("Param `years` must be between 1998 and %s.",
                 lubridate::year(Sys.Date())),
         call. = FALSE)

  if (!all(basins %in% c("AL", "EP")))
    stop("Basin must 'AL' and/or 'EP'.", call. = FALSE)

  links <-
    years %>%
    purrr::map(.f = year_archives_link) %>%
    purrr::flatten_chr()

  # 1998 is only year with slightly different URL. Modify accordingly
  links[grep("1998", links)] <- paste0(links[grep("1998", links)],
                                       "1998archive.shtml")

  contents <-
    links %>%
    purrr::map(get_url_contents) %>%
    purrr::flatten()

  purrr::map_df(basins, extract_storms, contents)

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
