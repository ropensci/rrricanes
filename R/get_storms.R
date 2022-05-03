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
  # Read in contents as html
  html <- purrr::imap(contents, xml2::read_html)

  storms <- purrr::map(html, rvest::html_nodes, xpath = link_xpath)

  years <-
    html |>
    purrr::map(rvest::html_nodes, xpath = "//title") |>
    purrr::map(rvest::html_text) |>
    stringr::str_sub(0L, 4L) |>
    as.numeric()

  links <-
    storms |>
    purrr::map(rvest::html_attr, name = "href") |>
    purrr::map2(years, ~stringr::str_c(year_archives_link(.y), .x)) |>
    purrr::flatten_chr()

  names <-
    storms |>
    purrr::map(rvest::html_text) |>
    purrr::flatten_chr() |>
    stringr::str_to_title()

  # As of 2019-01-05, during the shutdown, a new table element exists at the
  # top of the archive pages. This has broken the function. Easiest way I can
  # see keeping as much of the current functionality as possible is by
  # filtering names. I know that only letters, spaces and dashes will exist
  # within the name values. So, if I can get indexes of those values that do
  # not match that pattern, I can trim the links value as well, then continue
  # as normal. This should also work after the shutdown ends.
  idx <- grep("^[[:alpha:][:blank:]-]+$", names)
  links <- links[idx]
  names <- names[idx]

  basins <- purrr::map(names, purrr::rep_along, basin) |>
            purrr::flatten_chr()

  years <- as.numeric(sub(".+(\\d{4}).+", "\\1", links))

  tibble::tibble(
    "Year" = years,
    "Name" = names,
    "Basin" = basins,
    "Link" = links
  )
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

  if (!all(years %in% 1998:lubridate::year(Sys.Date()))){
    stop(sprintf("Param `years` must be between 1998 and %s.",
                 lubridate::year(Sys.Date())),
         call. = FALSE)
  }

  if (!all(basins %in% c("AL", "EP")))
    stop("Basin must 'AL' and/or 'EP'.", call. = FALSE)

  links <-
    years |>
    purrr::map(.f = year_archives_link) |>
    purrr::flatten_chr()

  # 1998 is only year with slightly different URL. Modify accordingly
  links[grep("1998", links)] <- stringr::str_c(links[grep("1998", links)],
                                               "1998archive.shtml")

  contents <- rrricanes:::get_url_contents(links)

  purrr::map_df(basins, extract_storms, contents)

}

#' @title year_archives_link
#' @description Returns link to a year's archive page
#' @param year 4-digit numeric
#' @keywords internal
year_archives_link <- function(year) {
  nhc_link <- get_nhc_link()
  sprintf(stringr::str_c(nhc_link, 'archive/%i/'), year)
}
