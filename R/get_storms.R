#' @title build_archive_df
#' @description Add storms for the given link, basin into dataframej.
#' @param page contents of page being parsed
#' @param basin c("AL", "EP")
#' @param p dplyr::progress_estimate.
#' @return dataframe
#' @keywords internal
build_archive_df <- function(link, basin = c("AL", "EP"), p) {

    p$pause(0.5)$tick()$print()

    # Get year
    year <- extract_year_archive_link(link)

    # Build archive URL
    year_link <- year_archives_link(year)

    # Slightly different URL 1998 data for some reason. Hold onto year_link and
    # make archive_link to get page contents
    if (year == 1998) {
        archive_link <- paste0(year_link, year, 'archive.shtml')
    } else {
        archive_link <- year_link
    }

    # Test link
    valid.link <- status(archive_link)
    valid.link <- stats::na.omit(valid.link)
    if (length(valid.link) == 0)
        stop(sprintf("Invalid URL. %d", archive_link))

    l <- purrr::map(basin, extract_storms, link = archive_link)

    df <- purrr::map_df(l, dplyr::bind_rows)

    return(df)

}

#' @title create_df_archives
#' @description Template for archives dataframe
#' \describe{
#'   \item{Year}{integer, year of storm}
#'   \item{Name}{character, name of storm}
#'   \item{Basin}{character, basin storm developed in}
#'   \item{Link}{character, link to storm's archive page}
#' }
#' @return dataframe
#' @keywords internal
create_df_archives <- function() {
    df <- tibble::data_frame("Year" = integer(),
                             "Name" = character(),
                             "Basin" = character(),
                             "Link" = character())
    return(df)
}

#' @title extract_storms
#' @description Extract storms for the given basin
#' @param basin AL or EP
#' @param link URL to basin archive page
#' @return 4xN Dataframe
#' @keywords internal
extract_storms <- function(basin, link) {

    # Get year
    year <- extract_year_archive_link(link)

    if (basin == "AL") {
        xp = "//td[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]//a"
    } else if (basin == "EP") {
        xp = "//td[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]//a"
    } else {
        stop("No basin given.")
    }

    df <- create_df_archives()

    s <- link %>%
        xml2::read_html() %>%
        rvest::html_nodes("table")

    col <- s %>%
        rvest::html_nodes(
            xpath = xp)

    col.links <- paste0(
        year_archives_link(year),
        col %>%
            rvest::html_attr("href"))

    col.names <- col %>%
        rvest::html_text()

    if (length(col.names) == 0) {
        message(sprintf("There are no %s storms for %d", basin, year))
        return(NULL)
    }

    # Make names proper case
    col.names <- map_chr(col.names, toproper)

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
#'
#' To disable the progress bar set option dplyr.show_progress to FALSE.
#'
#' @param year numeric or vector, four digits (\%Y format)
#' @param basin One or both of c("AL", "EP")
#' @return Dataframe of storms.
#' @importFrom data.table rbindlist
#' @examples
#' # Default. Get all storms, both basins, for last year.
#' storms <- get_storms(year = 2016, basin = c("AL", "EP"))
#'
#' # Get storms for two different years
#' storms.2010 <- get_storms(c(2010, 2015))
#'
#' # Get storms for two consecutive years, Atlantic basin only
#' storms.al.2005 <- get_storms(2005:2007, basin = "AL")
#' @source \url{http://www.nhc.noaa.gov/archive/2016/}
#' @export
get_storms <- function(year = format(Sys.Date(), "%Y"),
                       basin = c("AL", "EP")) {

    year <- validate_year(year)

    # No archives earlier than 1998 for now
    if (any(year < 1998))
        stop('Archives currently only available for 1998 to current year.')

    if (!all(basin %in% c("AL", "EP")))
        stop("Basin must 'AL' and/or 'EP'")

    p <- dplyr::progress_estimated(n = length(year))

    # Get archive pages for each year
    link <- purrr::map(year, year_archives_link)

    # Get archive pages for each storm in year
    l <- purrr::map(link, build_archive_df, basin, p)

    df <- purrr::map_df(l, dplyr::bind_rows)

    # At this point we have a list of links pointing to each storm, each basin.
    return(df)

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
