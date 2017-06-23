#' @title create_df_discus
#' @description Template for storm discussions dataframe
#' @return empty dataframe
#' @seealso \code{\link{get_discus}}
#' @keywords internal
create_df_discus <- function() {
    df <- tibble::data_frame("Status" = character(),
                             "Name" = character(),
                             # Allow for intermediate advisories,
                             # i.e., "1A", "2", "2A"...
                             "Adv" = character(),
                             "Date" = as.POSIXct(character(), tz = "UTC"),
                             "Contents" = character())

    return(df)
}

#' @title get_discus
#' @description Return dataframe of discussion data.
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane,
#'     etc.}
#'   \item{Name}{Name of storm}
#'   \item{Adv}{Advisory Number}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Contents}{Text content of product}
#' }
#' @param link URL to storm's archive page.
#' @seealso \code{\link{get_storms}}, \code{\link{public}}
#' @examples
#' \dontrun{
#' # Return dataframe of storm discussions for Tropical Storm Alex (AL011998)
#' get_discus("http://www.nhc.noaa.gov/archive/1998/1998ALEXadv.html")
#' }
#' @export
get_discus <- function(link) {

    products <- purrr::map(link, get_products) %>% purrr::flatten_chr()

    products <- filter_discus(products)

    # Set progress bar
    p <- dplyr::progress_estimated(n = length(products))

    products.discus <- purrr::map(products, discus, p)

    discus <- purrr::map_df(products.discus, dplyr::bind_rows)

    return(discus)

}

#' @title discus
#' @description Parse storm Discussion products
#' @details Given a direct link to a discussion product, parse and return
#' dataframe of values.
#' @param link Link to a storm's specific discussion product.
#' @param p dplyr::progress_estimate.
#' @return Dataframe
#' @seealso \code{\link{get_discus}}
#' @keywords internal
discus <- function(link, p = dplyr::progress_estimated(n = 1)) {

    p$pause(0.5)$tick()$print()

    contents <- scrape_contents(link)

    # Replace all carriage returns with empty string.
    contents <- stringr::str_replace_all(contents, "\r", "")

    # Make sure this is a discussion product
    if (!any(stringr::str_count(contents,
                                c("MIATCD", "MIATCM", "TCD", "WTPA", "WTPZ",
                                  "MIAWRKAD1")))) {
        # Check if the term "DISCUSSION" appears in header
        if (!stringr::str_detect(scrape_header(contents), "DISCUSSION"))
            stop(sprintf("Invalid Discussion link. %s", link))
    }

    df <- create_df_discus()

    status <- scrape_header(contents, ret = "status")
    name <- scrape_header(contents, ret = "name")
    adv <- scrape_header(contents, ret = "adv")
    date <- scrape_header(contents, ret = "date")

    if (getOption("rrricanes.working_msg"))
        message(sprintf("Working %s %s Storm Discussion #%s (%s)",
                        status, name, adv, date))

    df <- df %>%
        tibble::add_row("Status" = status,
                        "Name" = name,
                        "Adv" = adv,
                        "Date" = date,
                        "Contents" = contents)

    return(df)
}
