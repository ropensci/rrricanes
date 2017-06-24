#' @title create_df_posest
#' @description Template for position estimates dataframe
#' @return empty dataframe
#' @seealso \code{\link{get_posest}}
#' @keywords internal
create_df_posest <- function() {
    df <- tibble::data_frame("Status" = character(),
                             "Name" = character(),
                             "Date" = as.POSIXct(character(), tz = "UTC"),
                             "Contents" = character())

    return(df)
}

#' @title get_posest
#' @description Return dataframe of position estimate data.
#' @details This product was discontinued after the 2013 hurricane season and is
#' now included in the Tropical Cyclone Update product (\code{\link{update}}).
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane,
#'     etc.}
#'   \item{Name}{Name of storm}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Contents}{Text content of product}
#' }
#' @param link URL to storm's archive page.
#' @seealso \code{\link{get_storms}}, \code{\link{posest}}
#' @export
get_posest <- function(link) {

    products <- purrr::map(link, get_products) %>% purrr::flatten_chr()

    products <- filter_posest(products)

    # Set progress bar
    p <- dplyr::progress_estimated(n = length(products))

    products.posest <- purrr::map(products, posest, p)

    posest <- purrr::map_df(products.posest, dplyr::bind_rows)

    return(posest)

}

#' @title posest
#' @description Extrapolate data from Position Estimate products.
#' @details Given a direct link to a position estimate product, parse and return
#' dataframe of values.
#' @param link URL of a specific position estimate product
#' @param p dplyr::progress_estimate.
#' @return Dataframe
#' @seealso \code{\link{get_posest}}
#' @keywords internal
posest <- function(link, p = dplyr::progress_estimated(n = 1)) {

    p$pause(0.5)$tick()$print()

    contents <- scrape_contents(link)

    # Replace all carriage returns with empty string.
    contents <- stringr::str_replace_all(contents, "\r", "")

    # Make sure this is a public advisory product
    if (!any(stringr::str_count(contents,
                                c("MIATCE", "MEATIEST", "WTNT"))))
        stop(sprintf("Invalid Position Estimate link. %s", link))

    df <- create_df_posest()

    status <- scrape_header(contents, ret = "status")
    name <- scrape_header(contents, ret = "name")
    date <- scrape_header(contents, ret = "date")

    if (getOption("rrricanes.working_msg"))
        message(sprintf("Working %s %s Position Estimate #%s (%s)",
                        status, name, date))

    df <- df %>%
        tibble::add_row("Status" = status,
                        "Name" = name,
                        "Date" = date,
                        "Contents" = contents)

    return(df)
}
