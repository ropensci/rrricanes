#' @title create_df_posest
#' @description Template for position estimates dataframe
#' @return empty dataframe
#' @seealso \code{\link{get_posest}}
#' @keywords internal
create_df_posest <- function() {
    df <- tibble::data_frame("Status" = character(),
                             "Name" = character(),
                             # Allow for intermediate advisories, i.e., "1A", "2", "2A"...
                             "Adv" = character(),
                             "Date" = as.POSIXct(character(), tz = "UTC"),
                             "Contents" = character())

    return(df)
}

#' @title get_posest
#' @description Return dataframe of position estimate data.
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane,
#'     etc.}
#'   \item{Name}{Name of storm}
#'   \item{Adv}{Advisory Number}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Contents}{Text content of product}
#' }
#' @param link URL to storm's archive page.
#' @param msg Show link being worked. Default, FALSE.
#' @seealso \code{\link{get_storms}}, \code{\link{posest}}
#' @export
get_posest <- function(link, msg = FALSE) {

    # Check status of link(s)
    valid.link <- sapply(link, status)
    valid.link <- na.omit(valid.link)
    if (length(valid.link) == 0)
        stop("No valid links.")

    products <- purrr::map(valid.link, get_products) %>% purrr::flatten_chr()

    products.posest <- purrr::map(filter_position_estimate(products), posest)

    posest <- purrr::map_df(products.posest, dplyr::bind_rows)

    return(posest)

}

#' @title posest
#' @description Extrapolate data from Position Estimate products.
#' @details Given a direct link to a position estimate product, parse and return
#' dataframe of values.
#' @param link URL of a specific position estimate product
#' @param msg Display each link as being worked; default is FALSE.
#' @return Dataframe
#' @seealso \code{\link{get_posest}}
#' @keywords internal
posest <- function(link, msg = FALSE) {

    contents <- scrape_contents(link, msg = msg)

    # Make sure this is a public advisory product
    if (!any(stringr::str_count(contents, c("MIATCEAT", "MIATCEEP"))))
        stop(sprintf("Invalid Position Estimate link. %s", link))

    df <- create_df_posest()

    status <- scrape_header(contents, ret = "status")
    name <- scrape_header(contents, ret = "name")
    adv <- scrape_header(contents, ret = "adv")
    date <- scrape_header(contents, ret = "date")

    df <- df %>%
        tibble::add_row("Status" = status,
                        "Name" = name,
                        "Adv" = adv,
                        "Date" = date,
                        "Contents" = contents)

    return(df)
}
