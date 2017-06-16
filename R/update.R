#' @title create_df_update
#' @description Template for cyclone update dataframe
#' @return empty dataframe
#' @seealso \code{\link{get_update}}
#' @keywords internal
create_df_update <- function() {
    df <- tibble::data_frame("Status" = character(),
                             "Name" = character(),
                             # Allow for intermediate advisories,
                             # i.e., "1A", "2", "2A"...
                             "Adv" = character(),
                             "Date" = as.POSIXct(character(), tz = "UTC"),
                             "Contents" = character())

    return(df)
}

#' @title get_update
#' @description Return dataframe of cyclone update data.
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane,
#'     etc.}
#'   \item{Name}{Name of storm}
#'   \item{Adv}{Advisory Number}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Contents}{Text content of product}
#' }
#' @param link URL to storm's archive page.
#' @seealso \code{\link{get_storms}}, \code{\link{update}}
#' @export
get_update <- function(link) {

    products <- purrr::map(link, get_products) %>% purrr::flatten_chr()

    products <- filter_update(products)

    # Set progress bar
    p <- dplyr::progress_estimated(n = length(products))

    products.update <- purrr::map(products, update, p)

    update <- purrr::map_df(products.update, dplyr::bind_rows)

    return(update)
}

#' @title update
#' @description Parse cyclone update products
#' @details Given a direct link to a cyclone update product, parse and return
#' dataframe of values.
#' @param link Link to a storm's specific update advisory product.
#' @param p dplyr::progress_estimate.
#' @return Dataframe
#' @seealso \code{\link{get_update}}
#' @keywords internal
update <- function(link, p = dplyr::progress_estimated(n = 1)) {

    p$pause(0.5)$tick()$print()

    contents <- scrape_contents(link)

    # Replace all carriage returns with empty string.
    contents <- stringr::str_replace_all(contents, "\r", "")

    # Make sure this is a update advisory product
    if (!any(stringr::str_count(contents, c("MIATCU", "TCU"))))
        stop(sprintf("Invalid Cyclone Update link. %s", link))

    df <- create_df_update()

    status <- scrape_header(contents, ret = "status")
    name <- scrape_header(contents, ret = "name")
    adv <- scrape_header(contents, ret = "adv")
    date <- scrape_header(contents, ret = "date")

    if (getOption("rrricanes.working_msg"))
        message(sprintf("Working %s %s Update #%s (%s)",
                        status, name, adv, date))

    df <- df %>%
        tibble::add_row("Status" = status,
                        "Name" = name,
                        "Adv" = adv,
                        "Date" = date,
                        "Contents" = contents)

    return(df)
}
