#' @title create_df_public
#' @description Template for public advisory dataframe
#' @return empty dataframe
#' @seealso \code{\link{get_public}}
#' @keywords internal
create_df_public <- function() {
    df <- tibble::data_frame("Status" = character(),
                             "Name" = character(),
                             # Allow for intermediate advisories,
                             # i.e., "1A", "2", "2A"...
                             "Adv" = character(),
                             "Date" = as.POSIXct(character(), tz = "UTC"),
                             "Key" = character(),
                             "Contents" = character())

    return(df)
}

#' @title get_public
#' @description Return dataframe of public advisory data.
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane,
#'     etc.}
#'   \item{Name}{Name of storm}
#'   \item{Adv}{Advisory Number}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Key}{Unique ID of the cyclone}
#'   \item{Contents}{Text content of product}
#' }
#' @param link URL to storm's archive page.
#' @seealso \code{\link{get_storms}}, \code{\link{public}}
#' @export
get_public <- function(link) {

    products <- purrr::map(link, get_products) %>% purrr::flatten_chr()

    products <- filter_public(products)

    # Set progress bar
    p <- dplyr::progress_estimated(n = length(products))

    products.public <- purrr::map(products, public, p)

    public <- purrr::map_df(products.public, dplyr::bind_rows)

    return(public)
}

#' @title public
#' @description Parse Public Advisory products
#' @details Given a direct link to a public advisory product, parse and return
#' dataframe of values.
#' @param link Link to a storm's specific public advisory product.
#' @param p dplyr::progress_estimate.
#' @return Dataframe
#' @seealso \code{\link{get_public}}
#' @keywords internal
public <- function(link, p = dplyr::progress_estimated(n = 1)) {

    p$pause(0.5)$tick()$print()

    contents <- scrape_contents(link)

    # Replace all carriage returns with empty string.
    contents <- stringr::str_replace_all(contents, "\r", "")

    # Make sure this is a public advisory product
    if (!any(stringr::str_count(contents, c("MIATCP", "TCP", "WTPA",
                                            "MIAWRKAP"))))
        stop(sprintf("Invalid Public Advisory link. %s", link))

    df <- create_df_public()

    status <- scrape_header(contents, ret = "status")
    name <- scrape_header(contents, ret = "name")
    adv <- scrape_header(contents, ret = "adv")
    date <- scrape_header(contents, ret = "date")

    safely_scrape_header <- purrr::safely(scrape_header)
    key <- safely_scrape_header(contents, ret = "key")
    if (is.null(key$error)) {
        key <- key$result
    } else {
        key <- NA
    }

    if (getOption("rrricanes.working_msg"))
        message(sprintf("Working %s %s Public Advisory #%s (%s)",
                        status, name, adv, date))

    df <- df %>%
        tibble::add_row("Status" = status,
                        "Name" = name,
                        "Adv" = adv,
                        "Date" = date,
                        "Key" = key,
                        "Contents" = contents)

    return(df)
}
