#' @title get_prblty
#' @description Return dataframe of strike probability data.
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
#' @seealso \code{\link{get_storms}}, \code{\link{prblty}}
#' @export
get_prblty <- function(link, msg = FALSE) {

    # Check status of link(s)
    valid.link <- sapply(link, status)
    valid.link <- na.omit(valid.link)
    if (length(valid.link) == 0)
        stop("No valid links.")

    products <- unlist(sapply(valid.link, get_products))

    products.prblty <- lapply(filter_prblty(products), prblty, msg = msg)

    prblty <- data.table::rbindlist(products.prblty)

    return(prblty)
}

#' @title prblty
#' @description Parse strike probability products
#' @details Given a direct link to a strike probability advisory product, parse
#' and return dataframe of values.
#' @param link Link to a storm's specific strike probability advisory product.
#' @param msg Display each link as being worked; default is FALSE
#' @return Dataframe
#' @seealso \code{\link{get_prblty}}
#' @keywords internal
prblty <- function(link, msg = FALSE) {

    contents <- scrape_contents(link, msg = msg)

    # Make sure this is a strike probability product
    if (!any(stringr::str_count(contents, c("MIASPFAT", "MIASPFEP"))))
        stop(sprintf("Invalid Strike Probability link. %s", link))

    df <- create_df_prblty()

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