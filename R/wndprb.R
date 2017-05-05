#' @title get_wndprb
#' @description Return dataframe of wind probability data.
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
#' @seealso \code{\link{get_storms}}, \code{\link{wndprb}}
#' @export
get_wndprb <- function(link, msg = FALSE) {

    # Check status of link(s)
    valid.link <- sapply(link, .status)
    valid.link <- na.omit(valid.link)
    if(length(valid.link) == 0)
        stop("No valid links.")

    products <- unlist(sapply(valid.link, get_products))

    products.wndprb <- lapply(filter_wind_probabilities(products), wndprb, msg = msg)

    wndprb <- data.table::rbindlist(products.wndprb)

    return(wndprb)
}

#' @title wndprb
#' @description Parse wind probability products
#' @details Given a direct link to a wind probability product, parse and return
#' dataframe of values.
#' @param link Link to a storm's specific wind probability product.
#' @param msg Display each link as being worked; default is FALSE
#' @return Dataframe
#' @seealso \code{\link{get_wndprb}}
#' @export
wndprb <- function(link, msg = FALSE) {

    contents <- scrape_contents(link, msg = msg)

    # Make sure this is a wndprb advisory product
    if(!any(stringr::str_count(contents, c("MIAPWSAT", "MIAPWSEP"))))
        stop(sprint("Invalid Wind Probability link. %s", l))

    df <- .create_df_wndprb()

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