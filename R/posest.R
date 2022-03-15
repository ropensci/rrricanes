#' @title get_posest
#' @description Return dataframe of position estimate data.
#' @details This product was discontinued after the 2013 hurricane season and is
#' now included in the Tropical Cyclone Update product (\code{\link{update}}).
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane,
#'   etc.}
#'   \item{Name}{Name of storm}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Contents}{Text content of product}
#' }
#' @param links URL to storm's archive page.
#' @seealso \code{\link{get_storms}}, \code{\link{posest}}
#' @export
get_posest <- function(links) {
  get_product(links = links, product = "posest")
}

#' @title posest
#' @description Extrapolate data from Position Estimate products.
#' @details Given a direct link to a position estimate product, parse and return
#' dataframe of values.
#' @param contents URL of a specific position estimate product
#' @return Dataframe
#' @seealso \code{\link{get_posest}}
#' @keywords internal
posest <- function(contents) {
  status <- scrape_header(
    contents = contents,
    ptn_product_title = "(?:POSITION ESTIMATE)?",
    advisory_number = FALSE
  )

  issue_date <- scrape_date(contents)

  key <- scrape_key(contents)

  tibble::tibble(
    Status = status[, 1],
    Name = status[, 2],
    Date = issue_date,
    Contents = contents
  )
}
