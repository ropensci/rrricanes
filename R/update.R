#' @title get_update
#' @description Return dataframe of cyclone update data.
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane,
#'   etc.}
#'   \item{Name}{Name of storm}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Key}{Unique ID of cyclone}
#'   \item{Contents}{Text content of product}
#' }
#' @param links URL to storm's archive page.
#' @seealso \code{\link{get_storms}}, \code{\link{update}}
#' @export
get_update <- function(links) {
  get_product(links = links, product = "update")
}

#' @title update
#' @description Parse cyclone update products
#' @details Given a direct link to a cyclone update product, parse and return
#' dataframe of values.
#' @param contents Link to a storm's specific update advisory product.
#' @return Dataframe
#' @seealso \code{\link{get_update}}
#' @keywords internal
update <- function(contents) {

  # Replace all carriage returns with empty string.
  contents <- stringr::str_replace_all(contents, "\r", "")

  status <- scrape_header(contents)
  issue_date <- scrape_date(contents)
  key <- scrape_key(contents)

  tibble::tibble(
    Status = status[,1],
    Name = status[,2],
    Date = issue_date,
    Key = key,
    Contents = contents
  )

}
