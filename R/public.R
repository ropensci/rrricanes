#' @title get_public
#' @description Return dataframe of public advisory data.
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane,
#'   etc.}
#'   \item{Name}{Name of storm}
#'   \item{Adv}{Advisory Number}
#'   \item{Date}{Date of advisory issuance}
#'   \item{Key}{Unique ID of the cyclone}
#'   \item{Contents}{Text content of product}
#' }
#' @param links URL to storm's archive page.
#' @seealso \code{\link{get_storms}}, \code{\link{public}}
#' @export
get_public <- function(links) {
  get_product(links = links, product = "public")
}

#' @title public
#' @description Parse Public Advisory products
#' @details Given a direct link to a public advisory product, parse and return
#' dataframe of values.
#' @param contents Link to a storm's specific public advisory product.
#' @return Dataframe
#' @seealso \code{\link{get_public}}
#' @keywords internal
public <- function(contents) {

  status <- scrape_header(contents)
  issue_date <- scrape_date(contents)
  key <- scrape_key(contents)

  tibble::tibble(
    Status = status[,1],
    Name = status[,2],
    Adv = status[,3],
    Date = issue_date,
    Key = key,
    Contents = contents
  )

}
