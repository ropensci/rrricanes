#' @title get_discus
#' @description Return dataframe of discussion data.
#' \describe{
#'   \item{Status}{Classification of storm, e.g., Tropical Storm, Hurricane,
#'   etc.}
#'   \item{Name}{Name of storm}
#'   \item{Adv}{Advisory Number}
#'   \item{Date}{Date of advisory issuance}
#'   \item{StormKey}{ID of cyclone}
#'   \item{Contents}{Text content of product}
#' }
#' @param links URL to storm's archive page.
#' @seealso \code{\link{get_storms}}, \code{\link{public}}
#' @examples
#' \dontrun{
#' # Return dataframe of storm discussions for Tropical Storm Alex (AL011998)
#' get_discus("http://www.nhc.noaa.gov/archive/1998/1998ALEXadv.html")
#' }
#' @export
get_discus <- function(links) {
  get_product(links = links, product = "discus")
}

#' @title discus
#' @description Parse storm Discussion products
#' @details Given a direct link to a discussion product, parse and return
#' dataframe of values.
#' @param contents Link to a storm's specific discussion product.
#' @return Dataframe
#' @seealso \code{\link{get_discus}}
#' @keywords internal
discus <- function(contents) {

  status <- scrape_header(
    contents = contents,
    # The "SPECIAL" pattern has to be left here; moving it under
    # `scrape_header` will break posest and update products.
    ptn_product_title = "(?:\n?SPECIAL\\s+)?(?:DISCUSSION)?"
  )

  issue_date <- scrape_date(contents)

  key <- scrape_key(contents)

  tibble::tibble(
    Status = status[,1],
    Name = status[,2],
    Adv = as.numeric(status[,3]),
    Date = issue_date,
    StormKey = key,
    Contents = contents
  )

}
