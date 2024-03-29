#' @title extract_product_contents
#' @description Get and parse product contents for each link
#' @param links URLs to storm products
#' @param product specific product to parse
#' @keywords internal
extract_product_contents <- function(links, products) {

  if (length(links) == 0) {
    stop("No links input, please check that the data exist.")
  }

  # Some products may not exist within HTML but as strict text.
  safely_read_html <- purrr::safely(xml2::read_html)

  contents <-
    links |>
    get_url_contents()

    # Read in contents as html
    # If text is not within html, then we simply need to return the text.
    # Otherwise, extract the node from within the HTML and return the text of
    # that node.
  contents <- contents |> purrr::map_chr(.f = function(x) {
      txt <- safely_read_html(x)
      if (is.null(txt$result)) {
        x
      } else if (is.null(txt$error)) {
        txt$result |>
          rvest::html_node(xpath = "//pre") |>
          rvest::html_text() |>
          stringr::str_replace_all("\r", "") |>
          stringr::str_to_upper()
      }
    })
}
#' concept for isolating this step
#' @keywords internal

parse_product_contents <- function(contents, products){
  f <- match.fun(products)
  f(contents)
  #purrr::map(.x= contents, .f = match.fun(products))
}

#' @title extract_storm_links
#' @description Extract product links from a storm's archive page
#' @param links data frame with URLs to a storm's archive page
#' @param products Products to return
#' @keywords internal
extract_storm_links <- function(links, products) {
  if (length(links) == 0 ){
    stop("The links vector is empty.")
  }
  years <- attr(links, "years")
  if (!is.vector(links$Link))
    stop("Links must be a character vector.", call. = FALSE)

  # Get links of text products from each `links`
    html <- rvest::read_html(
      links$Link)
    product_links <-rvest::html_elements(html, "table td a")
    product_links <-  rvest::html_attr(x=product_links, name = "href")

    # Ensure we're only capturing archive pages
    product_links <- grep("archive", product_links, value = TRUE, fixed = TRUE)
  #product_links <- product_links[stats::complete.cases(product_links)]
    product_links <- product_links[!is.na(product_links)]
  # 1998 product links are relative and prefixed with "/archive/1998/" whereas
  # other years, product_links are absolute. If product_links exist for 1998
  # they must be modified. All product_links must then be prefixed with
  # NHC URL.

 product_links <- ifelse(!is.na(product_links) &years != 1998,
                paste0(get_nhc_link(withTrailingSlash = FALSE), product_links),
                paste0(get_nhc_link(withTrailingSlash = FALSE), "/1998/archive",
                       product_links)
            )

 # Needs to be revised to handle multiple products
 product_links[grep(products, product_links, fixed = TRUE)]
}

#' @title get_product
#' @description This function acts as a hub for the individual product extraction
#'   functions. Given the product and links, it will begin the scraping
#'   process and return a dataset for that product.
#' @param links a vector of links to storm pages
#' @param products  vector of products
#' @keywords internal
get_product <- function(links, products) {
     product_data <- get_storm_data(links, products)
     product_data
}

#' @title get_storm_data
#' @description Retrieve data from products.
#' @details \code{get_storm_data} is a wrapper function to make it more
#'   convenient to access the various storm products.
#'
#' Types of products:
#' \describe{
#'   \item{discus}{Storm Discussions. This is technical information on the
#'   cyclone such as satellite presentation, forecast model evaluation, etc.}
#'   \item{fstadv}{Forecast/Advisory. These products contain the meat of an
#'   advisory package. Current storm information is available as well as
#'   structural design and forecast data.}
#'   \item{posest}{Position Estimate. Issued generally when a storm is
#'   threatening; provides a brief update on location and winds.}
#'   \item{public}{Public Advisory. Issued for public knowledge; more often for
#'   Atlantic than East Pacific storms. Contains general information.}
#'   \item{prblty}{Strike Probability. Discontinued after the 2005 hurricane
#'   season, strike probabilities list the chances of x-force winds in a
#'   particular city.}
#'   \item{update}{Cyclone Update. Generally issued when a significant change
#'   occurs in the cyclone.}
#'   \item{windprb}{Wind Probability. Replace strike probabilities beginning in
#'   the 2006 season. Nearly identical.}
#' }
#'
#' Progress bars are displayed by default. These can be turned off by setting
#' the dplyr.show_progress to FALSE. Additionally, you can display messages for
#' each advisory being worked by setting the rrricanes.working_msg to TRUE.
#'
#' @param links to storm's archive page.
#' @param products Products to retrieve; discus, fstadv, posest, public,
#'   prblty, update, and windprb.
#' @return list of dataframes for each of the products.
#' @seealso \code{\link{get_ftp_storm_data}}
#' @examples
#' \dontrun{
#' ## Get public advisories for first storm of 2016 Atlantic season.
#' #get_storms(year = 2016, basin = "AL") |>
#'  # dplyr::slice(1) |>
#'  # pull(Link) |>
#'  # get_storm_data( products = "public")
#' ## Get public advisories and storm discussions for first storm of 2017 
#' Atlantic season.
#'# get_storms(year = 2017, basin = "AL") |>
#' #  slice(1) |>
#'  # pull(Link) |>
#'   # get_storm_data(products = c("discus", "public"))
#' }
#' @export
get_storm_data <- function(links,
                           products = c("discus", "fstadv", "posest",
                                        "public", "prblty", "update",
                                       "wndprb")) {

  products <- match.arg(products, several.ok = TRUE)
 # extract_product_contents(links, products)
  purrr::map2(links, products, extract_product_contents)
}

#' @title get_product_links
#' @param links data frame containing Link that lists storm page urls
#' @param product Data product
#' @return vector of links for specific storm and product
get_product_links<- function(links, product){
  year <-attr(links, "years")
  if (!is.character(links))
    stop("Links must be a character vector.", call. = FALSE)

  # Get links of text products from each `links`
  product_links <-
    extract_storm_links(links, product)
  product_links <- grep(product, product_links,
                       fixed = TRUE, value = TRUE)

  product_links
}
