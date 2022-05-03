#' @title extract_product_contents
#' @description Get and parse product contents for each links
#' @param links URLs to storm products
#' @param product specific product to parse
#' @keywords internal
extract_product_contents <- function(links, products) {

  # Some products may not exist within HTML but as strict text.
  safely_read_html <- purrr::safely(xml2::read_html)

  contents <-
    links |>
    rrricanes:::get_url_contents()
  #contents <- sub(
  #  ".*<!-- START OF CONTENT -->(.+)<!-- End of content div -->.*",
   #                 "",
    #                contents, perl =TRUE )

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
ccc <<- contents
 contents <- data.frame(Text = contents)
  purrr::map(.x= products, .f = ~contents)

}

#' @title extract_storm_links
#' @description Extract product links from a storm's archive page
#' @param links URLs to a storm's archive page
#' @keywords internal
extract_storm_links <- function(links) {

  if (!is.vector(links))
    stop("Links must be a character vector.", call. = FALSE)

  # Get links of text products from each `links`
  product_links <-
    links |>
    rrricanes:::get_url_contents() |>
    purrr::imap(.f = xml2::read_html) |>
    # Extract the html tables from each link to get the storm's text products
    purrr::imap(.f = ~rvest::html_nodes(.x, xpath = "//td//a")) |>
    # Extract the text product URLs from `nodes`
    purrr::imap(.f = ~rvest::html_attr(.x, name = "href")) |>
    purrr::flatten_chr() |>
    # Ensure we're only capturing archive pages
    stringr::str_match( "archive.+")
  
  product_links <- product_links[stats::complete.cases(product_links)]

  # Extract years from `links`
  years <- as.numeric(stringr::str_extract(product_links, "[[:digit:]]{4}"))

  # 1998 product links are relative and prefixed with "/archive/1998/" whereas
  # other years, product_links are absolute. If product_links exist for 1998
  # they must be modified. All product_links must then be prefixed with
  # NHC URL.
  product_links[years == 1998] <- stringr::str_c("/archive/1998/",
                                                 product_links[years == 1998])
  product_links <- stringr::str_c(get_nhc_link(), product_links)

  product_links
}

#' @title get_product
#' @description This function acts as a hub for the individual product extraction
#'   functions. Given the product and links, it will begin the scraping
#'   process and return a dataset for that product.
#' @keywords internal
get_product <- function(links, product) {
  links |>
    purrr::map2(.y = product, .f = get_storm_data) |>
    purrr::flatten_df()
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
#' get_storms(year = 2016, basin = "AL") |>
#'   slice(1) |>
#'   .$Link |>
#'   get_storm_data(products = "public")
#' ## Get public advisories and storm discussions for first storm of 2017 Atlantic season.
#' get_storms(year = 2017, basin = "AL") |>
#'   slice(1) |>
#'   .$Link |>
#'   get_storm_data(products = c("discus", "public"))
#' }
#' @export
get_storm_data <- function(links,
                           products = c("discus", "fstadv", "posest",
                                               "public", "prblty", "update",
                                               "wndprb")) {

  products <- match.arg(products, several.ok = TRUE)
  product_links <- rrricanes:::extract_storm_links(links)
  # Filter links based on products and make one-dimensional
  filtered_links <- lapply(products,function(x) grep(x, product_links,
                                                     value = TRUE,
                                                     fixed = TRUE))
  names(filtered_links) <- products

  purrr::map2(filtered_links, products, extract_product_contents)

}
