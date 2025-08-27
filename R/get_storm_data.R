#' @title extract_product_contents
#' @description Get and parse product contents for each link
#' @param links URLs to storm products
#' @param product specific product to parse
#' @keywords internal
extract_product_contents <- function(product_links, products) {

  if (length(product_links) == 0) {
    stop("No links input, please check that the data exist.")
  }

  # Some products may not exist within HTML but as strict text.
  # safely_read_html <- purrr::safely(xml2::read_html)
  safely_read_html <- purrr::safely(rvest::read_html)
  contents <-
    product_links |>
    get_url_contents()


    # Read in contents as html
    # If text is not within html, then we simply need to return the text.
    # Otherwise, extract the node from within the HTML and return the text of
    # that node.
  contents <- contents |> purrr::map_chr(.f = function(x) {

      txt <- safely_read_html(x)

      if (is.null(txt$result)) {
        return(x)
      } else if (is.null(txt$error)) {
        .progress <- FALSE

        txt$result |>
          rvest::html_element(xpath = "//pre") |>
          rvest::html_text() |>
          stringr::str_replace_all("\r", "") |>
          stringr::str_to_upper()
      }
    })

  contents_parsed <-purrr::map2(contents, products, parse_product_contents)
  contents_parsed
}

#' concept for isolating this step
#' @keywords internal

parse_product_contents <- function(contents, products){

  f <- match.fun(products)
  print(f)
  purrr::map(.x = contents, .f = f)
}

#' @title extract_storm_links
#' @description Extract product links from a storm's archive page
#' @param links data frame with URLs to a storm's archive page
#' @param products Products to return
#' @keywords internal
extract_storm_links <- function(archive_links, products) {

  if (is.data.frame(archive_links)){
    archive_links <- archive_links |> dplyr::pull(Link)
  }
  if (length(archive_links) == 0 ){
    stop("The links vector is empty.")
  }

  if (!is.vector(archive_links)){
    stop("Links must be a character vector.", call. = FALSE)
  }
  if (!grepl("www.nhc.noaa.gov", archive_links, fixed = TRUE)){
    archive_links <- get_nhc_link(archive_links)
  }
``
  # Get links of text products from each submitted archive.
    html_storm <- rvest::read_html( archive_links)

    product_links <- html_storm |>
      rvest::html_elements(xpath ="//td//a") |>
      rvest::html_attr("href")

   product_links <- product_links[substr(product_links,
                                         (nchar(product_links) - 6),
                                           nchar(product_links)) == ".shtml?"]

    # 1998 product links are relative and prefixed with "/archive/1998/" whereas
    # other years, product_links are absolute. If product_links exist for 1998
    # they must be modified. All product_links must then be prefixed with
    # NHC URL.
    product_links <- ifelse(substr(product_links, 1, 8) != "/archive",
                             paste0("/1998/archive", product_links),
                             product_links)

    product_links_full <- paste0(get_nhc_link(withTrailingSlash = FALSE),
                                   product_links)

    product_links.df <- data.frame(link = product_links,
                                link_full = product_links_full)

    product_links.df |>
      dplyr::mutate(product_name = dplyr::case_when(
                        grepl("fstadv",  link, fixed = TRUE) == TRUE ~ "fsadv",
                        grepl("wndprb",  link, fixed = TRUE) == TRUE ~ "wndprb",
                        grepl("fstadv",  link, fixed = TRUE) == TRUE ~ "fsadv",
                        grepl("discus",  link, fixed = TRUE) == TRUE ~ "discus",
                        grepl("posest",  link, fixed = TRUE) == TRUE ~ "posest",
                        grepl("public",  link, fixed = TRUE) == TRUE ~ "public",
                        grepl("prblty",  link, fixed = TRUE) == TRUE ~ "prblty",
                        grepl("update",  link, fixed = TRUE) == TRUE ~ NA,
                        TRUE ~ NA
                        )
      )  |>  dplyr::filter(product_name %in% products == TRUE)

 product_links.df$link_full
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
#'
#' @param archive_links data frame with link to storm's archive page.
#' @param products Products to retrieve; discus, fstadv, posest, public,
#'   prblty, and windprb.
#' @return list of dataframes for each of the products.
#' @seealso \code{\link{get_ftp_storm_data}}
#' @examples
#' \dontrun{
#' ## Get public advisories for first storm of 2016 Atlantic season.
#' #get_storms(year = 2016, basin = "AL") |>
#'  # dplyr::slice_head(n=1) |>
#'  # pull(Link) |>
#'  # get_storm_data( products = "public")
#' ## Get public advisories and storm discussions for first storm of 2017
#' Atlantic season.
#'# get_storms(year = 2017, basin = "AL") |>
#' #  slice_head(n=1) |>
#'  # pull(Link) |>
#'   # get_storm_data(products = c("discus", "public"))
#' }
#' @export
get_storm_data <- function(archive_links,
                           products = c("discus", "fstadv", "posest",
                                        "public", "prblty",
                                       "wndprb")) {

   if (is.data.frame(archive_links)){
       years <- archive_links$Year
   }
   products <- match.arg(products, several.ok = TRUE)
   #attr(archive_links$Link, "year") <- years

   storm_content_links <- archive_links |> dplyr::pull(Link) |>
                             get_product_links(products)

   expanded <- tidyr::expand_grid(storm_content_links, products)
   expanded.links <- expanded |> dplyr::pull(storm_content_links)
   expanded.products <- expanded |> dplyr::pull(products)

   storm_contents <- purrr::map2(expanded.links, expanded.products,
               extract_product_contents, .progress = FALSE)

   storm_contents
}

#' @title get_product_links
#' @param links data frame containing Link that lists storm page urls
#' @param product Data product
#' @return vector of links for specific storm and product
get_product_links <- function(links, product){

  if (!is.character(links))
    stop("Links must be a character vector.", call. = FALSE)

  # Get links of text products from each `links`
  product_links <-
    extract_storm_links(links, product)

  product_links
}
