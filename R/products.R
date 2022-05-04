#' @title twoal
#' @description Atlantic Tropical Weather Outlook
#' @details This function parses the latest xml tropical weather outlook for
#' the Atlantic ocean. The core data is located in the `channel$item` element
#' where `title`, `description` and `pubDate` reside. `link` is also
#' available to point to the NHC website.
#' @export
twoal <- function() {
  url <- sprintf("%sindex-at.xml", get_nhc_link())
  xml2::read_xml(url) |> xml2::as_list()
}

#' @title twoep
#' @description East Pacific Tropical Weather Outlook
#' @details This function parses the latest xml tropical weather outlook for
#' the east Pacific. The core data is located in the `channel$item` element
#' where `title`, `description` and `pubDate` reside. `link` is also
#' available to point to the NHC website.
#' @export
twoep <- function() {
  url <- sprintf("%sxml/TWOEP.xml", get_nhc_link())
  xml2::read_xml(url) |> xml2::as_list()
}
