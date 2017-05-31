#' @title twoep
#' @description East Pacific Tropical Weather Outlook
#' @details This function parses the latest xml tropical weather outlook for
#' the east Pacific. The core data is located in the `channel$item` element
#' where `title`, `description` and `pubDate` reside. `link` is also
#' available to point to the NHC website.
#' @return Returns nested list of xml nodes.
#' @seealso \link{http://www.nhc.noaa.gov/xml/TWOEP.xml}
#' @export
twoep <- function() {
    url <- "http://www.nhc.noaa.gov/xml/TWOEP.xml"
    contents <- xml2::read_xml(url) %>% xml2::as_list()
    return(contents)
}