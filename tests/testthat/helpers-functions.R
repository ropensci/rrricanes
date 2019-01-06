#' Extract text value in row(r), column(c) at link. Cell count goes left to
#' right, up to down starting at 1. There is a gap of 2 rowwise between each
#' storm. So, if Atlantic storm NICOLE is (26, 1) then MADELINE is (28, 2)
v <- function(r, c, link) {

  content <- xml2::read_html(link)

  path <- sprintf(
    stringr::str_c(
      "//td[(((count(preceding-sibling::*) + 1) = %i) and ",
      "parent::*)]//a[(((count(preceding-sibling::*) + 1) = %i) ",
      "and parent::*)]"
    ),
    c,
    r
  )

  content %>%
    rvest::html_nodes(xpath = path) %>%
    rvest::html_text()
}
