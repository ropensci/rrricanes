#' @title read_files
#' @description Given a product test file, this function imports the file
#'   contents and collapses into a string.
#' @param x File name located in inst/extdata directory.
read_files <- function(x) {

  f <- system.file("extdata", x, package = "rrricanes", mustWork = TRUE)
  readChar(f, file.info(f)$size
  )
}

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

  content |>
    rvest::html_nodes(xpath = path) |>
    rvest::html_text()
}
