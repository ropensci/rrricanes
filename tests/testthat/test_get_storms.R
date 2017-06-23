context("Get Storms")

## ---- URL Status -------------------------------------------------------------
#' Test that annual archive links work. All results should return 'OK'.
test_that("URL Status", {
    skip_on_cran()
    url <- "http://www.nhc.noaa.gov/archive/1998/1998archive.shtml"
    expect_identical(httr::http_status(httr::GET(url))$reason, "OK")

    #' 1999 to current all have nearly identical links (year changes)
    url <- "http://www.nhc.noaa.gov/archive/%i/"
    urls <- sprintf(url, 1999:2016)
    lapply(urls, function(x) {
        expect_identical(httr::http_status(httr::GET(x))$reason, "OK")
        })
})

## ---- HTML format ------------------------------------------------------------
#' Test that annual archive page formats haven't changed.
test_that("HTML format", {

    skip_on_cran()

    #' Extract text value in row(r), column(c) at link. Cell count goes left to
    #' right, up to down starting at 1. There is a gap of 2 rowwise between each
    #' storm. So, if Atlantic storm NICOLE is (26, 1) then MADELINE is (28, 2)
    v <- function(r, c, link) {
        content <- link %>%
            xml2::read_html()

        path <- sprintf(
            paste0("//td[(((count(preceding-sibling::*) + 1) = %i) and ",
                   "parent::*)]//a[(((count(preceding-sibling::*) + 1) = %i) ",
                   "and parent::*)]"),
            c, r)

        x <- content %>%
            rvest::html_nodes(xpath = path) %>%
            rvest::html_text()

        return(x)
    }

    ## ---- * 1998 -------------------------------------------------------------
    #' 1998
    expect_identical(
        v(1, 1, "http://www.nhc.noaa.gov/archive/1998/1998archive.shtml"),
        "TROPICAL STORM ALEX")
    expect_identical(
        v(29, 2, "http://www.nhc.noaa.gov/archive/1998/1998archive.shtml"),
        "HURRICANE MADELINE")
    ## ---- * 2005 -------------------------------------------------------------
    #' 2005
    expect_identical(v(1, 1, "http://www.nhc.noaa.gov/archive/2005/"),
                     "Tropical Storm ARLENE")
    expect_identical(v(31, 2, "http://www.nhc.noaa.gov/archive/2005/"),
                     "Tropical Depression SIXTEEN-E")
    expect_identical(v(59, 1, "http://www.nhc.noaa.gov/archive/2005/"),
                     "Tropical Storm ZETA")
    ## ---- * 2016 -------------------------------------------------------------
    #' 2016
    expect_identical(v(29, 1, "http://www.nhc.noaa.gov/archive/2016/"),
                     "Hurricane NICOLE")
    expect_identical(v(41, 2, "http://www.nhc.noaa.gov/archive/2016/"),
                     "Tropical Storm TINA")
})

## ---- Is Dataframe -----------------------------------------------------------
test_that("Is Dataframe", {
    skip_on_cran()
    expect_true(is.data.frame(get_storms(1998, basin = "AL")))
    expect_true(is.data.frame(get_storms(1998, basin = "EP")))
})

## ---- Column Names -----------------------------------------------------------
test_that('Column Names', {
    skip_on_cran()
    expect_named(get_storms(2016, basin = "AL"),
                 c("Year", "Name", "Basin", "Link"))
    expect_named(get_storms(2016, basin = "EP"),
                 c("Year", "Name", "Basin", "Link"))
})

## ---- Errors -----------------------------------------------------------------
test_that("Errors", {
    skip_on_cran()
    expect_error(get_storms(1997),
                 'Archives currently only available for 1998 to current year.')
})
