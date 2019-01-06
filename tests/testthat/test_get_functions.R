# Test all get functions
context("Get Functions")

## ---- Get Storms -------------------------------------------------------------

## ---- * URL Status -----------------------------------------------------------
#' Test that annual archive links work. All results should return 'OK'.
test_that("URL Status", {
  skip_on_cran()
  url <- sprintf("%sarchive/1998/1998archive.shtml", get_nhc_link())
  expect_identical(httr::http_status(httr::GET(url))$reason, "OK")

  #' 1999 to current all have nearly identical links (year changes)
  url <- sprintf("%sarchive/%%i/", get_nhc_link())
  urls <- sprintf(url, 1999:2016)
  lapply(urls, function(x) {
    expect_identical(httr::http_status(httr::GET(x))$reason, "OK")
  })
})

## ---- * HTML format ----------------------------------------------------------
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

  ## ---- * * 1998 -------------------------------------------------------------
  #' 1998
  expect_identical(
    v(1, 1, sprintf("%sarchive/1998/1998archive.shtml", get_nhc_link())),
    "TROPICAL STORM ALEX")
  expect_identical(
    v(29, 2, sprintf("%sarchive/1998/1998archive.shtml", get_nhc_link())),
    "HURRICANE MADELINE")
  ## ---- * * 2005 -------------------------------------------------------------
  #' 2005
  expect_identical(v(1, 1, sprintf("%sarchive/2005/", get_nhc_link())),
                   "Tropical Storm ARLENE")
  expect_identical(v(31, 2, sprintf("%sarchive/2005/", get_nhc_link())),
                   "Tropical Depression SIXTEEN-E")
  expect_identical(v(59, 1, sprintf("%sarchive/2005/", get_nhc_link())),
                   "Tropical Storm ZETA")
  ## ---- * * 2016 -------------------------------------------------------------
  #' 2016
  expect_identical(v(29, 1, sprintf("%sarchive/2016/", get_nhc_link())),
                   "Hurricane NICOLE")
  expect_identical(v(41, 2, sprintf("%sarchive/2016/", get_nhc_link())),
                   "Tropical Storm TINA")
})

## ---- * Is Dataframe ---------------------------------------------------------
test_that("Is Dataframe", {
  skip_on_cran()
  expect_true(is.data.frame(rrricanes::get_storms(1998, basin = "AL")))
  expect_true(is.data.frame(rrricanes::get_storms(1998, basin = "EP")))
})

## ---- * Column Names ---------------------------------------------------------
test_that('Column Names', {
  skip_on_cran()
  expect_named(rrricanes::get_storms(2016, basin = "AL"),
               c("Year", "Name", "Basin", "Link"))
  expect_named(rrricanes::get_storms(2016, basin = "EP"),
               c("Year", "Name", "Basin", "Link"))
})

## ---- * Errors ---------------------------------------------------------------
test_that("Errors", {
  skip_on_cran()
  expect_error(rrricanes::get_storms(1997),
               sprintf("Param `years` must be between 1998 and %s.",
                       lubridate::year(Sys.Date())))
})

## ---- Get Storm Data ---------------------------------------------------------
test_that("rrricanes:::get_storm_data()", {
  skip_on_cran()
  ## ---- * 2017, AL, 01 -------------------------------------------------------
  expect_identical(al_01_2017_products, df.al_01_2017_products)
  ## ---- * Errors -------------------------------------------------------------
  expect_error(rrricanes:::get_storm_data(al_2017[[1,4]], products = "test"))
  expect_error(rrricanes:::get_storm_data(),
               "argument \"links\" is missing, with no default")
})

## ---- discus -----------------------------------------------------------------

## ---- * get_discus -----------------------------------------------------------
#' Test return of get_discus()
test_that("Test get_discus()", {
  skip_on_cran()
  expect_identical(al_09_2008_discus, df.al_09_2008_discus)
})

## ---- fstadv -------------------------------------------------------------
## ---- * tidy_fstadv ----------------------------------------------------------
test_that("Test tidy_fstadv()", {
  expect_warning(x <- rrricanes::tidy_fstadv(al_09_2008_fstadv))
  expect_identical(dim(x), c(53L, 18L))
  expect_identical(names(x), c("Key", "Adv", "Date", "Status", "Name", "Lat",
                               "Lon", "Wind", "Gust", "Pressure", "PosAcc",
                               "FwdDir", "FwdSpeed", "Eye", "SeasNE", "SeasSE",
                               "SeasSW", "SeasNW"))
})

## ---- * tidy_wr --------------------------------------------------------------
test_that("Test tidy_wr()", {
  x <- rrricanes::tidy_wr(al_09_2008_fstadv)
  expect_identical(dim(x), c(138L, 8L))
  expect_identical(names(x), c("Key", "Adv", "Date", "WindField", "NE", "SE",
                               "SW", "NW"))
})

## ---- * tidy_fcst ------------------------------------------------------------
test_that("Test tidy_fcst()", {
  x <- rrricanes::tidy_fcst(al_09_2008_fstadv)
  expect_identical(dim(x), c(336L, 8L))
  expect_identical(names(x), c("Key", "Adv", "Date", "FcstDate", "Lat", "Lon",
                               "Wind", "Gust"))
})

## ---- * tidy_fcst_wr ---------------------------------------------------------
test_that("Test tidy_fcst_wr()", {
  x <- rrricanes::tidy_fcst_wr(al_09_2008_fstadv)
  expect_identical(dim(x), c(587L, 9L))
  expect_identical(names(x), c("Key", "Adv", "Date", "FcstDate", "WindField",
                               "NE", "SE", "SW", "NW"))
})

## ---- posest -----------------------------------------------------------------

## ---- * get_posest -----------------------------------------------------------
test_that("Test get_posest()", {
  skip_on_cran()
  expect_identical(al_09_2008_posest, df.al_09_2008_posest)
})

## ---- prblty -----------------------------------------------------------------

## ---- * get_prblty -----------------------------------------------------------
test_that("get_prblty()", {
  skip_on_cran()
  expect_identical(al_01_1998_prblty, df.al_01_1998_prblty)
})

## ---- public -----------------------------------------------------------------

## ---- * get_public -----------------------------------------------------------
test_that("get_public", {
  skip_on_cran()
  expect_equal(al_09_2008_public, df.al_09_2008_public)
})

## ---- update -----------------------------------------------------------------

## ---- Test get_update() ------------------------------------------------------
test_that("get_update", {
  skip_on_cran()
  expect_identical(al_09_2008_update, df.al_09_2008_update)
})

## ---- wndprb -----------------------------------------------------------------

## ---- * al_prblty_stations ---------------------------------------------------
test_that("al_prblty_stations", {
  expect_warning(x <- rrricanes::al_prblty_stations(),
                 "Expected 7 pieces. Additional pieces discarded in 1 rows [90].",
                 fixed = TRUE)
  expect_identical(dim(x), c(214L, 7L))
  expect_identical(names(x),
                   c("X1", "Location", "Lat", "Lon", "X5", "X6", "X7"))
})

## ---- * cp_prblty_stations ---------------------------------------------------
test_that("cp_prblty_stations", {
  expect_identical(dim(rrricanes::cp_prblty_stations()), c(168L, 7L))
  expect_identical(names(rrricanes::cp_prblty_stations()),
                   c("X1", "Location", "Lat", "Lon", "X5", "X6", "X7"))
})

## ---- * ep_prblty_stations ---------------------------------------------------
test_that("ep_prblty_stations", {
  expect_warning(x <- rrricanes::ep_prblty_stations(),
                 "Expected 7 pieces. Missing pieces filled with `NA` in 1 rows [41].",
                 fixed = TRUE)
  expect_identical(dim(x), c(168L, 7L))
  expect_identical(names(x),
                   c("X1", "Location", "Lat", "Lon", "X5", "X6", "X7"))
})

## ---- * get_wndprb -----------------------------------------------------------
test_that("Test get_wndprb()", {
  skip_on_cran()
  expect_identical(al_09_2008_wndprb, df.al_09_2008_wndprb)
})

## get_storm_list ----
test_that("Get Storm List", {
  skip_on_travis()
  storm_list <- rrricanes::get_storm_list()
  expect_output(str(storm_list), "21 variables")
  expect_identical(
    names(storm_list),
    c("STORM_NAME", "RE", "X", "R2", "R3", "R4", "R5", "CY", "YYYY", "TY", "I",
      "YYY1MMDDHH", "YYY2MMDDHH", "SIZE", "GENESIS_NUM", "PAR1", "PAR2",
      "PRIORITY", "STORM_STATE", "WT_NUMBER", "STORMID"
    )
  )
})
