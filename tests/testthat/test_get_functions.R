# Test all get functions


## ---- Get Storms -------------------------------------------------------------
if (!exists("al_2017")){
  al_2017 <- get_storms(years = 2017, basins = "AL")
}
if (!exists("al_1998")){
al_1998 <- get_storms(years = 1998, basins = "AL")
}
if (!exists("al_2008")){
  al_2008 <- get_storms(years = 2008, basins = "AL")
}
## ---- * URL Status -----------------------------------------------------------
#' Test that annual archive links work. All results should return 'OK'.
test_that("URL Status", {
  url <- sprintf("%sarchive/1998/1998archive.shtml", rrricanes:::get_nhc_link())
  expect_identical(httr::http_status(httr::GET(url))$reason, "OK")

  #' 1999 to current all have nearly identical links (year changes)
  url <- sprintf("%sarchive/%%i/", rrricanes:::get_nhc_link())
  urls <- sprintf(url, 1999:2016)
  lapply(urls, function(x) {
    expect_identical(httr::http_status(httr::GET(x))$reason, "OK")
  })
})

## ---- * HTML format ----------------------------------------------------------
#' Test that annual archive page formats haven't changed.
test_that("HTML format", {

  skip_on_cran()

  ## ---- * * 1998 -------------------------------------------------------------
  #' 1998
  expect_identical(
    v(1, 1, sprintf("%sarchive/1998/1998archive.shtml", rrricanes:::get_nhc_link())),
    "TROPICAL STORM ALEX")
  expect_identical(
    v(29, 2, sprintf("%sarchive/1998/1998archive.shtml", rrricanes:::get_nhc_link())),
    "HURRICANE MADELINE")
  ## ---- * * 2005 -------------------------------------------------------------
  #' 2005
  expect_identical(v(1, 1, sprintf("%sarchive/2005/", rrricanes:::get_nhc_link())),
                   "Tropical Storm ARLENE")
  expect_identical(v(31, 2, sprintf("%sarchive/2005/", rrricanes:::get_nhc_link())),
                   "Tropical Depression SIXTEEN-E")
  expect_identical(v(59, 1, sprintf("%sarchive/2005/", rrricanes:::get_nhc_link())),
                   "Tropical Storm ZETA")
  ## ---- * * 2016 -------------------------------------------------------------
  #' 2016
  expect_identical(v(29, 1, sprintf("%sarchive/2016/", rrricanes:::get_nhc_link())),
                   "Hurricane NICOLE")
  expect_identical(v(41, 2, sprintf("%sarchive/2016/", rrricanes:::get_nhc_link())),
                   "Tropical Storm TINA")
})

## ---- * Is Dataframe ---------------------------------------------------------
test_that("Is Dataframe", {
  expect_true(is.data.frame(get_storms(1998, basin = "AL")))
  expect_true(is.data.frame(get_storms(1998, basin = "EP")))
})

## ---- * Column Names ---------------------------------------------------------
test_that('Column Names', {
  expect_named(get_storms(2016, basin = "AL"),
               c("Year", "Name", "Basin", "Link"))
  expect_named(get_storms(2016, basin = "EP"),
               c("Year", "Name", "Basin", "Link"))
})

## ---- * Errors ---------------------------------------------------------------
test_that("Errors", {
  expect_error(get_storms(1997),
               sprintf("Param `years` must be between 1998 and %s.",
                       lubridate::year(Sys.Date())))
})

## ---- Get Storm Data ---------------------------------------------------------
test_that("rrricanes:::get_storm_data()", {
  # ## ---- * 2017, AL, 01 ---------------------------------------------------
  #----
  expect_identical(al_01_2017_products, df.al_01_2017_products)
  ## ---- * Errors -------------------------------------------------------------
  expect_error(rrricanes:::get_storm_data(al_2017[[1,4]], products = "test"))
  expect_error(rrricanes:::get_storm_data(),
               "argument \"links\" is missing, with no default")
})

## get_storm_list ----
# 2019-06-08 (Trice) - As of today, this test fails but I can't pinpoint why.
# read_csv gives a warning that it expected 21 columns but at row 2583 it got
# two columns. Looking over the CSV file, I do not see this. The last ob
# returned, Nadine (AL152018) is completely NA. I attempted to break the
# import into two pieces, skipping Nadine and binding the datasets, but nothing
# is returned after Nadine even with the "skip" parameter. No "problems" are
# generated from that attempt.
# So, if this test fails then it could be something else entirely. HOWEVER,
# check the results of `storm_list$warnings`; if the text file is fixed as it
# was prior, that will be the failure point.
test_that("Get Storm List", {

  quietly_get_storm_list <- purrr::quietly(.f = rrricanes::get_storm_list)
  storm_list <- quietly_get_storm_list()
  # Migrate to third edition of testthat
  #expect_output(str(storm_list$result), "tibble [2,580 × 21] (S3: tbl_df/tbl/data.frame)")
  expect_identical(
    names(storm_list$result),
    c("STORM_NAME", "RE", "X", "R2", "R3", "R4", "R5", "CY", "YYYY", "TY", "I",
      "YYY1MMDDHH", "YYY2MMDDHH", "SIZE", "GENESIS_NUM", "PAR1", "PAR2",
      "PRIORITY", "STORM_STATE", "WT_NUMBER", "STORMID"
    )
  )
  #expect_identical(
   # We need to switch to the third edition of
   # testthat and redo these with snapshot
   # storm_list$warnings,
   #  "1 parsing failure.\n row col   expected    actual         
   # file\n2583  -- 21 columns 2 columns literal data\n"
  #)
})
