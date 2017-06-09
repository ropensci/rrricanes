context("Test base functions.")

## ---- get_nhc_link() ---------------------------------------------------------
test_that("NHC Link", {
    expect_identical(get_nhc_link(), "http://www.nhc.noaa.gov/")
    expect_identical(get_nhc_link(withTrailingSlash = FALSE),
                     "http://www.nhc.noaa.gov")
})

## ---- month_str_to_num() -----------------------------------------------------
test_that("Month Abbreviated String to Number", {
    expect_identical(month_str_to_num("JAN"), as.integer(1))
    expect_identical(month_str_to_num("FEB"), as.integer(2))
    expect_identical(month_str_to_num("MAR"), as.integer(3))
    expect_identical(month_str_to_num("APR"), as.integer(4))
    expect_identical(month_str_to_num("MAY"), as.integer(5))
    expect_identical(month_str_to_num("JUN"), as.integer(6))
    expect_identical(month_str_to_num("JUL"), as.integer(7))
    expect_identical(month_str_to_num("AUG"), as.integer(8))
    expect_identical(month_str_to_num("SEP"), as.integer(9))
    expect_identical(month_str_to_num("OCT"), as.integer(10))
    expect_identical(month_str_to_num("NOV"), as.integer(11))
    expect_identical(month_str_to_num("DEC"), as.integer(12))
})

test_that("Month Abbreviated String to Number Error", {
    expect_error(month_str_to_num("JANN"),
                 "JANN is not a valid month abbreviation")
})

## ---- convert_lat_lon() ------------------------------------------------------
test_that("Convert Latitude, Longitude", {
    expect_identical(convert_lat_lon(93.1, "N"), as.numeric(93.1))
    expect_identical(convert_lat_lon(93.1, "S"), as.numeric(-93.1))
    expect_identical(convert_lat_lon(179.0, "E"), as.numeric(179.0))
    expect_identical(convert_lat_lon(179, "W"), as.numeric(-179))
    expect_error(convert_lat_lon(179, "X"), "y must be")
})

## ---- * Errors ---------------------------------------------------------------
test_that("Lat/Lon is not numeric", {
    expect_error(convert_lat_lon("93.1N", "N"), "x is not numeric")
})

## ---- knots_to_mph() ---------------------------------------------------------
test_that("Knots to Miles per Hour", {
    expect_equal(knots_to_mph(91), 104.72093)
    expect_equal(knots_to_mph(274), 315.313569)
})

## ---- mb_to_in() -------------------------------------------------------------
test_that("Millibars to Inches", {
    expect_equal(mb_to_in(1010), 29.82528290171)
    expect_equal(mb_to_in(888), 26.222624967048)
})

## ---- saffir() ---------------------------------------------------------------
test_that("test saffir()", {
    expect_identical(saffir(c(32, 45, 70, 90, 110, 125, 140)),
                     c("TD", "TS", "HU1", "HU2", "HU3", "HU4", "HU5"))
})

## ---- status() ---------------------------------------------------------------
y <- lubridate::year(Sys.Date()) + 1
test_that("URL Status", {
    expect_warning(
        status(u = sprintf("http://www.nhc.noaa.gov/archive/%d/", y)),
        sprintf("URL unavailable. http://www.nhc.noaa.gov/archive/%d/", y))
})

## ---- status_abbr_to_str() ---------------------------------------------------
test_that("test status_abbr_to_str()", {
    expect_identical(status_abbr_to_str("TD"), "Tropical Depression")
    expect_identical(status_abbr_to_str("TS"), "Tropical Storm")
    expect_identical(status_abbr_to_str("HU"), "Hurricane")
    expect_identical(status_abbr_to_str("EX"), "Extratropical Cyclone")
    expect_identical(status_abbr_to_str("SD"), "Subtropical Depression")
    expect_identical(status_abbr_to_str("SS"), "Subtropical Storm")
    expect_identical(status_abbr_to_str("LO"), "Low")
    expect_identical(status_abbr_to_str("WV"), "Tropical Wave")
    expect_identical(status_abbr_to_str("DB"), "Disturbance")
})