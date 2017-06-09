context("Scrapers")

# Set timeout options
opt.timeout <- getOption("rrricanes.http_timeout")
opt.attempts <- getOption("rrricanes.http_attempts")
options("rrricanes.http_timeout" = 1)
options("rrricanes.http_attempts" = 5)

## ---- Status -----------------------------------------------------------------
test_that("Status", {
    ## ---- * Tropical Storm Alex, Forecast/Advisory 1
    # This advisory uses the term "Tropical Depression"
    url <- "http://www.nhc.noaa.gov/archive/1998/archive/mar/MAL0198.001"
    expect_identical(scrape_header(scrape_contents(link = url), ret = "status"),
                     "Tropical Depression")
    ## ---- * Tropical Storm Alex, Forecast/Advisory 7
    # This advisory has "COR" appended on the second line
    # This advisory uses the term "Tropical Storm"
    url <- "http://www.nhc.noaa.gov/archive/1998/archive/mar/MAL0198.007"
    expect_identical(scrape_header(scrape_contents(link = url), ret = "status"),
                     "Tropical Storm")
    ## ---- * Tropical Storm Alex, Forecast/Advisory 8
    url <- "http://www.nhc.noaa.gov/archive/1998/archive/mar/MAL0198.008"
    expect_identical(scrape_header(scrape_contents(link = url), ret = "status"),
                     "Tropical Storm")
    ## ---- * Tropical Storm Alex, Forecast/Advisory 25
    # This advisory has an abnormal line, "...CORRECTED ADVISORY NUMBER..."
    url <- "http://www.nhc.noaa.gov/archive/1998/archive/mar/MAL0198.024"
    expect_identical(scrape_header(scrape_contents(link = url), ret = "status"),
                     "Tropical Storm")
    ## ---- * Tropical Storm Alex, Forecast/Advisory 26
    # This advisory uses the term "Tropical Disturbance"
    url <- "http://www.nhc.noaa.gov/archive/1998/archive/mar/MAL0198.026"
    expect_identical(scrape_header(scrape_contents(link = url), ret = "status"),
                     "Tropical Disturbance")
    ## ---- * Hurricane Bonnie, Forecast/Advisory 11
    # This advisory uses the term "Hurricane"
    url <- "http://www.nhc.noaa.gov/archive/1998/archive/mar/MAL0298.011"
    expect_identical(scrape_header(scrape_contents(link = url), ret = "status"),
                     "Hurricane")
    ## ---- * Tropical Storm Katrina, Forecast/Advisory 1
    url <- "http://www.nhc.noaa.gov/archive/1999/mar/MAL1599.001.html"
    expect_identical(scrape_header(scrape_contents(link = url), ret = "status"),
                     "Tropical Depression")
    ## ---- * Tropical Storm Arlene, Forecast/Advisory 9
    url <- "http://www.nhc.noaa.gov/archive/2017/al01/al012017.fstadv.009.shtml?"
    expect_identical(scrape_header(scrape_contents(link = url), ret = "status"),
                     "Post-Tropical Cyclone")
})

## ---- Name -------------------------------------------------------------------
test_that("Name", {
    ## ---- * Tropical Storm Alex, Forecast/Advisory 1
    url <- "http://www.nhc.noaa.gov/archive/1998/archive/mar/MAL0198.001"
    expect_identical(scrape_header(scrape_contents(link = url), ret = "name"),
                     "One")
    ## ---- * Tropical Storm Agatha, Forecast/Advisory 1
    url <- "http://www.nhc.noaa.gov/archive/1998/archive/mar/MEP0198.001"
    expect_identical(scrape_header(scrape_contents(link = url), ret = "name"),
                     "One-E")
    ## ---- * Tropical Storm Katrina, Forecast/Advisory 1
    url <- "http://www.nhc.noaa.gov/archive/1999/mar/MAL1599.001.html"
    expect_identical(scrape_header(scrape_contents(link = url), ret = "name"),
                     "Fifteen")
})

## ---- Adv --------------------------------------------------------------------
test_that("Adv", {
    #--------------------------------------------------------------------------#
    #                                                                          #
    # IMPORTANT!                                                               #
    # Advisories must return as character to accomodate intermediate           #
    # advisories; examples such as 1A, 10B, etc.                               #
    #                                                                          #
    #--------------------------------------------------------------------------#
    ## ---- * Tropical Storm Alex, Forecast/Advisory 1
    url <- "http://www.nhc.noaa.gov/archive/1998/archive/mar/MAL0198.001"
    expect_identical(scrape_header(scrape_contents(link = url), ret = "adv"),
                     "1")
    ## ---- * Tropical Storm Katrina, Forecast/Advisory 1
    url <- "http://www.nhc.noaa.gov/archive/1999/mar/MAL1599.001.html"
    expect_identical(scrape_header(scrape_contents(link = url), ret = "adv"),
                     "1")
})

## ---- Date -------------------------------------------------------------------
test_that("Date", {
    ## ---- * Tropical Storm Alex, Forecast/Advisory 1
    url <- "http://www.nhc.noaa.gov/archive/1998/archive/mar/MAL0198.001"
    expect_identical(scrape_header(scrape_contents(link = url), ret = "date"),
                     as.POSIXct("1998-07-27 15:00:00", tz = "UTC"))
    ## ---- * Tropical Storm Katrina, Forecast/Advisory 1
    url <- "http://www.nhc.noaa.gov/archive/1999/mar/MAL1599.001.html"
    expect_identical(scrape_header(scrape_contents(link = url), ret = "date"),
                     as.POSIXct("1999-10-28 21:00:00", tz = "UTC"))
})

## ---- Key --------------------------------------------------------------------
test_that("Key", {
    ## ---- * Tropical Storm Alex, Forecast/Advisory 1
    url <- "http://www.nhc.noaa.gov/archive/1998/archive/mar/MAL0198.001"
    expect_identical(scrape_header(scrape_contents(link = url), ret = "key"),
                     "AL011998")
    ## ---- * Tropical Storm Katrina, Forecast/Advisory 1
    url <- "http://www.nhc.noaa.gov/archive/1999/mar/MAL1599.001.html"
    expect_identical(scrape_header(scrape_contents(link = url), ret = "key"),
                     "AL151999")
})

# Reset options
options("rrricanes.http_timeout" = opt.timeout)
options("rrricanes.http_attempts" = opt.attempts)
