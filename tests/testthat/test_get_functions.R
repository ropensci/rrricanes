# Test all get functions
context("Get Functions")

## ---- Saved Data -------------------------------------------------------------
load(system.file("extdata", "al_01_1998_prblty.Rdata", package = "rrricanes"))
load(system.file("extdata", "al_09_2008_discus.Rdata", package = "rrricanes"))
load(system.file("extdata", "al_09_2008_fstadv.Rdata", package = "rrricanes"))
load(system.file("extdata", "al_09_2008_posest.Rdata", package = "rrricanes"))
load(system.file("extdata", "al_09_2008_public.Rdata", package = "rrricanes"))
load(system.file("extdata", "al_09_2008_update.Rdata", package = "rrricanes"))
load(system.file("extdata", "al_01_2017_products.Rdata", package = "rrricanes"))
load(system.file("extdata", "al_09_2008_wndprb.Rdata", package = "rrricanes"))

## ---- Get Data ---------------------------------------------------------------
al_1998 <- rrricanes::get_storms(years = 1998, basins = "AL")
al_2008 <- rrricanes::get_storms(years = 2008, basins = "AL")
al_2017 <- rrricanes::get_storms(years = 2017, basins = "AL")

df.al_01_2017_products <- rrricanes:::get_storm_data(al_2017[[1,4]],
                                      products = c("discus", "fstadv"))
df.al_09_2008_discus <- rrricanes:::get_discus(al_2008[[9,4]])
df.al_09_2008_posest <- rrricanes:::get_posest(al_2008[[9,4]])
df.al_01_1998_prblty <- rrricanes:::get_prblty(al_1998[[1,4]])
df.al_09_2008_public <- rrricanes:::get_public(al_2008[[9,4]])
df.al_09_2008_update <- rrricanes:::get_update(al_2008[[9,4]])
df.al_09_2008_wndprb <- rrricanes:::get_wndprb(al_2008[[9,4]])

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

## ---- * create_df_discus -----------------------------------------------------
#' Test structure of dataframe skeleton
test_that("Dataframe Skeleton", {
  df <- rrricanes:::create_df_discus()
  expect_true(is.data.frame(df))
  expect_true(tibble::is_tibble(df))
  expect_identical(class(df$Status), "character")
  expect_identical(class(df$Name), "character")
  expect_identical(class(df$Adv), "integer")
  expect_identical(class(df$Date), c("POSIXct", "POSIXt"))
  expect_identical(class(df$Key), "character")
  expect_identical(class(df$Contents), "character")
})

## ---- * get_discus -----------------------------------------------------------
#' Test return of get_discus()
test_that("Test get_discus()", {
  skip_on_cran()
  expect_identical(al_09_2008_discus, df.al_09_2008_discus)
})

## ---- fstadv -------------------------------------------------------------

## ---- * create_df_fstadv -----------------------------------------------------
test_that("Test create_df_fstadv", {
  x <- rrricanes:::create_df_fstadv()
  df <- tibble::data_frame(
    Status = character(), Name = character(), Adv = integer(),
    Date = as.POSIXct(character(), tz = "UTC"), Key = character(),
    Lat = numeric(), Lon = numeric(), Wind = integer(), Gust = integer(),
    Pressure = integer(), PosAcc = integer(), FwdDir = integer(),
    FwdSpeed = integer(), Eye = integer(), SeasNE = integer(),
    SeasSE = integer(), SeasSW = integer(), SeasNW = integer(),
    NE64 = integer(), SE64 = integer(), SW64 = integer(), NW64 = integer(),
    NE50 = integer(), SE50 = integer(), SW50 = integer(), NW50 = integer(),
    NE34 = integer(), SE34 = integer(), SW34 = integer(), NW34 = integer(),
    Hr12FcstDate = as.POSIXct(character(), tz = "UTC"), Hr12Lat = numeric(),
    Hr12Lon = numeric(), Hr12Wind = integer(), Hr12Gust = integer(),
    Hr12NE64 = integer(), Hr12SE64 = integer(), Hr12SW64 = integer(),
    Hr12NW64 = integer(), Hr12NE50 = integer(), Hr12SE50 = integer(),
    Hr12SW50 = integer(), Hr12NW50 = integer(), Hr12NE34 = integer(),
    Hr12SE34 = integer(), Hr12SW34 = integer(), Hr12NW34 = integer(),
    Hr24FcstDate = as.POSIXct(character(), tz = "UTC"), Hr24Lat = numeric(),
    Hr24Lon = numeric(), Hr24Wind = integer(), Hr24Gust = integer(),
    Hr24NE64 = integer(), Hr24SE64 = integer(), Hr24SW64 = integer(),
    Hr24NW64 = integer(), Hr24NE50 = integer(), Hr24SE50 = integer(),
    Hr24SW50 = integer(), Hr24NW50 = integer(), Hr24NE34 = integer(),
    Hr24SE34 = integer(), Hr24SW34 = integer(), Hr24NW34 = integer(),
    Hr36FcstDate = as.POSIXct(character(), tz = "UTC"), Hr36Lat = numeric(),
    Hr36Lon = numeric(), Hr36Wind = integer(), Hr36Gust = integer(),
    Hr36NE64 = integer(), Hr36SE64 = integer(), Hr36SW64 = integer(),
    Hr36NW64 = integer(), Hr36NE50 = integer(), Hr36SE50 = integer(),
    Hr36SW50 = integer(), Hr36NW50 = integer(), Hr36NE34 = integer(),
    Hr36SE34 = integer(), Hr36SW34 = integer(), Hr36NW34 = integer(),
    Hr48FcstDate = as.POSIXct(character(), tz = "UTC"), Hr48Lat = numeric(),
    Hr48Lon = numeric(), Hr48Wind = integer(), Hr48Gust = integer(),
    Hr48NE50 = integer(), Hr48SE50 = integer(), Hr48SW50 = integer(),
    Hr48NW50 = integer(), Hr48NE34 = integer(), Hr48SE34 = integer(),
    Hr48SW34 = integer(), Hr48NW34 = integer(),
    Hr72FcstDate = as.POSIXct(character(), tz = "UTC"), Hr72Lat = numeric(),
    Hr72Lon = numeric(), Hr72Wind = integer(), Hr72Gust = integer(),
    Hr72NE50 = integer(), Hr72SE50 = integer(), Hr72SW50 = integer(),
    Hr72NW50 = integer(), Hr72NE34 = integer(), Hr72SE34 = integer(),
    Hr72SW34 = integer(), Hr72NW34 = integer(),
    Hr96FcstDate = as.POSIXct(character(), tz = "UTC"), Hr96Lat = numeric(),
    Hr96Lon = numeric(), Hr96Wind = integer(), Hr96Gust = integer(),
    Hr120FcstDate = as.POSIXct(character(), tz = "UTC"), Hr120Lat = numeric(),
    Hr120Lon = numeric(), Hr120Wind = integer(), Hr120Gust = integer())
  expect_identical(x, df)
})

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

## ---- * create_df_posest -----------------------------------------------------
#' Test structure of dataframe skeleton
test_that("Dataframe Skeleton", {
  df <- rrricanes:::create_df_posest()
  expect_true(is.data.frame(df))
  expect_true(tibble::is_tibble(df))
  expect_identical(class(df$Status), "character")
  expect_identical(class(df$Name), "character")
  expect_identical(class(df$Date), c("POSIXct", "POSIXt"))
  expect_identical(class(df$Contents), "character")
})

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
  expect_identical(dim(x), c(216L, 7L))
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
