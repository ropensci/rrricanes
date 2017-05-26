context("Forecast/Advisory Products (fstadv)")

## ---- Data -------------------------------------------------------------------

## ---- * Current Data ---------------------------------------------------------
# Pre-load dataframes
url.al011998 <- "http://www.nhc.noaa.gov/archive/1998/1998ALEXadv.html"
df.al011998 <- get_fstadv(link = url.al011998)
url.al011998.01 <- "http://www.nhc.noaa.gov/archive/1998/archive/mar/MAL0198.001"
df.al011998.01 <- fstadv(link = url.al011998.01)
url.al011998.07 <- "http://www.nhc.noaa.gov/archive/1998/archive/mar/MAL0198.007"
df.al011998.07 <- fstadv(link = url.al011998.07)
url.al021998.08 <- "http://www.nhc.noaa.gov/archive/1998/archive/mar/MAL0298.008"
df.al021998.08 <- fstadv(link = url.al021998.08)
url.al021998.11 <- "http://www.nhc.noaa.gov/archive/1998/archive/mar/MAL0298.011"
df.al021998.11 <- fstadv(link = url.al021998.11)

#' Get all storms for 1998, AL basin
al1998 <- get_storms(year = 1998, basin = "AL") %>%
    dplyr::select(Link) %>%
    purrr::flatten_chr()

#' Get all storms for 1998, EP basin
ep1998 <- get_storms(year = 1998, basin = "EP") %>%
    dplyr::select(Link) %>%
    purrr::flatten_chr()

#' Get all storms for 1999, EP basin
ep1999 <- get_storms(year = 1999, basin = "EP") %>%
    dplyr::select(Link) %>%
    purrr::flatten_chr()

#' Get storms for 2000, AL basin
al2000 <- get_storms(year = 2000, basin = "AL") %>%
    dplyr::select(Link) %>%
    purrr::flatten_chr()

#' Get storms for 2000, EP basin
ep2000 <- get_storms(year = 2000, basin = "EP") %>%
    dplyr::select(Link) %>%
    purrr::flatten_chr()

#' Get storms for 2002, AL basin
al2002 <- get_storms(year = 2002, basin = "AL") %>%
    dplyr::select(Link) %>%
    purrr::flatten_chr()

#' Get storms for 2005, AL basin
al2005 <- get_storms(year = 2005, basin = "AL") %>%
    dplyr::select(Link) %>%
    purrr::flatten_chr()

#' Load Tropical Storm Alex
df.al011998.fstadv <- get_fstadv(al1998[1])
#' Load Hurricane Bonnie
df.al021998.fstadv <- get_fstadv(al1998[2])
#' Load Tropical Storm Agatha
df.ep011998.fstadv <- get_fstadv(ep1998[1])
#' Load Tropical Depression Two-E
df.ep021998.fstadv <- get_fstadv(ep1998[2])
## ---- * 1998, EP, 08 ---------------------------------------------------------
df.ep081998.fstadv <- get_fstadv(ep1998[8])
#' Load Tropical Depression Three-E
df.ep031999.fstadv <- get_fstadv(ep1999[3])
#' Load Tropical Depression Four-E
df.ep041999.fstadv <- get_fstadv(ep1999[4])
#' Load Hurricane Dora
df.ep071999.fstadv <- get_fstadv(ep1999[7])
#' Load Hurricane Eugene
df.ep081999.fstadv <- get_fstadv(ep1999[8])
## ---- * 2000, AL, 02 ---------------------------------------------------------
df.al022000.fstadv <- get_fstadv(al2000[2])
## ---- * 2000, EP, 06 ---------------------------------------------------------
df.ep062000.fstadv <- get_fstadv(ep2000[6])
## ---- * 2002, AL, 13 ---------------------------------------------------------
df.al132002.fstadv <- get_fstadv(al2002[13])
## ---- * 2005, AL, 30 ---------------------------------------------------------
df.al302005.fstadv <- get_fstadv(al2005[30])

## ---- * Saved Data -----------------------------------------------------------
load(system.file("extdata", "al011998.fstadv.Rda", package = "Hurricanes"))
load(system.file("extdata", "al021998.fstadv.Rda", package = "Hurricanes"))
load(system.file("extdata", "ep011998.fstadv.Rda", package = "Hurricanes"))
load(system.file("extdata", "ep021998.fstadv.Rda", package = "Hurricanes"))
load(system.file("extdata", "ep081998.fstadv.Rda", package = "Hurricanes"))
load(system.file("extdata", "ep031999.fstadv.Rda", package = "Hurricanes"))
load(system.file("extdata", "ep041999.fstadv.Rda", package = "Hurricanes"))
load(system.file("extdata", "ep071999.fstadv.Rda", package = "Hurricanes"))
load(system.file("extdata", "ep081999.fstadv.Rda", package = "Hurricanes"))
load(system.file("extdata", "al022000.fstadv.Rda", package = "Hurricanes"))
load(system.file("extdata", "ep062000.fstadv.Rda", package = "Hurricanes"))
load(system.file("extdata", "al132002.fstadv.Rda", package = "Hurricanes"))
load(system.file("extdata", "al302005.fstadv.Rda", package = "Hurricanes"))

## ---- Test get_fstadv() ------------------------------------------------------
#' Test return of get_fstadv()
test_that("Test get_fstadv()", {
    expect_true(is.data.frame(df.al011998))
    expect_true(tibble::is_tibble(df.al011998))
    expect_identical(dim(df.al011998), as.integer(c(25, 125)))
})

## ---- Test fstadv() ----------------------------------------------------------
#' Test return of fstadv()
test_that("Test fstadv()", {
    expect_identical(al011998.fstadv, df.al011998.fstadv)
    expect_identical(al021998.fstadv, df.al021998.fstadv)
    expect_identical(ep011998.fstadv, df.ep011998.fstadv)
    expect_identical(ep021998.fstadv, df.ep021998.fstadv)
    expect_identical(ep081998.fstadv, df.ep081998.fstadv)
    expect_identical(ep031999.fstadv, df.ep031999.fstadv)
    expect_identical(ep041999.fstadv, df.ep041999.fstadv)
    expect_warning(ep041999.fstadv <- get_fstadv(ep1999[4]),
                   "Known data quality error")
    expect_identical(ep071999.fstadv, df.ep071999.fstadv)
    expect_identical(ep081999.fstadv, df.ep081999.fstadv)
    expect_identical(al022000.fstadv, df.al022000.fstadv)
    expect_identical(ep062000.fstadv, df.ep062000.fstadv)
    expect_identical(al132002.fstadv, df.al132002.fstadv)
    expect_identical(al302005.fstadv, df.al302005.fstadv)
})

