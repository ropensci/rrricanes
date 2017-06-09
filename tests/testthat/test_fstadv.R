context("Forecast/Advisory Products (fstadv)")

## ---- 2008, AL ---------------------------------------------------------------
al2008 <- get_storms(year = 2008, basin = "AL") %>% dplyr::select(Link)

## ---- Base Data --------------------------------------------------------------
## ---- * 2008, AL -------------------------------------------------------------
df.al092008.fstadv <- al2008 %>% dplyr::slice(9) %>% .$Link %>% get_fstadv()
load(system.file("extdata", "al092008.fstadv.Rda", package = "rrricanes"))

## ---- * 2016, AL -------------------------------------------------------------
load_storm_data(years = 2016, basins = "AL", products = "fstadv")

## ---- Test fstadv() ----------------------------------------------------------
#' Test return of fstadv()
test_that("Test fstadv()", {
    expect_identical(al092008.fstadv, df.al092008.fstadv)
})

## ---- Test tidy_fstadv() -----------------------------------------------------
test_that("Test tidy_fstadv()", {
    x <- tidy_fstadv(AL2016_fstadv)
    expect_identical(dim(x), c(427L, 18L))
    expect_identical(names(x),
                     c("Key", "Adv", "Date", "Status", "Name", "Lat", "Lon",
                       "Wind", "Gust", "Pressure", "PosAcc", "FwdDir",
                       "FwdSpeed", "Eye", "SeasNE", "SeasSE", "SeasSW",
                       "SeasNW"))
})

## ---- Test tidy_wr() -----------------------------------------------------
test_that("Test tidy_wr()", {
    x <- tidy_wr(AL2016_fstadv)
    expect_identical(dim(x), c(637L, 8L))
    expect_identical(names(x),
                     c("Key", "Adv", "Date", "WindField", "NE", "SE", "SW",
                       "NW"))
})

## ---- Test tidy_fcst() -----------------------------------------------------
test_that("Test tidy_fcst()", {
    x <- tidy_fcst(AL2016_fstadv)
    expect_identical(dim(x), c(2523L, 8L))
    expect_identical(names(x),
                     c("Key", "Adv", "Date", "FcstDate", "Lat", "Lon", "Wind",
                       "Gust"))
})

## ---- Test tidy_fcst_wr() -----------------------------------------------------
test_that("Test tidy_fcst_wr()", {
    x <- tidy_fcst_wr(AL2016_fstadv)
    expect_identical(dim(x), c(2958L, 9L))
    expect_identical(names(x),
                     c("Key", "Adv", "Date", "FcstDate", "WindField", "NE",
                       "SE", "SW", "NW"))
})
