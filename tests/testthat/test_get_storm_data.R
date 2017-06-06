context("Get Storm Data")

## ---- Base Data --------------------------------------------------------------
## ---- * 2008, AL -------------------------------------------------------------
df.al_2017_storm_data <- get_storms(year = 2017, basin = "AL") %>%
    dplyr::slice(1) %>%
    .$Link %>%
    get_storm_data(products = c("discus", "fstadv"))
load(system.file("extdata", "al_2017_storm_data.Rda", package = "rrricanes"))

## ---- get_storm_data() -------------------------------------------------------
test_that("get_storm_data()", {
    ## ---- * 2017, AL, 01 -----------------------------------------------------
    expect_identical(al_2017_storm_data, df.al_2017_storm_data)
    ## ---- * Errors -----------------------------------------------------------
    expect_error(get_storms(year = 2017, basin = "AL") %>%
                     dplyr::slice(1) %>%
                     .$Link %>%
                     get_storm_data(products = "test"),
                 "'arg' should be one of “discus”, “fstadv”, “posest”, “public”, “prblty”, “update”, “wndprb”")
    expect_error(get_storm_data(), "argument \"link\" is missing, with no default")
})

## ---- load_storm_data() ------------------------------------------------------
test_that("load_storm_data()", {
    expect_error(load_storm_data(years = 1997:1999),
                 "years must be between 1998 and current year")
    expect_error(load_storm_data(years = 2017, basins = "CP"),
                 "basins must be one or both of AL, EP")
    expect_error(load_storm_data(years = 2017, products = "test"),
                 "products must either be NULL or have at least one valid product")
})