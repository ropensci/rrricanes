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
                 "Invalid products included.")
    expect_error(get_storm_data(), "No link provided")
})