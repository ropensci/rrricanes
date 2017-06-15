context("Get Storm Data")

# Set timeout options
opt.timeout <- getOption("rrricanes.http_timeout")
opt.attempts <- getOption("rrricanes.http_attempts")
options("rrricanes.http_timeout" = 1)
options("rrricanes.http_attempts" = 5)

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
                     get_storm_data(products = "test"))
    expect_error(get_storm_data(),
                 "argument \"link\" is missing, with no default")
})

# Reset options
options("rrricanes.http_timeout" = opt.timeout)
options("rrricanes.http_attempts" = opt.attempts)
