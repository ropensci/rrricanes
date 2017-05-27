context("Forecast/Advisory Products (fstadv)")

## ---- Base Data --------------------------------------------------------------
## ---- * 2008, AL -------------------------------------------------------------
df.al092008.fstadv <- al2008 %>% dplyr::slice(9) %>% .$Link %>% get_fstadv()
load(system.file("extdata", "al092008.fstadv.Rda", package = "Hurricanes"))

## ---- Test fstadv() ----------------------------------------------------------
#' Test return of fstadv()
test_that("Test fstadv()", {
    expect_identical(al092008.fstadv, df.al092008.fstadv)
})

