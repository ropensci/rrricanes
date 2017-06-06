context("Forecast/Advisory Products (fstadv)")

## ---- 2008, AL ---------------------------------------------------------------
al2008 <- get_storms(year = 2008, basin = "AL") %>% dplyr::select(Link)

## ---- Base Data --------------------------------------------------------------
## ---- * 2008, AL -------------------------------------------------------------
df.al092008.fstadv <- al2008 %>% dplyr::slice(9) %>% .$Link %>% get_fstadv()
load(system.file("extdata", "al092008.fstadv.Rda", package = "rrricanes"))

## ---- Test fstadv() ----------------------------------------------------------
#' Test return of fstadv()
test_that("Test fstadv()", {
    expect_identical(al092008.fstadv, df.al092008.fstadv)
})

