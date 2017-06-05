context("Wind Speed Probabilities (wndprb)")

## ---- Base Data --------------------------------------------------------------
## ---- * 2008, AL -------------------------------------------------------------
df.al092008.wndprb <- al2008 %>% dplyr::slice(9) %>% .$Link %>% get_wndprb()
load(system.file("extdata", "al092008.wndprb.Rda", package = "rrricanes"))

## ---- Test get_wndprb() ------------------------------------------------------
test_that("Test get_wndprb()", {
    expect_identical(al092008.wndprb, df.al092008.wndprb)
})
