context("Wind Speed Probabilities (wndprb)")

## ---- Data -------------------------------------------------------------------
## ---- * Current Data ---------------------------------------------------------
al2006 <- get_storms(year = 2006, basin = "AL") %>% dplyr::select(Link) %>% purrr::flatten_chr()

df.al012006.wndprb <- get_wndprb(al2006[1])

## ---- * Saved Data -----------------------------------------------------------
load(system.file("extdata", "al012006.wndprb.Rda", package = "rrricanes"))

## ---- Test wndprb ------------------------------------------------------------
#' Test return of wndprb
test_that("Test wndprb", {
    expect_identical(al012006.wndprb, df.al012006.wndprb)
})