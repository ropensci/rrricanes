context("Wind Speed Probabilities (wndprb)")

## ---- Data -------------------------------------------------------------------
## ---- * Current Data ---------------------------------------------------------
al2006 <- get_storms(year = 2006, basin = "AL") %>%
    dplyr::select(Link) %>%
    purrr::flatten_chr()

df.al012006.wndprb <- get_wndprb(al2006[1])
df.al032006.wndprb <- get_wndprb(al2006[3])
df.al042006.wndprb <- get_wndprb(al2006[4])

## ---- * Saved Data -----------------------------------------------------------
load(system.file("extdata", "al012006.wndprb.Rda", package = "Hurricanes"))
load(system.file("extdata", "al032006.wndprb.Rda", package = "Hurricanes"))
load(system.file("extdata", "al042006.wndprb.Rda", package = "Hurricanes"))

## ---- Test wndprb ------------------------------------------------------------
#' Test return of wndprb
test_that("Test wndprb", {
    expect_identical(al012006.wndprb, df.al012006.wndprb)
    expect_identical(al032006.wndprb, df.al032006.wndprb)
    expect_identical(al042006.wndprb, df.al042006.wndprb)
})