context("Strike Probabilities (prblty)")

## ---- Data -------------------------------------------------------------------
## ---- * Current Data ---------------------------------------------------------
al1998 <- get_storms(year = 1998, basin = "AL") %>%
    dplyr::select(Link) %>%
    purrr::flatten_chr()

df.al011998.prblty <- get_prblty(al1998[1])

## ---- * Saved Data -----------------------------------------------------------
load(system.file("extdata", "al011998.prblty.Rda", package = "rrricanes"))

## ---- Test prblty ------------------------------------------------------------
#' Test return of prblty
test_that("Test prblty", {
    expect_identical(al011998.prblty, df.al011998.prblty)
})