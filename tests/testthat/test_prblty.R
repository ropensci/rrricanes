context("Strike Probabilities (prblty)")

## ---- Data -------------------------------------------------------------------
## ---- * Current Data ---------------------------------------------------------
al1998 <- get_storms(year = 1998, basin = "AL") %>%
    dplyr::select(Link) %>%
    purrr::flatten_chr()

#' Get storms for 2000, AL basin
al2000 <- get_storms(year = 2000, basin = "AL") %>%
    dplyr::select(Link) %>%
    purrr::flatten_chr()

df.al011998.prblty <- get_prblty(al1998[1])
df.al022000.prblty <- get_prblty(al2000[2])
df.al032000.prblty <- get_prblty(al2000[3])

## ---- * Saved Data -----------------------------------------------------------
load(system.file("extdata", "al011998.prblty.Rda", package = "Hurricanes"))
load(system.file("extdata", "al022000.prblty.Rda", package = "Hurricanes"))
load(system.file("extdata", "al032000.prblty.Rda", package = "Hurricanes"))

## ---- Test prblty ------------------------------------------------------------
#' Test return of prblty
test_that("Test prblty", {
    expect_identical(al011998.prblty, df.al011998.prblty)
    expect_identical(al022000.prblty, df.al022000.prblty)
    expect_identical(al032000.prblty, df.al032000.prblty)
})