context("Strike Probabilities (prblty)")

## ---- Base Data --------------------------------------------------------------
## ---- * 1998, AL -------------------------------------------------------------
df.al011998.prblty <- al1998 %>% dplyr::slice(1) %>% .$Link %>% get_prblty()
load(system.file("extdata", "al011998.prblty.Rda", package = "Hurricanes"))

## ---- Test get_prblty() ------------------------------------------------------
test_that("Test get_prblty()", {
    expect_identical(al011998.prblty, df.al011998.prblty)
})