context("Strike Probabilities (prblty)")

## ---- 1998, AL ---------------------------------------------------------------
al1998 <- get_storms(year = 1998, basin = "AL") %>% dplyr::select(Link)
## ---- 2008, AL ---------------------------------------------------------------
al2008 <- get_storms(year = 2008, basin = "AL") %>% dplyr::select(Link)

## ---- Base Data --------------------------------------------------------------
## ---- * 1998, AL -------------------------------------------------------------
df.al011998.prblty <- al1998 %>% dplyr::slice(1) %>% .$Link %>% get_prblty()
load(system.file("extdata", "al011998.prblty.Rda", package = "rrricanes"))

## ---- Test get_prblty() ------------------------------------------------------
test_that("Test get_prblty()", {
    expect_identical(al011998.prblty, df.al011998.prblty)
})