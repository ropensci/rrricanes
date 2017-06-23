context("Strike Probabilities (prblty)")

load(system.file("extdata", "al011998.prblty.Rda", package = "rrricanes"))

## ---- Test get_prblty() ------------------------------------------------------
test_that("Test get_prblty()", {
    skip_on_cran()
    al1998 <- get_storms(year = 1998, basin = "AL") %>% dplyr::select(Link)
    df.al011998.prblty <- al1998 %>% dplyr::slice(1) %>% .$Link %>% get_prblty()
    expect_identical(al011998.prblty, df.al011998.prblty)
})
