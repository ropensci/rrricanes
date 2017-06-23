context("Public Advisory (public)")

load(system.file("extdata", "al092008.public.Rda", package = "rrricanes"))

## ---- Test get_public() ------------------------------------------------------
test_that("Test get_public()", {
    skip_on_cran()
    al2008 <- get_storms(year = 2008, basin = "AL") %>% dplyr::select(Link)
    df.al092008.public <- al2008 %>% dplyr::slice(9) %>% .$Link %>% get_public()
    testthat::expect_equal(al092008.public, df.al092008.public)
})
