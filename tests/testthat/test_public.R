context("Public Advisory (public)")

## ---- Base Data --------------------------------------------------------------
## ---- * 2008, AL -------------------------------------------------------------
df.al092008.public <- al2008 %>% dplyr::slice(9) %>% .$Link %>% get_public()
load(system.file("extdata", "al092008.public.Rda", package = "Hurricanes"))

## ---- Test get_public() ------------------------------------------------------
test_that("Test get_public()", {
    expect_equal(al092008.public, df.al092008.public)
})
