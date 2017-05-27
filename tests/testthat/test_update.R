context("Update (update)")

## ---- Base Data --------------------------------------------------------------
## ---- * 2008, AL -------------------------------------------------------------
df.al092008.update <- al2008 %>% dplyr::slice(9) %>% .$Link %>% get_update()
load(system.file("extdata", "al092008.update.Rda", package = "Hurricanes"))

## ---- Test get_update() ------------------------------------------------------
test_that("Test get_update()", {
    expect_identical(al092008.update, df.al092008.update)
})