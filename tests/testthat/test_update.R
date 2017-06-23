context("Update (update)")

load(system.file("extdata", "al092008.update.Rda", package = "rrricanes"))

## ---- Test get_update() ------------------------------------------------------
test_that("Test get_update()", {
    skip_on_cran()
    al2008 <- get_storms(year = 2008, basin = "AL") %>% dplyr::select(Link)
    df.al092008.update <- al2008 %>% dplyr::slice(9) %>% .$Link %>% get_update()
    expect_identical(al092008.update, df.al092008.update)
})
