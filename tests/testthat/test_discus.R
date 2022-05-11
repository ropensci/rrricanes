## ---- discus -----------------------------------------------------------------
al_2008 <- get_storms(years = 2008, basins = "AL")
al_09_2008_discus <- get_discus(al_2008[[9,4]])

## ---- * get_discus -----------------------------------------------------------
#' Test return of get_discus()
test_that("Test get_discus()", {
  expect_identical(al_09_2008_discus, df.al_09_2008_discus)
})

