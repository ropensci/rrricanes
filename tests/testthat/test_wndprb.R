## ---- wndprb -----------------------------------------------------------------

## ---- * al_prblty_stations ---------------------------------------------------
test_that("al_prblty_stations", {
  expect_warning(x <- rrricanes::al_prblty_stations(),
                 "Expected 7 pieces. Additional pieces discarded in 1 rows [90].",
                 fixed = TRUE)
  expect_identical(dim(x), c(216L, 7L))
  expect_identical(names(x),
                   c("X1", "Location", "Lat", "Lon", "X5", "X6", "X7"))
})

## ---- * cp_prblty_stations ---------------------------------------------------
test_that("cp_prblty_stations", {
  expect_identical(dim(rrricanes::cp_prblty_stations()), c(168L, 7L))
  expect_identical(names(rrricanes::cp_prblty_stations()),
                   c("X1", "Location", "Lat", "Lon", "X5", "X6", "X7"))
})

## ---- * ep_prblty_stations ---------------------------------------------------
test_that("ep_prblty_stations", {
  expect_warning(x <- rrricanes::ep_prblty_stations(),
                 "Expected 7 pieces. Missing pieces filled with `NA` in 1 rows [41].",
                 fixed = TRUE)
  expect_identical(dim(x), c(168L, 7L))
  expect_identical(names(x),
                   c("X1", "Location", "Lat", "Lon", "X5", "X6", "X7"))
})

## ---- * get_wndprb -----------------------------------------------------------
test_that("Test get_wndprb()", {
  expect_identical(al_09_2008_wndprb, df.al_09_2008_wndprb)
})
