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
  skip_on_cran()
  expect_identical(al_09_2008_wndprb, df.al_09_2008_wndprb)
})

## get_storm_list ----
# 2019-06-08 (Trice) - As of today, this test fails but I can't pinpoint why.
# read_csv gives a warning that it expected 21 columns but at row 2583 it got
# two columns. Looking over the CSV file, I do not see this. The last ob
# returned, Nadine (AL152018) is completely NA. I attempted to break the
# import into two pieces, skipping Nadine and binding the datasets, but nothing
# is returned after Nadine even with the "skip" parameter. No "problems" are
# generated from that attempt.
# So, if this test fails then it could be something else entirely. HOWEVER,
# check the results of `storm_list$warnings`; if the text file is fixed as it
# was prior, that will be the failure point.
test_that("Get Storm List", {
  quietly_get_storm_list <- purrr::quietly(.f = rrricanes::get_storm_list)
  storm_list <- quietly_get_storm_list()
  expect_output(str(storm_list$result), "21 variables")
  expect_identical(
    names(storm_list$result),
    c("STORM_NAME", "RE", "X", "R2", "R3", "R4", "R5", "CY", "YYYY", "TY", "I",
      "YYY1MMDDHH", "YYY2MMDDHH", "SIZE", "GENESIS_NUM", "PAR1", "PAR2",
      "PRIORITY", "STORM_STATE", "WT_NUMBER", "STORMID"
    )
  )
  expect_identical(
    storm_list$warnings,
    "1 parsing failure.\n row col   expected    actual         file\n2583  -- 21 columns 2 columns literal data\n"
  )
})
