## ---- fstadv -------------------------------------------------------------
if (!exists("al_2008")){
  al_2008 <- get_storms(years = 2008, basins = "AL")
}
al_09_2008_fstadv <- get_fstadv(al_2008[[9,4]])
## ---- * tidy_fstadv ----------------------------------------------------------
test_that("Test tidy_fstadv()", {
  expect_warning(x <- rrricanes::tidy_fstadv(al_09_2008_fstadv))
  expect_identical(dim(x), c(53L, 18L))

  expect_identical(names(x), c("Key", "Adv", "Date", "Status", "Name", "Lat",
                               "Lon", "Wind", "Gust", "Pressure", "PosAcc",
                               "FwdDir", "FwdSpeed", "Eye", "SeasNE", "SeasSE",
                               "SeasSW", "SeasNW"))
})

## ---- * tidy_wr --------------------------------------------------------------
test_that("Test tidy_wr()", {
  x <- rrricanes::tidy_wr(al_09_2008_fstadv)
  expect_identical(dim(x), c(138L, 8L))
  expect_identical(names(x), c("Key", "Adv", "Date", "WindField", "NE",
                               "SE", "SW", "NW"))
})

## ---- * tidy_fcst ------------------------------------------------------------
test_that("Test tidy_fcst()", {
  x <- rrricanes::tidy_fcst(al_09_2008_fstadv)
  expect_identical(dim(x), c(336L, 8L))
  expect_identical(names(x),
                  c("StormKey", "Adv", "Date", "FcstDate", "Lat", "Lon",
                               "Wind", "Gust"))
})

## ---- * tidy_fcst_wr ---------------------------------------------------------
test_that("Test tidy_fcst_wr()", {
  x <- rrricanes::tidy_fcst_wr(al_09_2008_fstadv)
  expect_identical(dim(x), c(587L, 9L))
  expect_identical(names(x), c("StormKey", "Adv", "Date", "FcstDate", "WindField",
                               "NE", "SE", "SW", "NW"))
})

