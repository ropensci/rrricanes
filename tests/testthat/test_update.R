## ---- update -----------------------------------------------------------------
if (!exists("al_2008")){
  al_2008 <- get_storms(years = 2008, basins = "AL")
}
al_09_2008_update <- get_update(al_2008[[9,4]])
## ---- * get_update() ------------------------------------------------------
test_that("get_update", {
  expect_identical(al_09_2008_update, df.al_09_2008_update)
})

