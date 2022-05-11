## ---- posest -----------------------------------------------------------------
if (!exists("al_2008")){
  al_2008 <- get_storms(years = 2008, basins = "AL")
}
al_09_2008_posest <- get_posest(al_2008[[9,4]])
df.al_09_2008_posest <- rrricanes:::get_posest(al_2008[[9,4]])

## ---- * get_posest() ---------------------------------------------------------
test_that("Test get_posest()", {
  expect_identical(al_09_2008_posest, df.al_09_2008_posest)
})

