## ---- public -----------------------------------------------------------------
if (!exists("al_2008")){
  al_2008 <- get_storms(years = 2008, basins = "AL")
}
al_09_2008_public <- get_public(al_2008[[9,4]])
## ---- * get_public () --------------------------------------------------------
test_that("get_public", {
  expect_equal(al_09_2008_public, df.al_09_2008_public)
})

