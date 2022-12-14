## ---- prblty -----------------------------------------------------------------
if (!exists("al_1998")){
  al_1998 <- get_storms(years = 1998, basins = "AL")
}
df.al_01_1998_prblty <- rrricanes:::get_prblty(al_1998[[1,4]])
al_01_1998_prblty <- get_prblty(al_1998[[1,4]])
## ---- * get_prblty() ---------------------------------------------------------
test_that("get_prblty()", {
  expect_identical(al_01_1998_prblty, df.al_01_1998_prblty)
})

