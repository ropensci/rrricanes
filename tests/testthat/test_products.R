context("Test Product functions.")

## ---- twoal() ----------------------------------------------------------------
test_that("twoal()", {
    x <- twoal()
    expect_identical(is.list(x), TRUE)
})

## ---- twoep() ----------------------------------------------------------------
test_that("twoep()", {
    x <- twoep()
    expect_identical(is.list(x), TRUE)
})
