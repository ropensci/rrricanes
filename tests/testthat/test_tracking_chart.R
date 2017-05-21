context("Test tracking chart")

## ---- tracking_chart() -------------------------------------------------------
test_that("tracking_chart Errors", {
    expect_error(tracking_chart(res = "test"),
                 "Chart resolution must be 110, 50, 10")
})
