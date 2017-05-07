context("Wind Speed Probabilities (wndprb)")

## ---- Dataframe Skeleton -----------------------------------------------------
#' Test structure of dataframe skeleton
test_that("Dataframe Skeleton", {
    df <- create_df_wndprb()
    expect_true(is.data.frame(df))
    expect_true(is_tibble(df))
    expect_identical(class(df$Status), "character")
    expect_identical(class(df$Name), "character")
    expect_identical(class(df$Adv), "character")
    expect_identical(class(df$Date), c("POSIXct", "POSIXt"))
    expect_identical(class(df$Contents), "character")
})
