context("Position Estimates (posest)")

## ---- Dataframe Skeleton -----------------------------------------------------
#' Test structure of dataframe skeleton
test_that("Dataframe Skeleton", {
    df <- create_df_posest()
    expect_true(is.data.frame(df))
    expect_true(tibble::is_tibble(df))
    expect_identical(class(df$Status), "character")
    expect_identical(class(df$Name), "character")
    expect_identical(class(df$Adv), "character")
    expect_identical(class(df$Date), c("POSIXct", "POSIXt"))
    expect_identical(class(df$Contents), "character")
})

## ---- Test get_posest() ------------------------------------------------------
#' Test return of get_posest()
test_that("Test get_posest()", {
    url <- "http://www.nhc.noaa.gov/archive/2008/IKE.shtml?"
    df <- get_posest(link = url)
    expect_true(is.data.frame(df))
    expect_true(tibble::is_tibble(df))
    expect_identical(class(df$Status), "character")
    expect_identical(class(df$Name), "character")
    expect_identical(class(df$Adv), "character")
    expect_identical(class(df$Date), c("POSIXct", "POSIXt"))
    expect_identical(dim(df), as.integer(c(11, 5)))
    ## ---- * Hurricane Ike, AL, 1998, position: 500 PM CDT --------------------
    expect_identical(df$Status[1], "Hurricane")
    expect_identical(df$Name[1], "Ike")
    expect_identical(df$Date[1], as.POSIXct("2008-09-12 22:00:00", tz = "UTC"))
})
