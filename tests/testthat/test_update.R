context("Update (update)")

## ---- Dataframe Skeleton -----------------------------------------------------
#' Test structure of dataframe skeleton
test_that("Dataframe Skeleton", {
    df <- create_df_update()
    expect_true(is.data.frame(df))
    expect_true(tibble::is_tibble(df))
    expect_identical(class(df$Status), "character")
    expect_identical(class(df$Name), "character")
    expect_identical(class(df$Adv), "character")
    expect_identical(class(df$Date), c("POSIXct", "POSIXt"))
    expect_identical(class(df$Contents), "character")
})

## ---- Test get_update() ------------------------------------------------------
#' Test return of get_update()
test_that("Test get_update()", {
    url <- "http://www.nhc.noaa.gov/archive/2008/IKE.shtml?"
    df <- get_update(link = url)
    expect_true(is.data.frame(df))
    expect_true(tibble::is_tibble(df))
    expect_identical(class(df$Status), "character")
    expect_identical(class(df$Name), "character")
    expect_identical(class(df$Adv), "character")
    expect_identical(class(df$Date), c("POSIXct", "POSIXt"))
    expect_identical(dim(df), as.integer(c(3, 5)))
    ## ---- * Hurricane Ike, AL, 1998, Sep 5 - update: 200 PM AST --------------
    expect_identical(df$Status[1], "Hurricane")
    expect_identical(df$Name[1], "Ike")
    expect_identical(df$Date[1], as.POSIXct("2008-09-05 18:00:00", tz = "UTC"))
})