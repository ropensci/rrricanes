context("Forecast/Advisory Products (fstadv)")

## ---- Dataframe Skeleton -----------------------------------------------------
#' Test structure of dataframe skeleton
test_that("Dataframe Skeleton", {
    df <- create_df_fstadv()
    expect_true(is.data.frame(df))
    expect_true(is_tibble(df))
    expect_identical(class(df$Status), "character")
    expect_identical(class(df$Name), "character")
    expect_identical(class(df$Adv), "character")
    expect_identical(class(df$Date), c("POSIXct", "POSIXt"))
    expect_identical(class(df$Key), "character")
    expect_identical(class(df$Lat), "numeric")
    expect_identical(class(df$Lon), "numeric")
    expect_identical(class(df$Wind), "numeric")
    expect_identical(class(df$Pressure), "numeric")
    expect_identical(class(df$PosAcc), "numeric")
    expect_identical(class(df$FwdDir), "numeric")
    expect_identical(class(df$FwdSpeed), "numeric")
    expect_identical(class(df$Eye), "numeric")
})

## ---- Test get_fstadv() ------------------------------------------------------
#' Test return of get_fstadv()
test_that("Test get_fstadv()", {
    url <- "http://www.nhc.noaa.gov/archive/1998/1998ALEXadv.html"
    df <- get_fstadv(link = url)
    expect_true(is.data.frame(df))
    expect_true(is_tibble(df))
    expect_identical(class(df$Status), "character")
    expect_identical(class(df$Name), "character")
    expect_identical(class(df$Adv), "character")
    expect_identical(class(df$Date), c("POSIXct", "POSIXt"))
    expect_identical(class(df$Key), "character")
    expect_identical(class(df$Lat), "numeric")
    expect_identical(class(df$Lon), "numeric")
    expect_identical(class(df$Wind), "numeric")
    expect_identical(class(df$Pressure), "numeric")
    expect_identical(class(df$PosAcc), "numeric")
    expect_identical(class(df$FwdDir), "numeric")
    expect_identical(class(df$FwdSpeed), "numeric")
    expect_identical(class(df$Eye), "numeric")
    expect_identical(dim(df), as.integer(c(25, 14)))
})

## ---- Test fstadv() ----------------------------------------------------------
#' Test return of fstadv()
test_that("Test fstadv()", {
    ## ---- * Advisory #1 ------------------------------------------------------
    url <- "http://www.nhc.noaa.gov/archive/1998/archive/mar/MAL0198.001"
    df <- fstadv(link = url)
    expect_identical(df$Status[1], "TROPICAL DEPRESSION")
    expect_identical(df$Name[1], "ONE")
    expect_identical(df$Adv[1], "1")
    expect_identical(df$Date[1], as.POSIXct("1998-07-27 15:00:00", tz = "UTC"))
    expect_identical(df$Key[1], "AL011998")
    expect_identical(df$Lat, 11.5)
    expect_identical(df$Lon, -27.0)
    expect_identical(df$Wind, 25)
    expect_identical(df$Pressure, 1008)
    expect_identical(df$PosAcc, 50)
    expect_identical(df$FwdDir, 280)
    expect_identical(df$FwdSpeed, 20)
    expect_identical(is.na(df$Eye), TRUE)
})