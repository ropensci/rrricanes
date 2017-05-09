context("Forecast/Advisory Products (fstadv)")

# Pre-load dataframes
url.al011998 <- "http://www.nhc.noaa.gov/archive/1998/1998ALEXadv.html"
df.al011998 <- get_fstadv(link = url.al011998)
url.al011998.01 <- "http://www.nhc.noaa.gov/archive/1998/archive/mar/MAL0198.001"
df.al011998.01 <- fstadv(link = url.al011998.01)
url.al011998.07 <- "http://www.nhc.noaa.gov/archive/1998/archive/mar/MAL0198.007"
df.al011998.07 <- fstadv(link = url.al011998.07)
url.al021998.08 <- "http://www.nhc.noaa.gov/archive/1998/archive/mar/MAL0298.008"
df.al021998.08 <- fstadv(link = url.al021998.08)
url.al021998.11 <- "http://www.nhc.noaa.gov/archive/1998/archive/mar/MAL0298.011"
df.al021998.11 <- fstadv(link = url.al021998.11)

## ---- Dataframe Skeleton -----------------------------------------------------
#' Test structure of dataframe skeleton
test_that("Dataframe Skeleton", {
    df <- create_df_fstadv()
    expect_true(is.data.frame(df))
    expect_true(tibble::is_tibble(df))
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
    expect_true(is.data.frame(df.al011998))
    expect_true(tibble::is_tibble(df.al011998))
    expect_identical(class(df.al011998$Status), "character")
    expect_identical(class(df.al011998$Name), "character")
    expect_identical(class(df.al011998$Adv), "character")
    expect_identical(class(df.al011998$Date), c("POSIXct", "POSIXt"))
    expect_identical(class(df.al011998$Key), "character")
    expect_identical(class(df.al011998$Lat), "numeric")
    expect_identical(class(df.al011998$Lon), "numeric")
    expect_identical(class(df.al011998$Wind), "numeric")
    expect_identical(class(df.al011998$Pressure), "numeric")
    expect_identical(class(df.al011998$PosAcc), "numeric")
    expect_identical(class(df.al011998$FwdDir), "numeric")
    expect_identical(class(df.al011998$FwdSpeed), "numeric")
    expect_identical(class(df.al011998$Eye), "numeric")
    expect_identical(class(df.al011998$NE34), "numeric")
    expect_identical(class(df.al011998$SE34), "numeric")
    expect_identical(class(df.al011998$SW34), "numeric")
    expect_identical(class(df.al011998$NW34), "numeric")
    expect_identical(class(df.al011998$NE50), "numeric")
    expect_identical(class(df.al011998$SE50), "numeric")
    expect_identical(class(df.al011998$SW50), "numeric")
    expect_identical(class(df.al011998$NW50), "numeric")
    expect_identical(class(df.al011998$NE64), "numeric")
    expect_identical(class(df.al011998$SE64), "numeric")
    expect_identical(class(df.al011998$SW64), "numeric")
    expect_identical(class(df.al011998$NW64), "numeric")
    expect_identical(class(df.al011998$SeasNE), "numeric")
    expect_identical(class(df.al011998$SeasSE), "numeric")
    expect_identical(class(df.al011998$SeasSW), "numeric")
    expect_identical(class(df.al011998$SeasNW), "numeric")
    expect_identical(dim(df.al011998), as.integer(c(25, 30)))
})

## ---- Test fstadv() ----------------------------------------------------------
#' Test return of fstadv()
test_that("Test fstadv()", {
    ## ---- * Status -----------------------------------------------------------
    expect_identical(df.al011998.01$Status[1], "Tropical Depression")
    ## ---- * Name -------------------------------------------------------------
    expect_identical(df.al011998.01$Name[1], "One")
    ## ---- * Advisory ---------------------------------------------------------
    expect_identical(df.al011998.01$Adv[1], "1")
    ## ---- * Date -------------------------------------------------------------
    expect_identical(df.al011998.01$Date[1], as.POSIXct("1998-07-27 15:00:00", tz = "UTC"))
    ## ---- * Key --------------------------------------------------------------
    expect_identical(df.al011998.01$Key[1], "AL011998")
    ## ---- * Lat --------------------------------------------------------------
    expect_identical(df.al011998.01$Lat, 11.5)
    ## ---- * Lon --------------------------------------------------------------
    expect_identical(df.al011998.01$Lon, -27.0)
    ## ---- * Wind -------------------------------------------------------------
    expect_identical(df.al011998.01$Wind, 25)
    ## ---- * Pressure ---------------------------------------------------------
    expect_identical(df.al011998.01$Pressure, 1008)
    ## ---- * PosAcc -----------------------------------------------------------
    expect_identical(df.al011998.01$PosAcc, 50)
    ## ---- * FwdDir -----------------------------------------------------------
    expect_identical(df.al011998.01$FwdDir, 280)
    ## ---- * FwdSpeed --------------------------------------------------------
    expect_identical(df.al011998.01$FwdSpeed, 20)
    ## ---- * Eye --------------------------------------------------------------
    expect_identical(is.na(df.al011998.01$Eye), TRUE)
    ## ---- * NE34 -------------------------------------------------------------
    expect_identical(df.al011998.01$NE34, as.numeric(NA))
    expect_identical(df.al011998.07$NE34, 100)
    expect_identical(df.al021998.08$NE34, 125)
    expect_identical(df.al021998.11$NE34, 150)
    ## ---- * SE34 -------------------------------------------------------------
    expect_identical(df.al011998.01$SE34, as.numeric(NA))
    expect_identical(df.al011998.07$SE34, 50)
    expect_identical(df.al021998.08$SE34, 25)
    expect_identical(df.al021998.11$SE34, 50)
    ## ---- * SW34 -------------------------------------------------------------
    expect_identical(df.al011998.01$SW34, as.numeric(NA))
    expect_identical(df.al011998.07$SW34, 50)
    expect_identical(df.al021998.08$SW34, 25)
    expect_identical(df.al021998.11$SW34, 50)
    ## ---- * NW34 -------------------------------------------------------------
    expect_identical(df.al011998.01$NW34, as.numeric(NA))
    expect_identical(df.al011998.07$NW34, 100)
    expect_identical(df.al021998.08$NW34, 125)
    expect_identical(df.al021998.11$NW34, 150)
    ## ---- * NE50 -------------------------------------------------------------
    expect_identical(df.al011998.01$NE50, as.numeric(NA))
    expect_identical(df.al021998.08$NE50, 30)
    expect_identical(df.al021998.11$NE50, 75)
    ## ---- * SE50 -------------------------------------------------------------
    expect_identical(df.al011998.01$SE50, as.numeric(NA))
    expect_identical(df.al021998.08$SE50, 0)
    expect_identical(df.al021998.11$SE50, 25)
    ## ---- * SW50 -------------------------------------------------------------
    expect_identical(df.al011998.01$SW50, as.numeric(NA))
    expect_identical(df.al021998.08$SW50, 0)
    expect_identical(df.al021998.11$SW50, 25)
    ## ---- * NW50 -------------------------------------------------------------
    expect_identical(df.al011998.01$NW50, as.numeric(NA))
    expect_identical(df.al021998.08$NW50, 30)
    expect_identical(df.al021998.11$NW50, 50)
    ## ---- * NE64 -------------------------------------------------------------
    expect_identical(df.al011998.01$NE64, as.numeric(NA))
    expect_identical(df.al021998.11$NE64, 30)
    ## ---- * SE64 -------------------------------------------------------------
    expect_identical(df.al011998.01$SE64, as.numeric(NA))
    expect_identical(df.al021998.11$SE64, 0)
    ## ---- * SW64 -------------------------------------------------------------
    expect_identical(df.al011998.01$SW64, as.numeric(NA))
    expect_identical(df.al021998.11$SW64, 0)
    ## ---- * NW64 -------------------------------------------------------------
    expect_identical(df.al011998.01$NW64, as.numeric(NA))
    expect_identical(df.al021998.11$NW64, 30)
    ## ---- * SeasNE -----------------------------------------------------------
    expect_identical(df.al011998.07$SeasNE, 100)
    ## ---- * SeasSE -----------------------------------------------------------
    expect_identical(df.al011998.07$SeasSE, 50)
    ## ---- * SeasSW -----------------------------------------------------------
    expect_identical(df.al011998.07$SeasSW, 50)
    ## ---- * SeasNW -----------------------------------------------------------
    expect_identical(df.al011998.07$SeasNW, 100)
})

