context("Forecast/Advisory Products (fstadv)")

## ---- Data -------------------------------------------------------------------

## ---- * Current Data ---------------------------------------------------------
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

#' Get all storms for 1998, AL basin
al1998 <- get_storms(year = 1998, basin = "AL") %>%
    dplyr::select(Link) %>%
    purrr::flatten_chr()

#' Get all storms for 1998, EP basin
ep1998 <- get_storms(year = 1998, basin = "EP") %>%
    dplyr::select(Link) %>%
    purrr::flatten_chr()

#' Get all storms for 1999, EP basin
ep1999 <- get_storms(year = 1999, basin = "EP") %>%
    dplyr::select(Link) %>%
    purrr::flatten_chr()

#' Load Tropical Storm Alex
df.al011998.fstadv <- get_fstadv(al1998[1])
#' Load Hurricane Bonnie
df.al021998.fstadv <- get_fstadv(al1998[2])
#' Load Tropical Storm Agatha
df.ep011998.fstadv <- get_fstadv(ep1998[1])
#' Load Tropical Depression Two-E
df.ep021998.fstadv <- get_fstadv(ep1998[2])
#' Load Tropical Depression Three-E
df.ep031999.fstadv <- get_fstadv(ep1999[3])
#' Load Tropical Depression Four-E
df.ep041999.fstadv <- get_fstadv(ep1999[4])

## ---- * Saved Data -----------------------------------------------------------
load(system.file("extdata", "al011998.fstadv.Rda", package = "Hurricanes"))
load(system.file("extdata", "al021998.fstadv.Rda", package = "Hurricanes"))
load(system.file("extdata", "ep011998.fstadv.Rda", package = "Hurricanes"))
load(system.file("extdata", "ep021998.fstadv.Rda", package = "Hurricanes"))
load(system.file("extdata", "ep031999.fstadv.Rda", package = "Hurricanes"))
load(system.file("extdata", "ep041999.fstadv.Rda", package = "Hurricanes"))

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
    expect_identical(class(df$NE34), "numeric")
    expect_identical(class(df$SE34), "numeric")
    expect_identical(class(df$SW34), "numeric")
    expect_identical(class(df$NW34), "numeric")
    expect_identical(class(df$NE50), "numeric")
    expect_identical(class(df$SE50), "numeric")
    expect_identical(class(df$SW50), "numeric")
    expect_identical(class(df$NW50), "numeric")
    expect_identical(class(df$NE64), "numeric")
    expect_identical(class(df$SE64), "numeric")
    expect_identical(class(df$SW64), "numeric")
    expect_identical(class(df$NW64), "numeric")
    expect_identical(class(df$SeasNE), "numeric")
    expect_identical(class(df$SeasSE), "numeric")
    expect_identical(class(df$SeasSW), "numeric")
    expect_identical(class(df$SeasNW), "numeric")
    expect_identical(class(df$Hr12FcstDate), c("POSIXct", "POSIXt"))
    expect_identical(class(df$Hr12Lat), "numeric")
    expect_identical(class(df$Hr12Lon), "numeric")
    expect_identical(class(df$Hr12Wind), "numeric")
    expect_identical(class(df$Hr12Gust), "numeric")
    expect_identical(class(df$Hr12NE34), "numeric")
    expect_identical(class(df$Hr12SE34), "numeric")
    expect_identical(class(df$Hr12SW34), "numeric")
    expect_identical(class(df$Hr12NW34), "numeric")
    expect_identical(class(df$Hr12NE50), "numeric")
    expect_identical(class(df$Hr12SE50), "numeric")
    expect_identical(class(df$Hr12SW50), "numeric")
    expect_identical(class(df$Hr12NW50), "numeric")
    expect_identical(class(df$Hr12NE64), "numeric")
    expect_identical(class(df$Hr12SE64), "numeric")
    expect_identical(class(df$Hr12SW64), "numeric")
    expect_identical(class(df$Hr12NW64), "numeric")
    expect_identical(class(df$Hr24FcstDate), c("POSIXct", "POSIXt"))
    expect_identical(class(df$Hr24Lat), "numeric")
    expect_identical(class(df$Hr24Lon), "numeric")
    expect_identical(class(df$Hr24Wind), "numeric")
    expect_identical(class(df$Hr24Gust), "numeric")
    expect_identical(class(df$Hr24NE34), "numeric")
    expect_identical(class(df$Hr24SE34), "numeric")
    expect_identical(class(df$Hr24SW34), "numeric")
    expect_identical(class(df$Hr24NW34), "numeric")
    expect_identical(class(df$Hr24NE50), "numeric")
    expect_identical(class(df$Hr24SE50), "numeric")
    expect_identical(class(df$Hr24SW50), "numeric")
    expect_identical(class(df$Hr24NW50), "numeric")
    expect_identical(class(df$Hr24NE64), "numeric")
    expect_identical(class(df$Hr24SE64), "numeric")
    expect_identical(class(df$Hr24SW64), "numeric")
    expect_identical(class(df$Hr24NW64), "numeric")
    expect_identical(class(df$Hr36FcstDate), c("POSIXct", "POSIXt"))
    expect_identical(class(df$Hr36Lat), "numeric")
    expect_identical(class(df$Hr36Lon), "numeric")
    expect_identical(class(df$Hr36Wind), "numeric")
    expect_identical(class(df$Hr36Gust), "numeric")
    expect_identical(class(df$Hr36NE34), "numeric")
    expect_identical(class(df$Hr36SE34), "numeric")
    expect_identical(class(df$Hr36SW34), "numeric")
    expect_identical(class(df$Hr36NW34), "numeric")
    expect_identical(class(df$Hr36NE50), "numeric")
    expect_identical(class(df$Hr36SE50), "numeric")
    expect_identical(class(df$Hr36SW50), "numeric")
    expect_identical(class(df$Hr36NW50), "numeric")
    expect_identical(class(df$Hr36NE64), "numeric")
    expect_identical(class(df$Hr36SE64), "numeric")
    expect_identical(class(df$Hr36SW64), "numeric")
    expect_identical(class(df$Hr36NW64), "numeric")
    expect_identical(class(df$Hr48FcstDate), c("POSIXct", "POSIXt"))
    expect_identical(class(df$Hr48Lat), "numeric")
    expect_identical(class(df$Hr48Lon), "numeric")
    expect_identical(class(df$Hr48Wind), "numeric")
    expect_identical(class(df$Hr48Gust), "numeric")
    expect_identical(class(df$Hr48NE34), "numeric")
    expect_identical(class(df$Hr48SE34), "numeric")
    expect_identical(class(df$Hr48SW34), "numeric")
    expect_identical(class(df$Hr48NW34), "numeric")
    expect_identical(class(df$Hr48NE50), "numeric")
    expect_identical(class(df$Hr48SE50), "numeric")
    expect_identical(class(df$Hr48SW50), "numeric")
    expect_identical(class(df$Hr48NW50), "numeric")
    expect_identical(class(df$Hr48NE64), "numeric")
    expect_identical(class(df$Hr48SE64), "numeric")
    expect_identical(class(df$Hr48SW64), "numeric")
    expect_identical(class(df$Hr48NW64), "numeric")
    expect_identical(class(df$Hr72FcstDate), c("POSIXct", "POSIXt"))
    expect_identical(class(df$Hr72Lat), "numeric")
    expect_identical(class(df$Hr72Lon), "numeric")
    expect_identical(class(df$Hr72Wind), "numeric")
    expect_identical(class(df$Hr72Gust), "numeric")
    expect_identical(class(df$Hr72NE34), "numeric")
    expect_identical(class(df$Hr72SE34), "numeric")
    expect_identical(class(df$Hr72SW34), "numeric")
    expect_identical(class(df$Hr72NW34), "numeric")
    expect_identical(class(df$Hr72NE50), "numeric")
    expect_identical(class(df$Hr72SE50), "numeric")
    expect_identical(class(df$Hr72SW50), "numeric")
    expect_identical(class(df$Hr72NW50), "numeric")
    expect_identical(class(df$Hr72NE64), "numeric")
    expect_identical(class(df$Hr72SE64), "numeric")
    expect_identical(class(df$Hr72SW64), "numeric")
    expect_identical(class(df$Hr72NW64), "numeric")
    expect_identical(class(df$Hr96FcstDate), c("POSIXct", "POSIXt"))
    expect_identical(class(df$Hr96Lat), "numeric")
    expect_identical(class(df$Hr96Lon), "numeric")
    expect_identical(class(df$Hr96Wind), "numeric")
    expect_identical(class(df$Hr96Gust), "numeric")
    expect_identical(class(df$Hr120FcstDate), c("POSIXct", "POSIXt"))
    expect_identical(class(df$Hr120Lat), "numeric")
    expect_identical(class(df$Hr120Lon), "numeric")
    expect_identical(class(df$Hr120Wind), "numeric")
    expect_identical(class(df$Hr120Gust), "numeric")
})

## ---- Test get_fstadv() ------------------------------------------------------
#' Test return of get_fstadv()
test_that("Test get_fstadv()", {
    expect_true(is.data.frame(df.al011998))
    expect_true(tibble::is_tibble(df.al011998))
    expect_identical(dim(df.al011998), as.integer(c(25, 125)))
})

## ---- Test fstadv() ----------------------------------------------------------
#' Test return of fstadv()
test_that("Test fstadv()", {
    expect_identical(identical(al011998.fstadv, df.al011998.fstadv), TRUE)
    expect_identical(identical(al021998.fstadv, df.al021998.fstadv), TRUE)
    expect_identical(identical(ep011998.fstadv, df.ep011998.fstadv), TRUE)
    expect_identical(identical(ep021998.fstadv, df.ep021998.fstadv), TRUE)
    expect_identical(identical(ep031999.fstadv, df.ep031999.fstadv), TRUE)
    expect_identical(identical(ep041999.fstadv, df.ep041999.fstadv), TRUE)
    expect_warning(ep041999.fstadv <- get_fstadv(ep1999[4]),
                   "Known data quality error")
})

