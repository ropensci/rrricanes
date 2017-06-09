context("Get Storm Data")

## ---- Base Data --------------------------------------------------------------
## ---- * 2008, AL -------------------------------------------------------------
df.al_2017_storm_data <- get_storms(year = 2017, basin = "AL") %>%
    dplyr::slice(1) %>%
    .$Link %>%
    get_storm_data(products = c("discus", "fstadv"))
load(system.file("extdata", "al_2017_storm_data.Rda", package = "rrricanes"))

## ---- * 2016, AL -------------------------------------------------------------
load_storm_data(years = 2016, basins = "AL")

## ---- * 2016, AL, fstadv -----------------------------------------------------
load_storm_data(years = 2016, basins = "AL", products = "fstadv")

## ---- get_storm_data() -------------------------------------------------------
test_that("get_storm_data()", {
    ## ---- * 2017, AL, 01 -----------------------------------------------------
    expect_identical(al_2017_storm_data, df.al_2017_storm_data)
    ## ---- * Errors -----------------------------------------------------------
    expect_error(get_storms(year = 2017, basin = "AL") %>%
                     dplyr::slice(1) %>%
                     .$Link %>%
                     get_storm_data(products = "test"))
    expect_error(get_storm_data(),
                 "argument \"link\" is missing, with no default")
})

## ---- load_storm_data() ------------------------------------------------------
test_that("load_storm_data()", {
    expect_error(load_storm_data(years = 1997:1999),
                 "years must be between 1998 and current year")
    expect_error(load_storm_data(years = 2017, basins = "CP"),
                 "basins must be one or both of AL, EP")
    expect_error(load_storm_data(years = 2017, products = "test"),
                 paste0("products must either be NULL or have ",
                        "at least one valid product"))
    expect_identical(dim(AL2016),c(17L, 5L))
    expect_identical(names(AL2016),
                     c("Key", "Name", "Wind", "StartDate", "EndDate"))
    expect_identical(dim(AL2016_fstadv), c(427L, 149L))
    expect_identical(names(AL2016_fstadv),
                     c("Status", "Name", "Adv", "Date", "Key", "Lat", "Lon",
                       "Wind", "Gust", "Pressure", "PosAcc", "FwdDir",
                       "FwdSpeed", "Eye", "NE64", "SE64", "SW64", "NW64",
                       "NE50", "SE50", "SW50", "NW50", "NE34", "SE34", "SW34",
                       "NW34", "SeasNE", "SeasSE", "SeasSW", "SeasNW",
                       "Hr12FcstDate", "Hr12Lat", "Hr12Lon", "Hr12Wind",
                       "Hr12Gust", "Hr12NE64", "Hr12SE64", "Hr12SW64",
                       "Hr12NW64", "Hr12NE50", "Hr12SE50", "Hr12SW50",
                       "Hr12NW50", "Hr12NE34", "Hr12SE34", "Hr12SW34",
                       "Hr12NW34", "Hr24FcstDate", "Hr24Lat", "Hr24Lon",
                       "Hr24Wind", "Hr24Gust", "Hr24NE64", "Hr24SE64",
                       "Hr24SW64", "Hr24NW64", "Hr24NE50", "Hr24SE50",
                       "Hr24SW50", "Hr24NW50", "Hr24NE34", "Hr24SE34",
                       "Hr24SW34", "Hr24NW34", "Hr36FcstDate", "Hr36Lat",
                       "Hr36Lon", "Hr36Wind", "Hr36Gust", "Hr36NE64",
                       "Hr36SE64", "Hr36SW64", "Hr36NW64", "Hr36NE50",
                       "Hr36SE50", "Hr36SW50", "Hr36NW50", "Hr36NE34",
                       "Hr36SE34", "Hr36SW34", "Hr36NW34", "Hr48FcstDate",
                       "Hr48Lat", "Hr48Lon", "Hr48Wind", "Hr48Gust",
                       "Hr48NE64", "Hr48SE64", "Hr48SW64", "Hr48NW64",
                       "Hr48NE50", "Hr48SE50", "Hr48SW50", "Hr48NW50",
                       "Hr48NE34", "Hr48SE34", "Hr48SW34", "Hr48NW34",
                       "Hr72FcstDate", "Hr72Lat", "Hr72Lon", "Hr72Wind",
                       "Hr72Gust", "Hr72NE64", "Hr72SE64", "Hr72SW64",
                       "Hr72NW64", "Hr72NE50", "Hr72SE50", "Hr72SW50",
                       "Hr72NW50", "Hr72NE34", "Hr72SE34", "Hr72SW34",
                       "Hr72NW34", "Hr96FcstDate", "Hr96Lat", "Hr96Lon",
                       "Hr96Wind", "Hr96Gust", "Hr96NE64", "Hr96SE64",
                       "Hr96SW64", "Hr96NW64", "Hr96NE50", "Hr96SE50",
                       "Hr96SW50", "Hr96NW50", "Hr96NE34", "Hr96SE34",
                       "Hr96SW34", "Hr96NW34", "Hr120FcstDate", "Hr120Lat",
                       "Hr120Lon", "Hr120Wind", "Hr120Gust", "Hr120NE64",
                       "Hr120SE64", "Hr120SW64", "Hr120NW64", "Hr120NE50",
                       "Hr120SE50", "Hr120SW50", "Hr120NW50", "Hr120NE34",
                       "Hr120SE34", "Hr120SW34", "Hr120NW34"))
})