context("Storm Discussions (discus)")

## ---- Dataframe Skeleton -----------------------------------------------------
#' Test structure of dataframe skeleton
test_that("Dataframe Skeleton", {
    df <- create_df_discus()
    expect_true(is.data.frame(df))
    expect_true(is_tibble(df))
    expect_identical(class(df$Status), "character")
    expect_identical(class(df$Name), "character")
    expect_identical(class(df$Adv), "character")
    expect_identical(class(df$Date), c("POSIXct", "POSIXt"))
    expect_identical(class(df$Contents), "character")
})

## ---- Test discus() ----------------------------------------------------------
#' Test return of discus()
test_that("Test discus()", {
    ## ---- * Advisory #1 ------------------------------------------------------
    url <- "http://www.nhc.noaa.gov/archive/1998/archive/dis/NAL0198.001"
    df <- discus(link = url)
    expect_true(is.data.frame(df))
    expect_true(is_tibble(df))
    expect_identical(class(df$Status), "character")
    expect_identical(class(df$Name), "character")
    expect_identical(class(df$Adv), "character")
    expect_identical(class(df$Date), c("POSIXct", "POSIXt"))
    expect_identical(df$Status[1], "TROPICAL DEPRESSION")
    expect_identical(df$Name[1], "ONE")
    expect_identical(df$Adv[1], "1")
    expect_identical(df$Date[1], as.POSIXct("1998-07-27 15:00:00", tz = "UTC"))
    ## ---- * Advisory #7 ------------------------------------------------------
    url <- "http://www.nhc.noaa.gov/archive/1998/archive/dis/NAL0198.007"
    df <- discus(link = url)
    expect_true(is.data.frame(df))
    expect_true(is_tibble(df))
    expect_identical(class(df$Status), "character")
    expect_identical(class(df$Name), "character")
    expect_identical(class(df$Adv), "character")
    expect_identical(class(df$Date), c("POSIXct", "POSIXt"))
    expect_identical(df$Status[1], "TROPICAL STORM")
    expect_identical(df$Name[1], "ALEX")
    expect_identical(df$Adv[1], "7")
    expect_identical(df$Date[1], as.POSIXct("1998-07-29 03:00:00", tz = "UTC"))
    ## ---- * Advisory #25 -----------------------------------------------------
    #' This advisory header slightly breaks format:
    #' ZCZC MIATCDAT1 ALL
    #' TTAA00 KNHC DDHHMM COR
    #' ...CORRECTED DISCUSSION NUMBER...             # This piece is new
    #' TROPICAL STORM ALEX DISCUSSION NUMBER  25
    #' NATIONAL WEATHER SERVICE MIAMI FL
    #' 11 AM EDT SUN AUG 02 1998
    url <- "http://www.nhc.noaa.gov/archive/1998/archive/dis/NAL0198.024"
    df <- discus(link = url)
    expect_true(is.data.frame(df))
    expect_true(is_tibble(df))
    expect_identical(class(df$Status), "character")
    expect_identical(class(df$Name), "character")
    expect_identical(class(df$Adv), "character")
    expect_identical(class(df$Date), c("POSIXct", "POSIXt"))
    expect_identical(df$Status[1], "TROPICAL STORM")
    expect_identical(df$Name[1], "ALEX")
    expect_identical(df$Adv[1], "25")
    expect_identical(df$Date[1], as.POSIXct("1998-08-02 15:00:00", tz = "UTC"))
})

## ---- Test get_discus() ------------------------------------------------------
#' Test return of get_discus()
test_that("Test get_discus()", {
    url <- "http://www.nhc.noaa.gov/archive/1998/1998ALEXadv.html"
    df <- get_discus(link = url)
    expect_true(is.data.frame(df))
    expect_true(is_tibble(df))
    expect_identical(class(df$Status), "character")
    expect_identical(class(df$Name), "character")
    expect_identical(class(df$Adv), "character")
    expect_identical(class(df$Date), c("POSIXct", "POSIXt"))
    expect_identical(dim(df), as.integer(c(25, 5)))
})
