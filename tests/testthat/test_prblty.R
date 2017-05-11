context("Strike Probabilities (prblty)")

## ---- Data -------------------------------------------------------------------
## ---- * Current Data ---------------------------------------------------------
al1998 <- get_storms(year = 1998, basin = "AL") %>%
    dplyr::select(Link) %>%
    purrr::flatten_chr()

## ---- Test prblty() ----------------------------------------------------------
#' Test return of prblty()
test_that("Test prblty()", {
    ## ---- * Advisory #7 ------------------------------------------------------
    url <- "http://www.nhc.noaa.gov/archive/1998/archive/prb/LAL0198.007"
    df <- prblty(link = url)
    expect_true(is.data.frame(df))
    expect_true(is_tibble(df))
    expect_identical(class(df$Status), "character")
    expect_identical(class(df$Name), "character")
    expect_identical(class(df$Adv), "character")
    expect_identical(class(df$Date), c("POSIXct", "POSIXt"))
    expect_identical(df$Status[1], "Tropical Storm")
    expect_identical(df$Name[1], "Alex")
    expect_identical(df$Adv[1], "7")
    expect_identical(df$Date[1], as.POSIXct("1998-07-29 03:00:00", tz = "UTC"))
})