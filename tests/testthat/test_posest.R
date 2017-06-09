context("Position Estimates (posest)")

# Set timeout options
opt.timeout <- getOption("rrricanes.http_timeout")
opt.attempts <- getOption("rrricanes.http_attempts")
options("rrricanes.http_timeout" = 1)
options("rrricanes.http_attempts" = 5)

## ---- 2008, AL ---------------------------------------------------------------
al2008 <- get_storms(year = 2008, basin = "AL") %>% dplyr::select(Link)

## ---- Base Data --------------------------------------------------------------
## ---- * 2008, AL -------------------------------------------------------------
df.al092008.posest <- al2008 %>% dplyr::slice(9) %>% .$Link %>% get_posest()
load(system.file("extdata", "al092008.posest.Rda", package = "rrricanes"))

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
test_that("Test get_posest()", {
    expect_identical(al092008.posest, df.al092008.posest)
})

# Reset options
options("rrricanes.http_timeout" = opt.timeout)
options("rrricanes.http_attempts" = opt.attempts)
