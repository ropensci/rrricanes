context("Position Estimates (posest)")

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
    skip_on_cran()
    al2008 <- get_storms(year = 2008, basin = "AL") %>% dplyr::select(Link)
    df.al092008.posest <- al2008 %>% dplyr::slice(9) %>% .$Link %>% get_posest()
    expect_identical(al092008.posest, df.al092008.posest)
})
