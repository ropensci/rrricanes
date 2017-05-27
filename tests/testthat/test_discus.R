context("Storm Discussions (discus)")

## ---- Base Data --------------------------------------------------------------
## ---- * 2008, AL -------------------------------------------------------------
df.al092008.discus <- al2008 %>% dplyr::slice(9) %>% .$Link %>% get_discus()
load(system.file("extdata", "al092008.discus.Rda", package = "Hurricanes"))

## ---- Dataframe Skeleton -----------------------------------------------------
#' Test structure of dataframe skeleton
test_that("Dataframe Skeleton", {
    df <- create_df_discus()
    expect_true(is.data.frame(df))
    expect_true(tibble::is_tibble(df))
    expect_identical(class(df$Status), "character")
    expect_identical(class(df$Name), "character")
    expect_identical(class(df$Adv), "character")
    expect_identical(class(df$Date), c("POSIXct", "POSIXt"))
    expect_identical(class(df$Contents), "character")
})

## ---- Test get_discus() ------------------------------------------------------
#' Test return of get_discus()
test_that("Test get_discus()", {
    expect_identical(al092008.discus, df.al092008.discus)
})
