context("Update (update)")

# Set timeout options
opt.timeout <- getOption("rrricanes.http_timeout")
opt.attempts <- getOption("rrricanes.http_attempts")
options("rrricanes.http_timeout" = 1)
options("rrricanes.http_attempts" = 5)

## ---- 2008, AL ---------------------------------------------------------------
al2008 <- get_storms(year = 2008, basin = "AL") %>% dplyr::select(Link)

## ---- Base Data --------------------------------------------------------------
## ---- * 2008, AL -------------------------------------------------------------
df.al092008.update <- al2008 %>% dplyr::slice(9) %>% .$Link %>% get_update()
load(system.file("extdata", "al092008.update.Rda", package = "rrricanes"))

## ---- Test get_update() ------------------------------------------------------
test_that("Test get_update()", {
    expect_identical(al092008.update, df.al092008.update)
})

# Reset options
options("rrricanes.http_timeout" = opt.timeout)
options("rrricanes.http_attempts" = opt.attempts)
