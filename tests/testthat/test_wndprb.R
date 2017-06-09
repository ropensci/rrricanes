context("Wind Speed Probabilities (wndprb)")

# Set timeout options
opt.timeout <- getOption("rrricanes.http_timeout")
opt.attempts <- getOption("rrricanes.http_attempts")
options("rrricanes.http_timeout" = 1)
options("rrricanes.http_attempts" = 5)

## ---- 2008, AL ---------------------------------------------------------------
al2008 <- get_storms(year = 2008, basin = "AL") %>% dplyr::select(Link)

## ---- Base Data --------------------------------------------------------------
## ---- * 2008, AL -------------------------------------------------------------
df.al092008.wndprb <- al2008 %>% dplyr::slice(9) %>% .$Link %>% get_wndprb()
load(system.file("extdata", "al092008.wndprb.Rda", package = "rrricanes"))

## ---- Test al_prblty_stations() ----------------------------------------------
test_that("Test al_prblty_stations()", {
    expect_identical(dim(al_prblty_stations()), c(155L, 3L))
    expect_identical(names(al_prblty_stations()),
                     c("Location", "Lat", "Lon"))
})

## ---- Test cp_prblty_stations() ----------------------------------------------
test_that("Test cp_prblty_stations()", {
    expect_identical(dim(cp_prblty_stations()), c(125L, 3L))
    expect_identical(names(cp_prblty_stations()),
                     c("Location", "Lat", "Lon"))
})

## ---- Test ep_prblty_stations() ----------------------------------------------
test_that("Test ep_prblty_stations()", {
    expect_identical(ep_prblty_stations(), FALSE)
})

## ---- Test get_wndprb() ------------------------------------------------------
test_that("Test get_wndprb()", {
    expect_identical(al092008.wndprb, df.al092008.wndprb)
})

# Reset options
options("rrricanes.http_timeout" = opt.timeout)
options("rrricanes.http_attempts" = opt.attempts)
