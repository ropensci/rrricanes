context("Wind Speed Probabilities (wndprb)")

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
    skip_on_cran()
    al2008 <- get_storms(year = 2008, basin = "AL") %>% dplyr::select(Link)
    df.al092008.wndprb <- al2008 %>% dplyr::slice(9) %>% .$Link %>% get_wndprb()
    expect_identical(al092008.wndprb, df.al092008.wndprb)
})
