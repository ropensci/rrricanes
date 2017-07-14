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
