context("Wind Speed Probabilities (wndprb)")

## ---- Base Data --------------------------------------------------------------
## ---- * 2008, AL -------------------------------------------------------------
df.al092008.wndprb <- al2008 %>% dplyr::slice(9) %>% .$Link %>% get_wndprb()
load(system.file("extdata", "al092008.wndprb.Rda", package = "rrricanes"))
## ---- AL probability stations ------------------------------------------------
load(system.file("extdata", "al_prblty_stations.Rda", package = "rrricanes"))
## ---- CP probability stations ------------------------------------------------
load(system.file("extdata", "cp_prblty_stations.Rda", package = "rrricanes"))

## ---- Test get_wndprb() ------------------------------------------------------
test_that("Test get_wndprb()", {
    expect_identical(al092008.wndprb, df.al092008.wndprb)
})

## ---- Test al_prblty_stations() ----------------------------------------------
test_that("Test al_prblty_stations()", {
    expect_identical(al_prblty_stations, al_prblty_stations())
})

## ---- Test ep_prblty_stations() ----------------------------------------------
test_that("Test ep_prblty_stations()", {
    expect_identical(ep_prblty_stations(), FALSE)
})

## ---- Test cp_prblty_stations() ----------------------------------------------
test_that("Test cp_prblty_stations()", {
    expect_identical(cp_prblty_stations(), cp_prblty_stations)
})
