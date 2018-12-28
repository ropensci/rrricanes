context("Filters")

#' Filters are to help extract product URL's from a storm's archive pages.

AL122005 <-
  "https://www.nhc.noaa.gov/archive/2005/KATRINA.shtml?" %>%
  rrricanes:::extract_storm_links(p = dplyr::progress_estimated(1)) %>%
  purrr::flatten_chr()

AL092008 <-
  "https://www.nhc.noaa.gov/archive/2008/IKE.shtml?" %>%
  rrricanes:::extract_storm_links(p = dplyr::progress_estimated(1)) %>%
  purrr::flatten_chr()

test_that("Get products from storm archive", {
  # Test number of products for Katrina, 2005
  expect_equal(length(AL122005),154)
  expect_equal(
    length(rrricanes:::filter_discus(AL122005) %>% purrr::flatten_chr()), 32
  )
  expect_equal(
    length(rrricanes:::filter_fstadv(AL122005) %>% purrr::flatten_chr()), 31
  )
  expect_equal(
    length(rrricanes:::filter_orig(AL122005) %>% purrr::flatten_chr()), 1
  )
  expect_equal(
    length(rrricanes:::filter_public(AL122005) %>% purrr::flatten_chr()), 61
  )
  expect_equal(
    length(rrricanes:::filter_prblty(AL122005) %>% purrr::flatten_chr()), 30
  )

  # Test number of products for Ike, 2008
  expect_equal(length(AL092008), 262)
  expect_equal(
    length(rrricanes:::filter_posest(AL092008) %>% purrr::flatten_chr()), 11
  )
  expect_equal(
    length(rrricanes:::filter_update(AL092008) %>% purrr::flatten_chr()), 3
  )
  expect_equal(
    length(rrricanes:::filter_wndprb(AL092008) %>% purrr::flatten_chr()), 52
  )
})
