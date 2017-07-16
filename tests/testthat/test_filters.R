context("Filters")

#' Filters are to help extract product URL's from a storm's archive pages.

test_that("Get products from storm archive", {
  # Test number of products for Katrina, 2005
  expect_equal(length(x <- extract_storm_links(
    "http://www.nhc.noaa.gov/archive/2005/KATRINA.shtml?",
    p = dplyr::progress_estimated(1)) %>%
      purrr::flatten_chr()),
    154)
  expect_equal(length(filter_discus(x) %>% purrr::flatten_chr()), 32)
  expect_equal(length(filter_fstadv(x) %>% purrr::flatten_chr()), 31)
  expect_equal(length(filter_orig(x) %>% purrr::flatten_chr()), 1)
  expect_equal(length(filter_public(x) %>% purrr::flatten_chr()), 61)
  expect_equal(length(filter_prblty(x) %>% purrr::flatten_chr()), 30)

  # Test number of products for Ike, 2008
  expect_equal(length(x <- extract_storm_links(
    "http://www.nhc.noaa.gov/archive/2008/IKE.shtml?",
    p = dplyr::progress_estimated(1)) %>%
      purrr::flatten_chr()),
    262)
  expect_equal(length(filter_posest(x) %>% purrr::flatten_chr()), 11)
  expect_equal(length(filter_update(x) %>% purrr::flatten_chr()), 3)
  expect_equal(length(filter_wndprb(x) %>% purrr::flatten_chr()), 52)
})
