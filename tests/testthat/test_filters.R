context("Filters")

#' Filters are to help extract product URL's from a storm's archive pages.

test_that("Get products from storm archive", {
  # Test number of products for Katrina, 2005
  expect_equal(
    length(
      p <- get_products(
        get_storms(2005, basin = "AL") %>%
          dplyr::filter(Name == "Hurricane Katrina") %>%
          dplyr::select(Link) %>%
          dplyr::first())),
    154)
  expect_equal(length(filter_discus(p)), 32)
  expect_equal(length(filter_fstadv(p)), 31)
  expect_equal(length(filter_orig(p)), 1)
  expect_equal(length(filter_public(p)), 61)
  expect_equal(length(filter_prblty(p)), 30)

  # Test number of products for Ike, 2008
  expect_equal(
    length(
      p <- get_products(
        get_storms(2008, basin = "AL") %>%
          dplyr::filter(Name == "Hurricane Ike") %>%
          dplyr::select(Link) %>%
          dplyr::first())),
    262)
  expect_equal(length(filter_posest(p)), 11)
  expect_equal(length(filter_update(p)), 3)
  expect_equal(length(filter_wndprb(p)), 52)
})
