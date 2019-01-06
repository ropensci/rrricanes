context("Filters")

#' Filters are to help extract product URL's from a storm's archive pages.
test_that("Get products from storm archive", {
  # Test number of products for Katrina, 2005
  expect_equal(length(AL122005),154)
  expect_equal(length(rrricanes:::filter_discus(AL122005)), 32)
  expect_equal(length(rrricanes:::filter_fstadv(AL122005)), 31)
  expect_equal(length(rrricanes:::filter_orig(AL122005)), 1)
  expect_equal(length(rrricanes:::filter_public(AL122005)), 61)
  expect_equal(length(rrricanes:::filter_prblty(AL122005)), 30)

  # Test number of products for Ike, 2008
  expect_equal(length(AL092008), 262)
  expect_equal(length(rrricanes:::filter_posest(AL092008)), 11)
  expect_equal(length(rrricanes:::filter_update(AL092008)), 3)
  expect_equal(length(rrricanes:::filter_wndprb(AL092008)), 52)
})
