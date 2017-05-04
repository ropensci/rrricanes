context("Test Public Advisory Scraping.")

# 1998, Tropical Storm Alex
al.1998.alex <- get_storms(1998, basin = "AL") %>%
  dplyr::filter(Name == "TROPICAL STORM ALEX")
al.1998.alex.products <- get_products(al.1998.alex %>%
                                        dplyr::select(Link) %>%
                                        dplyr::first())

test_that("Filter Public Advisories.", {
  expect_equal(length(filter_public_advisories(al.1998.alex.products)), 25)
})

al.1998.alex.products.public <- filter_public_advisories(al.1998.alex.products)

test_that("1998, Tropical Storm Alex, Advisory 1", {
  # Status
  expect_identical(public(al.1998.alex.products.public[1]) %>%
                     dplyr::select(Status) %>%
                     dplyr::first(), "TROPICAL DEPRESSION")
  # Name
  expect_identical(public(al.1998.alex.products.public[1]) %>%
                     dplyr::select(Name) %>%
                     dplyr::first(), "ONE")
  # Advisory number
  expect_equal(public(al.1998.alex.products.public[1]) %>%
                 dplyr::select(Adv) %>%
                 dplyr::first(), "1")
  # Date
  expect_equal(public(al.1998.alex.products.public[1]) %>%
                 dplyr::select(Date) %>%
                 dplyr::first(),
               as.POSIXlt("1998-07-27 11:00:00", tz = "AST"))
})

test_that("1998, Tropical Storm Alex, Advisory 26", {
  # Status
  expect_identical(public(al.1998.alex.products.public[25]) %>%
                     dplyr::select(Status) %>%
                     dplyr::first(), "TROPICAL DISTURBANCE")
  # Name
  expect_identical(public(al.1998.alex.products.public[25]) %>%
                     dplyr::select(Name) %>%
                     dplyr::first(), "ALEX")
  # Advisory number
  expect_equal(public(al.1998.alex.products.public[25]) %>%
                 dplyr::select(Adv) %>%
                 dplyr::first(), "26")
  # # Date
  # expect_equal(public(al.1998.alex.products.public[25]) %>%
  #                dplyr::select(Date) %>%
  #                dplyr::first(),
  #              as.POSIXct("1998-08-02 17:00:00", tz = "AST"))
})