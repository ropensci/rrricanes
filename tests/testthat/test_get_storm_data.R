context("Test getting storm data.")

al.charley.1998.url <- "http://www.nhc.noaa.gov/archive/1998/1998CHARLEYadv.html"

test_that("Error handling.", {
  expect_error(get_storm_data(link = al.charley.1998.url))
})