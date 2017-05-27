context("Get Storm Data")

al.charley.1998.url <- "http://www.nhc.noaa.gov/archive/1998/1998CHARLEYadv.html"

test_that("Error handling.", {
    message("Temporarily disable get_storm_data() tests.")
    # expect_error(get_storm_data(link = al.charley.1998.url))
})