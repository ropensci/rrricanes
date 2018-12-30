context("Scrapers")

## ---- Data -------------------------------------------------------------------
links <- c("archive/1998/archive/mar/MAL0198.001",
           "archive/1998/archive/mar/MAL0198.007",
           "archive/1998/archive/mar/MAL0198.008",
           "archive/1998/archive/mar/MAL0198.024",
           "archive/1998/archive/mar/MAL0198.026",
           "archive/1998/archive/mar/MAL0298.011",
           "archive/1999/mar/MAL1599.001.html",
           "archive/2017/al01/al012017.fstadv.009.shtml?",
           "archive/1998/archive/mar/MEP0198.001")
links <- sprintf("%s%s", get_nhc_link(), links)
contents <- purrr::map(links, get_url_contents) %>%
  purrr::flatten() %>%
  purrr::map(~xml2::read_html(.$content)) %>%
  purrr::map(.f = function(x) {
    if (is.na(txt <- rvest::html_node(x, xpath = "//pre") %>%
              rvest::html_text()))
      txt <- rvest::html_text(x)
    return(txt)
  }) %>%
  purrr::flatten_chr()

## ---- Status -----------------------------------------------------------------
test_that("Status", {
  ## ---- * Tropical Storm Alex, Forecast/Advisory 1
  # This advisory uses the term "Tropical Depression"
  expect_identical(scrape_status(contents[1]), "Tropical Depression")

  ## ---- * Tropical Storm Alex, Forecast/Advisory 7
  # This advisory has "COR" appended on the second line
  # This advisory uses the term "Tropical Storm"
  expect_identical(scrape_status(contents[2]), "Tropical Storm")

  ## ---- * Tropical Storm Alex, Forecast/Advisory 8
  expect_identical(scrape_status(contents[3]), "Tropical Storm")

  ## ---- * Tropical Storm Alex, Forecast/Advisory 25
  # This advisory has an abnormal line, "...CORRECTED ADVISORY NUMBER..."
  expect_identical(scrape_status(contents[4]), "Tropical Storm")

  ## ---- * Tropical Storm Alex, Forecast/Advisory 26
  # This advisory uses the term "Tropical Disturbance"
  expect_identical(scrape_status(contents[5]), "Tropical Disturbance")

  ## ---- * Hurricane Bonnie, Forecast/Advisory 11
  # This advisory uses the term "Hurricane"
  expect_identical(scrape_status(contents[6]), "Hurricane")

  ## ---- * Tropical Storm Katrina, Forecast/Advisory 1
  expect_identical(scrape_status(contents[7]), "Tropical Depression")

  ## ---- * Tropical Storm Arlene, Forecast/Advisory 9
  expect_identical(scrape_status(contents[8]), "Post-Tropical Cyclone")
})

## ---- Name -------------------------------------------------------------------
test_that("Name", {
  ## ---- * Tropical Storm Alex, Forecast/Advisory 1
  expect_identical(scrape_name(contents[1]),"One")
  ## ---- * Tropical Storm Agatha, Forecast/Advisory 1
  expect_identical(scrape_name(contents[9]), "One-E")
  ## ---- * Tropical Storm Katrina, Forecast/Advisory 1
  expect_identical(scrape_name(contents[7]), "Fifteen")
})

## ---- Adv --------------------------------------------------------------------
test_that("Adv", {
  ## ---- * Tropical Storm Alex, Forecast/Advisory 1
  expect_identical(scrape_adv_num(contents[1]), "1")
  ## ---- * Tropical Storm Katrina, Forecast/Advisory 1
  expect_identical(scrape_adv_num(contents[7]), "1")
})

## ---- Date -------------------------------------------------------------------
test_that("Date", {
  ## ---- * Tropical Storm Alex, Forecast/Advisory 1
  expect_identical(scrape_date(contents[1]),
                   as.POSIXct("1998-07-27 15:00:00", tz = "UTC"))
  ## ---- * Tropical Storm Katrina, Forecast/Advisory 1
  expect_identical(scrape_date(contents[7]),
                   as.POSIXct("1999-10-28 21:00:00", tz = "UTC"))
})

## ---- Key --------------------------------------------------------------------
test_that("Key", {
  ## ---- * Tropical Storm Alex, Forecast/Advisory 1
  expect_identical(scrape_key(contents[1]), "AL011998")
  ## ---- * Tropical Storm Katrina, Forecast/Advisory 1
  expect_identical(scrape_key(contents[7]), "AL151999")
})
