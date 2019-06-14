context("Scrapers")

## --- scrape_header() ----
test_that("scrape_header", {

  ## ---- * AL011991 ----
  # "\nTROPICAL STORM ANA MARINE ADVISORY NUMBER   1\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$AL011991$fstadv[1])),
    c("Tropical Storm", "Ana", "1")
  )

  ## ---- * AL012001 ----
  # "\nTROPICAL STORM ALLISON SPECIAL FORECAST/ADVISORY NUMBER   1\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$AL012001$fstadv[1])),
    c("Tropical Storm", "Allison", "1")
  )

  ## ---- * AL012001 ----
  # "\nTROPICAL DEPRESSION ALLISON FORECAST/ADVISORY NUMBER   4\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$AL012001$fstadv[2])),
    c("Tropical Depression", "Allison", "4")
  )

  ## ---- * AL012003 ----
  # "\nSUBTROPICAL STORM ANA FORECAST/ADVISORY NUMBER   1\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$AL012003$fstadv[1])),
    c("Subtropical Storm", "Ana", "1")
  )

  ## ---- * AL012004 ----
  # "\nHURRICANE ALEX FORECAST/ADVISORY NUMBER  12\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$AL012004$fstadv[1])),
    c("Hurricane", "Alex", "12")
  )

  ## ---- * AL012011 ----
  # "\nREMNANTS OF ARLENE FORECAST/ADVISORY NUMBER  10\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$AL012011$fstadv[1])),
    c("Remnants", "Arlene", "10")
  )

  ## ---- * AL012012 ----
  # "\nPOST-TROPICAL CYCLONE ALBERTO FORECAST/ADVISORY NUMBER  12\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$AL012012$fstadv[1])),
    c("Post-Tropical Cyclone", "Alberto", "12")
  )

  ## ---- * AL022017 ----
  # "\nPOTENTIAL TROPICAL CYCLONE TWO FORECAST/ADVISORY NUMBER   1\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$AL022017$fstadv[1])),
    c("Potential Tropical Cyclone", "Two", "1")
  )

  ## ---- * AL052011 ----
  # "\nTROPICAL STORM EMILY \nSPECIAL FORECAST/ADVISORY NUMBER   1...CORRECTED\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$AL052011$fstadv[1])),
    c("Tropical Storm", "Emily", "1")
  )

  ## ---- * AL102017 ----
  # "\nPOTENTIAL TROPICAL CYCLONE TEN\nFORECAST/ADVISORY NUMBER  4...CORRECTED\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$AL102017$fstadv[1])),
    c("Potential Tropical Cyclone", "Ten", "4")
  )

  ## ---- * AL102008 ----
  # "\nTROPICAL DEPRESSION JOSEPHINE FORECAST/ADVISORY NUMBER \n17...CORRECTED\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$AL102008$fstadv[1])),
    c("Tropical Depression", "Josephine", "17")
  )

  ## ---- * CP031994 ----
  # "\nTROPICAL DEPRESSION THREE-C ADVISORY NUMBER 1\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$CP031994$fstadv[1])),
    c("Tropical Depression", "Three-C", "1")
  )

  ## ---- * EP132014 ----
  # "\nTROPICAL DEPRESSION THIRTEEN-E FORECAST/ADVISORY\nNUMBER 1...CORRECTED\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$EP132014$fstadv[1])),
    c("Tropical Depression", "Thirteen-E", "1")
  )
})

## ---- Key --------------------------------------------------------------------
test_that("Key", {
  ## ---- * Tropical Storm Alex, Forecast/Advisory 1
  expect_identical(
    rrricanes:::scrape_key(read_files(files$AL011998$fstadv[1])),
    "AL0198"
  )

  ## ---- * Tropical Storm Katrina, Forecast/Advisory 1
  expect_identical(
    rrricanes:::scrape_key(read_files(files$AL151999$fstadv[1])),
    "AL1599"
  )

})
