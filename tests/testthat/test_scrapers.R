context("Scrapers")

## --- scrape_header() ----
test_that("scrape_header", {

  ## ---- * AL011991 ----
  # "\nTROPICAL STORM ANA MARINE ADVISORY NUMBER   1\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$AL011991$fstadv[1])),
    c("TROPICAL STORM", "ANA", "1")
  )

  ## ---- * AL012001 ----
  # "\nTROPICAL STORM ALLISON SPECIAL FORECAST/ADVISORY NUMBER   1\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$AL012001$fstadv[1])),
    c("TROPICAL STORM", "ALLISON", "1")
  )

  ## ---- * AL012001 ----
  # "\nTROPICAL DEPRESSION ALLISON FORECAST/ADVISORY NUMBER   4\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$AL012001$fstadv[2])),
    c("TROPICAL DEPRESSION", "ALLISON", "4")
  )

  ## ---- * AL012003 ----
  # "\nSUBTROPICAL STORM ANA FORECAST/ADVISORY NUMBER   1\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$AL012003$fstadv[1])),
    c("SUBTROPICAL STORM", "ANA", "1")
  )

  ## ---- * AL012004 ----
  # "\nHURRICANE ALEX FORECAST/ADVISORY NUMBER  12\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$AL012004$fstadv[1])),
    c("HURRICANE", "ALEX", "12")
  )

  ## ---- * AL012011 ----
  # "\nREMNANTS OF ARLENE FORECAST/ADVISORY NUMBER  10\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$AL012011$fstadv[1])),
    c("REMNANTS", "ARLENE", "10")
  )

  ## ---- * AL012012 ----
  # "\nPOST-TROPICAL CYCLONE ALBERTO FORECAST/ADVISORY NUMBER  12\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$AL012012$fstadv[1])),
    c("POST-TROPICAL CYCLONE", "ALBERTO", "12")
  )

  ## ---- * AL022017 ----
  # "\nPOTENTIAL TROPICAL CYCLONE TWO FORECAST/ADVISORY NUMBER   1\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$AL022017$fstadv[1])),
    c("POTENTIAL TROPICAL CYCLONE", "TWO", "1")
  )

  ## ---- * AL052011 ----
  # "\nTROPICAL STORM EMILY \nSPECIAL FORECAST/ADVISORY NUMBER   1...CORRECTED\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$AL052011$fstadv[1])),
    c("TROPICAL STORM", "EMILY", "1")
  )

  ## ---- * AL102017 ----
  # "\nPOTENTIAL TROPICAL CYCLONE TEN\nFORECAST/ADVISORY NUMBER  4...CORRECTED\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$AL102017$fstadv[1])),
    c("POTENTIAL TROPICAL CYCLONE", "TEN", "4")
  )

  ## ---- * AL102008 ----
  # "\nTROPICAL DEPRESSION JOSEPHINE FORECAST/ADVISORY NUMBER \n17...CORRECTED\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$AL102008$fstadv[1])),
    c("TROPICAL DEPRESSION", "JOSEPHINE", "17")
  )

  ## ---- * CP031994 ----
  # "\nTROPICAL DEPRESSION THREE-C ADVISORY NUMBER 1\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$CP031994$fstadv[1])),
    c("TROPICAL DEPRESSION", "THREE-C", "1")
  )

  ## ---- * EP132014 ----
  # "\nTROPICAL DEPRESSION THIRTEEN-E FORECAST/ADVISORY\nNUMBER 1...CORRECTED\n"
  expect_identical(
    rrricanes:::scrape_header(read_files(files$EP132014$fstadv[1])),
    c("TROPICAL DEPRESSION", "THIRTEEN-E", "1")
  )
})

## ---- Key --------------------------------------------------------------------
test_that("Key", {
  ## ---- * Tropical Storm Alex, Forecast/Advisory 1
  expect_identical(scrape_key(contents[1]), "AL011998")
  ## ---- * Tropical Storm Katrina, Forecast/Advisory 1
  expect_identical(scrape_key(contents[7]), "AL151999")
})
