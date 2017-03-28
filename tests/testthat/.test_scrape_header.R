context('Scrape Header')

header.forecast.AL011998 <- paste("TZCZC MIATCMAT1 ALL", 
                                  "TTAA00 KNHC DDHHMM", 
                                  "TROPICAL DEPRESSION ONE FORECAST/ADVISORY NUMBER   1", 
                                  "NATIONAL WEATHER SERVICE MIAMI FL   AL0198", 
                                  "1500Z MON JUL 27 1998", 
                                  "", "", sep = "\n")

header.public.AL011998 <- paste("ZCZC MIATCPAT1 ALL", 
                                "TTAA00 KNHC DDHHMM", 
                                "BULLETIN", 
                                "TROPICAL DEPRESSION ONE ADVISORY NUMBER   1", 
                                "NATIONAL WEATHER SERVICE MIAMI FL", 
                                "11 AM AST MON JUL 27 1998", 
                                "", "", sep = "\n")

header.discussion.AL011998 <- paste("ZCZC MIATCDAT1 ALL", 
                                    "TTAA00 KNHC DDHHMM", 
                                    "TROPICAL DEPRESSION ONE DISCUSSION NUMBER   1", 
                                    "NATIONAL WEATHER SERVICE MIAMI FL", 
                                    "11 AM EDT MON JUL 27 1998", 
                                    "", "", sep = "\n")

header.probabilities.AL011998 <- paste("ZCZC MIASPFAT1 ALL", 
                                       "TTAA00 KNHC DDHHMM", 
                                       "TROPICAL STORM ALEX PROBABILITIES NUMBER   7", 
                                       "NATIONAL WEATHER SERVICE MIAMI FL", 
                                       "11 PM AST TUE JUL 28 1998", 
                                       "", "", sep = "\n")

test_that("Extract Status", {
  expect_identical(scrape_header(header.forecast.AL011998, ret = "status"), "TROPICAL DEPRESSION")
  expect_identical(scrape_header(header.public.AL011998, ret = "status"), "TROPICAL DEPRESSION")
  expect_identical(scrape_header(header.discussion.AL011998, ret = "status"), "TROPICAL DEPRESSION")
  expect_identical(scrape_header(header.probabilities.AL011998, ret = "status"), "TROPICAL STORM")
})

test_that("Extract Name", {
  expect_identical(scrape_header(header.forecast.AL011998, ret = "name"), "ONE")
  expect_identical(scrape_header(header.public.AL011998, ret = "name"), "ONE")
  expect_identical(scrape_header(header.discussion.AL011998, ret = "name"), "ONE")
  expect_identical(scrape_header(header.probabilities.AL011998, ret = "name"), "ALEX")
})

test_that("Extract Advisory Number", {
  expect_identical(scrape_header(header.forecast.AL011998, ret = "adv"), 1)
  expect_identical(scrape_header(header.public.AL011998, ret = "adv"), 1)
  expect_identical(scrape_header(header.discussion.AL011998, ret = "adv"), 1)
  expect_identical(scrape_header(header.probabilities.AL011998, ret = "adv"), 7)
})

test_that("Extract Dates, Times", {
  expect_identical(scrape_header(header.forecast.AL011998, ret = "date"), 
                   as.POSIXct("1998-07-27 15:00:00", tz = "UTC"))
  expect_identical(scrape_header(header.public.AL011998, ret = "date"), 
                   as.POSIXct("1998-07-27 11:00:00", tz = "AST"))
  expect_identical(scrape_header(header.discussion.AL011998, ret = "date"), 
                   as.POSIXct("1998-07-27 11:00:00", tz = "EDT"))
  expect_identical(scrape_header(header.probabilities.AL011998, ret = "date"), 
                   as.POSIXct("1998-07-28 23:00:00", tz = "AST"))
})
