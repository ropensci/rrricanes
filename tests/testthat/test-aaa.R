context("---MP TESTS---")

# Calls are stubbed with mock data if !test_all - note that I use a local env
# variable to do full tests.
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
             identical (Sys.getenv ("TRAVIS"), "true") |
             identical (Sys.getenv ("APPVEYOR"), "True"))

source ("../stub.R")

do_local <- FALSE
if (do_local) {
    # when vcr appears on CRAN, this'll be much simpler:
    #st_test <- get_storms(year = 2017, basin = "AL")
    #saveRDS (st_test, file = "./tests/testthat/test_st17.Rds")

    # stubbed data for `get_url_contents()`
    url17 <- "http://www.nhc.noaa.gov/archive/2017/"
    cfm_output_st <- NULL
    trace(
          curl::curl_fetch_memory,
          exit = function() {
              cfm_output_st <<- returnValue()
          }
          )
    res <- httr::GET (url17)
    untrace (curl::curl_fetch_memory)
    save (cfm_output_st, file = "cfm_storms17.rda")

    # stubbed data for `extract_storms()`
    url_contents_result <- get_url_contents (url17)
    xml2::write_xml (url_contents_result, file = "../url_contents_result.xml")
}

# uncomment following to trace curl fetches:
#trace ( curl::curl_fetch_memory, exit = function() { })

test_that("stubbed get_url_contents", {
              load ("../cfm_storms17.rda")
              link <- "http://www.nhc.noaa.gov/archive/2017/"
              #if (!test_all) #TODO: Uncomment for proper tests
              stub (get_url_contents, 'httr::GET',
                    function (url, ...) cfm_output_st )

              expect_is (get_url_contents (link), "xml_document")
          })

test_that("stubbed extract_storms", {
              # The XML files from nhc.noaa are malformed!
              suppressWarnings (url_contents_result <- xml2::read_xml
                                ("../url_contents_result.xml", as_html = TRUE))
              link <- "http://www.nhc.noaa.gov/archive/2017/"
              #if (!test_all) #TODO: Uncomment for proper tests
              stub (extract_storms, "get_url_contents",
                    function (link) url_contents_result)
              st <- extract_storms (basin = "AL", link = link)
              expect_equal (ncol (st), 4)
          })
