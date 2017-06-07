context("Test GIS functions.")

## ---- gis_wsp ----------------------------------------------------------------
test_that("NHC Link", {
    expect_identical(gis_wsp(datetime = "2016100606", res = 0.5),
                    "http://www.nhc.noaa.gov/gis/forecast/archive/2016100606_wsp_120hrhalfDeg.zip")
})