context("GIS data")

## ---- Test fstadv_gis() ------------------------------------------------------
test_that("Test fstadv_gis()", {
    expect_error(fstadv_gis(key = "AL0198"), "Invalid key")
})
