context("Test GIS functions.")

## ---- gis_advisory -----------------------------------------------------------
test_that("gis_advisory", {
  expect_identical(
    gis_advisory("AL092008", 32),
    sprintf(
      "%s%s%s",
      rrricanes:::get_nhc_link(),
      "gis/forecast/archive/",
      "al092008_5day_032.zip"
    )
  )
})

## ---- gis_breakpoints --------------------------------------------------------
# TODO #109
# test_that("gis_breakpoints", {
#   expect_identical(gis_breakpoints(2017),
#                    sprintf("%s%s%s",
#                            rrricanes:::get_nhc_link(),
#                            "gis/breakpoints/archive/",
#                            "Breakpoints_2017.zip"))
# })

## ---- gis_outlook ------------------------------------------------------------
test_that("gis_outlook", {
  expect_identical(
    gis_outlook(),
    sprintf(
      "%s%s",
      rrricanes:::get_nhc_link(),
      "xgtwo/gtwo_shapefiles.zip"
    )
  )
})

## ---- gis_prob_storm_surge ---------------------------------------------------
test_that("gis_prob_storm_surge", {
  expect_identical(
    gis_prob_storm_surge("AL092016",
      products = list("psurge" = 0),
      datetime = "2016090306"
    ),
    sprintf(
      "%s%s%s",
      rrricanes:::get_nhc_link(),
      "gis/storm_surge/",
      "al092016_psurge0_2016090306.zip"
    )
  )
})

## ---- gis_windfield ----------------------------------------------------------
test_that("gis_windfield", {
  expect_identical(
    gis_windfield("AL142016", advisory = 30),
    sprintf(
      "%s%s%s",
      rrricanes:::get_nhc_link(),
      "gis/forecast/archive/",
      "al142016_fcst_030.zip"
    )
  )
})

## ---- gis_wsp ----------------------------------------------------------------
test_that("NHC Link", {
  expect_identical(
    gis_wsp(datetime = "2016100606", res = 0.5),
    sprintf(
      "%s%s%s",
      rrricanes:::get_nhc_link(),
      "gis/forecast/archive/",
      "2016100606_wsp_120hrhalfDeg.zip"
    )
  )
})
