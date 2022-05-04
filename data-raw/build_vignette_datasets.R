# Datasets for vignettes

## ---- Libraries --------------------------------------------------------------
library(dplyr)
library(rrricanes)

## ---- Getting Started --------------------------------------------------------
df.al_2012 <- get_storms(year = 2012, basin = "AL")

df.al_18_2012_fstadv <- df.al_2012 |>
    filter(Name == "Hurricane Sandy") |>
    .$Link |>
    get_fstadv()

df.al_18_2012 <- df.al_2012 |>
    filter(Name == "Hurricane Sandy") |>
    .$Link |>
    get_storm_data(c("fstadv", "wndprb"))

df.al_12_2005_prblty <- get_storms(year = 2005, basin = "AL") |>
    filter(Name == "Hurricane Katrina") |>
    .$Link |>
    get_prblty()

df.al_18_2012_wndprb <- df.al_2012 |>
    filter(Name == "Hurricane Sandy") |>
    .$Link |>
    get_wndprb()

usethis::use_data(
    df.al_2012,
    df.al_18_2012_fstadv,
    df.al_18_2012,
    df.al_12_2005_prblty,
    df.al_18_2012_wndprb,
    overwrite = TRUE
)

## ---- GIS Data ---------------------------------------------------------------
df.gis_adv <- gis_advisory(key = "AL182012", advisory = "18") |> gis_download()

df.gis_storm_surge <- gis_prob_storm_surge(key = "AL142016",
                           products = list(psurge = 0),
                           datetime = "20161006") |>
    last() |>
    gis_download()

df.gis_wind_radii <- gis_windfield("AL142016", advisory = "33") |> gis_download()

df.gis_wsp <- gis_wsp(datetime = "2016100606", res = 0.5) |> gis_download()

usethis::use_data(
    df.gis_adv,
    df.gis_storm_surge,
    df.gis_wind_radii,
    df.gis_wsp,
    overwrite = TRUE
)
