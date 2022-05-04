library(dplyr)

AL122005 <-

  "%sarchive/2005/KATRINA.shtml?" |>
  sprintf(rrricanes:::get_nhc_link()) |>
  rrricanes:::extract_storm_links()

files <- list(
  "AL011991" = list(
    "fstadv" = c("mal0191.001")
  ),
  "AL011998" = list(
    "fstadv" = c("mal0198.001")
  ),
  "AL151999" = list(
    "fstadv" = c("mal1599.001")
  ),
  "AL012001" = list(
    "fstadv" = c("al012001.fstadv.001", "al012001.fstadv.004")
  ),
  "AL012003" = list(
    "fstadv" = c("al012003.fstadv.001")
  ),
  "AL012004" = list(
    "fstadv" = c("al012004.fstadv.012")
  ),
  "AL012011" = list(
    "fstadv" = c("al012011.fstadv.010")
  ),
  "AL012012" = list(
    "fstadv" = c("al012012.fstadv.012")
  ),
  "AL022017" = list(
    "fstadv" = c("al022017.fstadv.001")
  ),
  "AL102008" = list(
    "fstadv" = c("al102008.fstadv.017")
  ),
  "AL102017" = list(
    "fstadv" = c("al102017.fstadv.004")
  ),
  "AL052011" = list(
    "fstadv" = c("al052011.fstadv.001")
  ),
  "CP031994" = list(
    "fstadv" = c("mcp0394.001")
  ),
  "EP132014" = list(
      "fstadv" = c("ep132014.fstadv.001.08220251")
  )
)

AL092008 <-
  "%sarchive/2008/IKE.shtml?" |>
  sprintf(rrricanes:::get_nhc_link()) |>
  rrricanes:::extract_storm_links()

## ---- Saved Data -------------------------------------------------------------
df.tests <- system.file("extdta", "test_datasets.RData", package = "rrricanes")

## ---- Get Data ---------------------------------------------------------------
al_1998 <- get_storms(years = 1998, basins = "AL")
al_2008 <- get_storms(years = 2008, basins = "AL")
al_2017 <- get_storms(years = 2017, basins = "AL")

df.al_01_2017_products <- rrricanes:::get_storm_data(al_2017[[1,4]],
                                             products = c("discus", "fstadv"))
df.al_09_2008_discus <- rrricanes:::get_discus(al_2008[[9,4]])
df.al_09_2008_fstadv <- rrricanes:::get_fstadv(al_2008[[9,4]])
df.al_09_2008_posest <- rrricanes:::get_posest(al_2008[[9,4]])
#df.al_01_1998_prblty <- rrricanes:::get_prblty(al_1998[[1,4]])
df.al_09_2008_public <- rrricanes:::get_public(al_2008[[9,4]])
df.al_09_2008_update <- rrricanes:::get_update(al_2008[[9,4]])
df.al_09_2008_wndprb <- rrricanes:::get_wndprb(al_2008[[9,4]])
