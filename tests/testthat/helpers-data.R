AL122005 <-
  "%sarchive/2005/KATRINA.shtml?" %>%
  sprintf(rrricanes:::get_nhc_link()) %>%
  rrricanes:::extract_storm_links()

AL092008 <-
  "%sarchive/2008/IKE.shtml?" %>%
  sprintf(rrricanes:::get_nhc_link()) %>%
  rrricanes:::extract_storm_links()

## ---- Saved Data -------------------------------------------------------------
load(system.file("extdata", "al_01_1998_prblty.Rdata", package = "rrricanes"))
load(system.file("extdata", "al_09_2008_discus.Rdata", package = "rrricanes"))
load(system.file("extdata", "al_09_2008_fstadv.Rdata", package = "rrricanes"))
load(system.file("extdata", "al_09_2008_posest.Rdata", package = "rrricanes"))
load(system.file("extdata", "al_09_2008_public.Rdata", package = "rrricanes"))
load(system.file("extdata", "al_09_2008_update.Rdata", package = "rrricanes"))
load(system.file("extdata", "al_01_2017_products.Rdata", package = "rrricanes"))
load(system.file("extdata", "al_09_2008_wndprb.Rdata", package = "rrricanes"))

## ---- Get Data ---------------------------------------------------------------
al_1998 <- rrricanes::get_storms(years = 1998, basins = "AL")
al_2008 <- rrricanes::get_storms(years = 2008, basins = "AL")
al_2017 <- rrricanes::get_storms(years = 2017, basins = "AL")

df.al_01_2017_products <- rrricanes:::get_storm_data(al_2017[[1,4]],
                                                     products = c("discus", "fstadv"))
df.al_09_2008_discus <- rrricanes:::get_discus(al_2008[[9,4]])
df.al_09_2008_posest <- rrricanes:::get_posest(al_2008[[9,4]])
df.al_01_1998_prblty <- rrricanes:::get_prblty(al_1998[[1,4]])
df.al_09_2008_public <- rrricanes:::get_public(al_2008[[9,4]])
df.al_09_2008_update <- rrricanes:::get_update(al_2008[[9,4]])
df.al_09_2008_wndprb <- rrricanes:::get_wndprb(al_2008[[9,4]])
