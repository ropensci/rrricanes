library(dplyr)
library(rrricanes)

al_1998 <- get_storms(years = 1998, basins = "AL")
al_2008 <- get_storms(years = 2008, basins = "AL")
al_2017 <- get_storms(years = 2017, basins = "AL")

al_01_2017_products <- get_storm_data(al_2017[[1,4]],
                                      products = c("discus", "fstadv"))
save(al_01_2017_products, file = "./inst/extdata/al_01_2017_products.Rdata",
     compression_level = 9)

al_09_2008_discus <- get_discus(al_2008[[9,4]])
save(al_09_2008_discus, file = "./inst/extdata/al_09_2008_discus.Rdata",
     compression_level = 9)

al_09_2008_fstadv <- get_fstadv(al_2008[[9,4]])
save(al_09_2008_fstadv, file = "./inst/extdata/al_09_2008_fstadv.Rdata",
     compression_level = 9)

al_09_2008_posest <- get_posest(al_2008[[9,4]])
save(al_09_2008_posest, file = "./inst/extdata/al_09_2008_posest.Rdata",
     compression_level = 9)

al_01_1998_prblty <- get_prblty(al_1998[[1,4]])
save(al_01_1998_prblty, file = "./inst/extdata/al_01_1998_prblty.Rdata",
     compression_level = 9)

al_09_2008_public <- get_public(al_2008[[9,4]])
save(al_09_2008_public, file = "./inst/extdata/al_09_2008_public.Rdata",
     compression_level = 9)

al_09_2008_update <- get_update(al_2008[[9,4]])
save(al_09_2008_update, file = "./inst/extdata/al_09_2008_update.Rdata",
     compression_level = 9)

al_09_2008_wndprb <- get_wndprb(al_2008[[9,4]])
save(al_09_2008_wndprb, file = "./inst/extdata/al_09_2008_wndprb.Rdata",
     compression_level = 9)
