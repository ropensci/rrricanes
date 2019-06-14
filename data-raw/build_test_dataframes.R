library(dplyr)
library(rrricanes)

al_1998 <- get_storms(years = 1998, basins = "AL")

al_2008 <- get_storms(years = 2008, basins = "AL")

al_2017 <- get_storms(years = 2017, basins = "AL")

al_01_2017_products <- get_storm_data(
        links = al_2017[[1,4]],
        products = c("discus", "fstadv")
)

al_09_2008_discus <- get_discus(al_2008[[9,4]])

al_09_2008_fstadv <- get_fstadv(al_2008[[9,4]])

al_09_2008_posest <- get_posest(al_2008[[9,4]])

al_01_1998_prblty <- get_prblty(al_1998[[1,4]])

al_09_2008_public <- get_public(al_2008[[9,4]])

al_09_2008_update <- get_update(al_2008[[9,4]])

al_09_2008_wndprb <- get_wndprb(al_2008[[9,4]])

usethis::use_data(
        al_01_2017_products,
        al_09_2008_discus,
        al_09_2008_fstadv,
        al_09_2008_posest,
        al_01_1998_prblty,
        al_09_2008_public,
        al_09_2008_update,
        al_09_2008_wndprb,
        internal = TRUE
)
