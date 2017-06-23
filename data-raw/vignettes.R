# Datasets for vignettes

## ---- Libraries --------------------------------------------------------------
library(dplyr)
library(rrricanes)

## ---- Getting Started --------------------------------------------------------
al.2012 <- get_storms(year = 2012, basin = "AL")

al182012_fstadv <- al.2012 %>%
    filter(Name == "Hurricane Sandy") %>%
    .$Link %>%
    get_fstadv()

al182012 <- al.2012 %>%
    filter(Name == "Hurricane Sandy") %>%
    .$Link %>%
    get_storm_data(c("fstadv", "wndprb"))

al122005_prblty <- get_storms(year = 2005, basin = "AL") %>%
    filter(Name == "Hurricane Katrina") %>%
    .$Link %>%
    get_prblty()

al182012_wndprb <- al.2012 %>%
    filter(Name == "Hurricane Sandy") %>%
    .$Link %>%
    get_wndprb()

devtools::use_data(al.2012, al182012_fstadv, al182012, al122005_prblty,
                   al182012_wndprb)

## ---- GIS Data ---------------------------------------------------------------
adv <- gis_advisory(key = "AL182012", advisory = "18") %>% gis_download()

ss <- gis_prob_storm_surge(key = "AL142016",
                           products = list(psurge = 0),
                           datetime = "20161006") %>%
    last() %>%
    gis_download()

wf <- gis_windfield("AL142016", advisory = "33") %>% gis_download()

wsp <- gis_wsp(datetime = "2016100606", res = 0.5) %>% gis_download()

devtools::use_data(adv, ss, wf, wsp)
