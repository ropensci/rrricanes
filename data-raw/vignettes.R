# Datasets for vignettes

## ---- Libraries --------------------------------------------------------------
library(dplyr)
library(rrricanes)

## ---- Getting Started --------------------------------------------------------
al.2012 <- get_storms(year = 2012, basin = "AL")
save(al.2012, file = "./data/al.2012.Rda", compression_level = 9)

al182012_fstadv <- al.2012 %>%
    filter(Name == "Hurricane Sandy") %>%
    .$Link %>%
    get_fstadv()
save(al182012_fstadv, file = "./data/al182012_fstadv.Rda",
     compression_level = 9)

al182012 <- al.2012 %>%
    filter(Name == "Hurricane Sandy") %>%
    .$Link %>%
    get_storm_data(c("fstadv", "wndprb"))
save(al182012, file = "./data/al182012.Rda", compression_level = 9)

al122005_prblty <- get_storms(year = 2005, basin = "AL") %>%
    filter(Name == "Hurricane Katrina") %>%
    .$Link %>%
    get_prblty()
save(al122005_prblty, file = "./data/al122005_prblty.Rda",
     compression_level = 9)

al122005_wndprb <- al.2012 %>%
    filter(Name == "Hurricane Sandy") %>%
    .$Link %>%
    get_wndprb()
save(al122005_wndprb, file = "./data/al122005_wndprb.Rda",
     compression_level = 9)

## ---- GIS Data ---------------------------------------------------------------
adv <- gis_advisory(key = "AL182012", advisory = "18") %>% gis_download()
save(adv, file = "./data/adv.Rda", compression_level = 9)

ss <- gis_prob_storm_surge(key = "AL142016",
                           products = list(psurge = 0),
                           datetime = "20161006") %>%
    last() %>%
    gis_download()
save(ss, file = "./data/ss.Rda", compression_level = 9)

wf <- gis_windfield("AL142016", advisory = "33") %>% gis_download()
save(wf, file = "./data/wf.Rda", compression_level = 9)

wsp <- gis_wsp(datetime = "2016100606", res = 0.5) %>% gis_download()
save(wsp, file = "./data/wsp.Rda", compression_level = 9)
