library(Hurricanes)
library(dplyr)
library(purrr)

#' Get all storms for 1998, AL basin
al1998 <- get_storms(year = 1998, basin = "AL") %>%
    dplyr::select(Link) %>%
    purrr::flatten_chr()

#' Get all storms for 1998, EP basin
ep1998 <- get_storms(year = 1998, basin = "EP") %>%
    dplyr::select(Link) %>%
    purrr::flatten_chr()

#' Get Tropical Storm Alex
al011998 <- get_fstadv(al1998[1])
save(al011998, file = "./inst/extdata/al011998.Rda", compression_level = 9)

#' Get Hurricane Bonnie
al021998 <- get_fstadv(al1998[2])
save(al021998, file = "./inst/extdata/al021998.Rda", compression_level = 9)

#' Get Tropical Storm Agatha
ep011998 <- get_fstadv(ep1998[1])
save(ep011998, file = "./inst/extdata/ep011998.Rda", compression_level = 9)

#' Get Tropical Depression Two-E
ep021998 <- get_fstadv(ep1998[2])
save(ep021998, file = "./inst/extdata/ep021998.Rda", compression_level = 9)
