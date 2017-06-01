library(dplyr)
library(purrr)
library(rrricanes)

## ---- Base Data --------------------------------------------------------------
#' Get all storms for 1998, AL basin
al1998 <- get_storms(year = 1998, basin = "AL") %>%
    dplyr::select(Link) %>%
    purrr::flatten_chr()

#' Get all storms for 1998, EP basin
ep1998 <- get_storms(year = 1998, basin = "EP") %>%
    dplyr::select(Link) %>%
    purrr::flatten_chr()

#' Get storms for 2006, AL basin
al2006 <- get_storms(year = 2006, basin = "AL") %>%
    select(Link) %>%
    flatten_chr()

## ---- fstadv -----------------------------------------------------------------

## ---- * 1998, AL, 01 ---------------------------------------------------------
al011998.fstadv <- get_fstadv(al1998[1])
save(al011998.fstadv, file = "./inst/extdata/al011998.fstadv.Rda", compression_level = 9)

## ---- * 1998, AL, 02 ---------------------------------------------------------
al021998.fstadv <- get_fstadv(al1998[2])
save(al021998.fstadv, file = "./inst/extdata/al021998.fstadv.Rda", compression_level = 9)

## ---- * 1998, EP, 01 ---------------------------------------------------------
ep011998.fstadv <- get_fstadv(ep1998[1])
save(ep011998.fstadv, file = "./inst/extdata/ep011998.fstadv.Rda", compression_level = 9)

## ---- * 1998, EP, 02 ---------------------------------------------------------
ep021998.fstadv <- get_fstadv(ep1998[2])
save(ep021998.fstadv, file = "./inst/extdata/ep021998.fstadv.Rda", compression_level = 9)

## ---- prblty -----------------------------------------------------------------

## ---- * 1998, AL, 01 ---------------------------------------------------------
al011998.prblty <- get_prblty(al1998[1])
save(al011998.prblty, file = "./inst/extdata/al011998.prblty.Rda", compression_level = 9)

## ---- wndprb -----------------------------------------------------------------

## ---- * 2006, AL, 01 ---------------------------------------------------------
al012006.wndprb <- get_wndprb(al2006[1])
save(al012006.wndprb,
     file = "./inst/extdata/al012006.wndprb.Rda", compression_level = 9)
