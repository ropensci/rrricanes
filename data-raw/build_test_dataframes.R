library(Hurricanes)
library(dplyr)
library(purrr)

## ---- Base Data --------------------------------------------------------------
#' Get all storms for 1998, AL basin
al1998 <- get_storms(year = 1998, basin = "AL") %>%
    dplyr::select(Link) %>%
    purrr::flatten_chr()

#' Get all storms for 1998, EP basin
ep1998 <- get_storms(year = 1998, basin = "EP") %>%
    dplyr::select(Link) %>%
    purrr::flatten_chr()

#' Get all storms for 1999, EP basin
ep1999 <- get_storms(year = 1999, basin = "EP") %>%
    dplyr::select(Link) %>%
    purrr::flatten_chr()

#' Get storms for 2000, AL basin
al2000 <- get_storms(year = 2000, basin = "AL") %>%
    select(Link) %>%
    flatten_chr()

#' Get storms for 2000, EP basin
ep2000 <- get_storms(year = 2000, basin = "EP") %>%
    select(Link) %>%
    flatten_chr()

#' Get storms for 2001, AL basin
al2001 <- get_storms(year = 2001, basin = "AL") %>%
    select(Link) %>%
    flatten_chr()

#' Get storms for 2002, AL basin
al2002 <- get_storms(year = 2002, basin = "AL") %>%
    select(Link) %>%
    flatten_chr()

#' Get storms for 2005, AL basin
al2005 <- get_storms(year = 2005, basin = "AL") %>%
    select(Link) %>%
    flatten_chr()

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

## ---- * 1999, EP, 03 ---------------------------------------------------------
#' See GitHub Issue #53
ep031999.fstadv <- get_fstadv(ep1999[3])
save(ep031999.fstadv, file = "./inst/extdata/ep031999.fstadv.Rda", compression_level = 9)

## ---- * 1999, EP, 04 ---------------------------------------------------------
ep041999.fstadv <- get_fstadv(ep1999[4])
save(ep041999.fstadv, file = "./inst/extdata/ep041999.fstadv.Rda", compression_level = 9)

## ---- * 1999, EP, 07 ---------------------------------------------------------
ep071999.fstadv <- get_fstadv(ep1999[7])
save(ep071999.fstadv, file = "./inst/extdata/ep071999.fstadv.Rda", compression_level = 9)

## ---- * 1999, EP, 08 ---------------------------------------------------------
ep081999.fstadv <- get_fstadv(ep1999[8])
save(ep081999.fstadv, file = "./inst/extdata/ep081999.fstadv.Rda", compression_level = 9)

## ---- * 2000, AL, 02 ---------------------------------------------------------
al022000.fstadv <- get_fstadv(al2000[2])
save(al022000.fstadv, file = "./inst/extdata/al022000.fstadv.Rda", compression_level = 9)

## ---- * 2000, EP, 06 ---------------------------------------------------------
ep062000.fstadv <- get_fstadv(ep2000[6])
save(ep062000.fstadv, file = "./inst/extdata/ep062000.fstadv.Rda", compression_level = 9)

## ---- * 2002, AL, 13 ---------------------------------------------------------
al132002.fstadv <- get_fstadv(al2002[13])
save(al132002.fstadv, file = "./inst/extdata/al132002.fstadv.Rda", compression_level = 9)

## ---- * 2005, AL, 25 ---------------------------------------------------------
al302005.fstadv <- get_fstadv(al2005[30])
save(al302005.fstadv, file = "./inst/extdata/al302005.fstadv.Rda", compression_level = 9)

## ---- prblty -----------------------------------------------------------------

## ---- * 1998, AL, 01 ---------------------------------------------------------
al011998.prblty <- get_prblty(al1998[1])
save(al011998.prblty,
     file = "./inst/extdata/al011998.prblty.Rda", compression_level = 9)

## ---- * 2000, AL, 02 ---------------------------------------------------------
al022000.prblty <- get_prblty(al2000[2])
save(al022000.prblty, file = "./inst/extdata/al022000.prblty.Rda", compression_level = 9)

## ---- * 2000, AL, 03 ---------------------------------------------------------
al032000.prblty <- get_prblty(al2000[3])
save(al032000.prblty, file = "./inst/extdata/al032000.prblty.Rda", compression_level = 9)

## ---- * 2001, AL, 16 ---------------------------------------------------------
al162001.prblty <- get_prblty(al2001[16])
save(al162001.prblty, file = "./inst/extdata/al162001.prblty.Rda", compression_level = 9)

## ---- wndprb -----------------------------------------------------------------

## ---- * 2006, AL, 01 ---------------------------------------------------------
al012006.wndprb <- get_wndprb(al2006[1])
save(al012006.wndprb,
     file = "./inst/extdata/al012006.wndprb.Rda", compression_level = 9)

## ---- * 2006, AL, 03 ---------------------------------------------------------
al032006.wndprb <- get_wndprb(al2006[3])
save(al032006.wndprb,
     file = "./inst/extdata/al032006.wndprb.Rda", compression_level = 9)
