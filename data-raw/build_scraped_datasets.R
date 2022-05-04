#' ########################################################################## '#
#' build_scraped_datasets
#'
#' Run script to build full datasets for all products. Should only be run after
#' major revisions to the packages product functions output.
#'
#' ########################################################################## '#

## ---- Libraries --------------------------------------------------------------
library(dplyr)
library(purrr)
library(readr)
library(rrricanes)

## ---- Variables --------------------------------------------------------------
basins <- c("AL", "EP")
years <- 1998:2017
products <- c("discus", "fstadv", "posest", "prblty", "public", "update",
              "wndprb")

## ---- Get Storms -------------------------------------------------------------
storms <- map_df(years, .f = function(year) {
    map_df(basins, .f = function(basin) {
        get_storms(year = year, basin = basin)
    })
})

## ---- Storm Discussions ------------------------------------------------------
discus <- map_df(storms |> .$Link, .f = function(storm_link) {
    safely_get_discus <- purrr::safely(get_discus)
    df <- safely_get_discus(link = storm_link)
    if (is.null(df$error)) {
        return(df$result)
    } else {
        write(sprintf("Storm Discussion error: %s\n%s", df$error, storm_link),
              file = "log.txt",
              append = TRUE)
    }
})

write_csv(discus, "./datasets/discus.csv")

## ---- Forecast/Advisory ------------------------------------------------------
fstadv <- map_df(storms |> .$Link, .f = function(storm_link) {
    safely_get_fstadv <- purrr::safely(get_fstadv)
    df <- safely_get_fstadv(link = storm_link)
    if (is.null(df$error)) {
      return(df$result)
    } else {
        write(sprintf("Forecast/Advisory error: %s\n%s", df$error, storm_link),
              file = "log.txt",
              append = TRUE)
    }
})

write_csv(fstadv, "./datasets/fstadv.csv")

adv <- tidy_adv(fstadv)
write_csv(adv, "./datasets/adv.csv")

fcst <- tidy_fcst(fstadv)
write_csv(fcst, "./datasets/fcst.csv")

wr <- tidy_wr(fstadv)
write_csv(wr, "./datasets/wr.csv")

fcst_wr <- tidy_fcst_wr(fstadv)
write_csv(fcst_wr, "./datasets/fcst_wr.csv")

## ---- Position Estimates -----------------------------------------------------
posest <- map_df(storms |> .$Link, .f = function(storm_link) {
    safely_get_posest <- purrr::safely(get_posest)
    df <- safely_get_posest(link = storm_link)
    if (is.null(df$error)) {
      return(df$result)
    } else {
        write(sprintf("Position Estimate error: %s\n%s", df$error, storm_link),
              file = "log.txt",
              append = TRUE)
    }
})

write_csv(posest, "./datasets/posest.csv")

## ---- Strike Probabilities ---------------------------------------------------
prblty <- map_df(storms |> .$Link, .f = function(storm_link) {
    safely_get_prblty <- purrr::safely(get_prblty)
    df <- safely_get_prblty(link = storm_link)
    if (is.null(df$error)) {
      return(df$result)
    } else {
        write(sprintf("Strike Probabilities error: %s\n%s", df$error, storm_link),
              file = "log.txt",
              append = TRUE)
    }
})

write_csv(prblty, "./datasets/prblty.csv")

## ---- Public Advisories ------------------------------------------------------
public <- map_df(storms |> .$Link, .f = function(storm_link) {
    safely_get_public <- purrr::safely(get_public)
    df <- safely_get_public(link = storm_link)
    if (is.null(df$error)) {
      return(df$result)
    } else {
        write(sprintf("Public Advisory error: %s\n%s", df$error, storm_link),
              file = "log.txt",
              append = TRUE)
    }
})

write_csv(public, "./datasets/public.csv")

## ---- Updates ----------------------------------------------------------------
update <- map_df(storms |> .$Link, .f = function(storm_link) {
    safely_get_update <- purrr::safely(get_update)
    df <- safely_get_update(link = storm_link)
    if (is.null(df$error)) {
      return(df$result)
    } else {
        write(sprintf("Update error: %s\n%s", df$error, storm_link),
              file = "log.txt",
              append = TRUE)
    }
})

write_csv(update, "./datasets/update.csv")

## ---- Wind Speed Probabilities -----------------------------------------------
wndprb <- map_df(storms |> .$Link, .f = function(storm_link) {
    safely_get_wndprb <- purrr::safely(get_wndprb)
    df <- safely_get_wndprb(link = storm_link)
    if (is.null(df$error)) {
      return(df$result)
    } else {
        write(sprintf("Wind Probabilities error: %s\n%s", df$error, storm_link),
              file = "log.txt",
              append = TRUE)
    }
})

write_csv(wndprb, "./datasets/wndprb.csv")
