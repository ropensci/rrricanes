## ----libraries-----------------------------------------------------------
library(dplyr)
library(Hurricanes)

## ----chunk-1-------------------------------------------------------------
head(get_storms(year = 2016), n = 5L)

## ----chunk-2-------------------------------------------------------------
get_storms(year = 2015, basin = "AL")

## ----chunk-3-------------------------------------------------------------
get_storms(year = 2014:2015, basin = "EP")

## ----chunk-4-------------------------------------------------------------
toproper("Hurricane ODILE")
toproper("HURRICANE ODILE")

## ----chunk-5-------------------------------------------------------------
al.1998.charley <- get_storms(year = 1998, basin = "AL") %>% 
  filter(Name == "TROPICAL STORM CHARLEY")

## ----chunk-6-------------------------------------------------------------
get_storm_data("fstadv", link = al.1998.charley %>% select(Link) %>% first())

## ----chunk-7-------------------------------------------------------------
head(fstadv)

## ----chunk-8, eval = FALSE-----------------------------------------------
#  get_storm_data("fstadv",
#                 names = list("fstadv" = "al.1998.charley.fstadv"),
#                 link = al.1998.charley %>% .$Link)

