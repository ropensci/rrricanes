## ----libraries-----------------------------------------------------------
library(dplyr)
library(Hurricanes)

## ----chunk-1-------------------------------------------------------------
(al.1998.charley <- get_storms(year = 1998, basin = "AL") %>% 
  filter(Name == "TROPICAL STORM CHARLEY"))

## ----chunk-2-------------------------------------------------------------
(a <- al.1998.charley %>% .$Link)
(b <- al.1998.charley %>% select(Link) %>% first())
(c <- al.1998.charley[1,4])
(d <- "http://www.nhc.noaa.gov/archive/1998/1998CHARLEYadv.html")

## ----chunk-3-------------------------------------------------------------
identical(a, b)

## ----chunk-4-------------------------------------------------------------
identical(b, c)

## ----chunk-5-------------------------------------------------------------
identical(c, d)

## ----chunk-6, eval = FALSE-----------------------------------------------
#  get_storm_data("fstadv", link = a, msg = TRUE)

## ----chunk-7, eval = FALSE-----------------------------------------------
#  get_storm_data("fstadv", link = b, msg = TRUE)

## ----chunk-8, eval = FALSE-----------------------------------------------
#  get_storm_data("fstadv", link = c, msg = TRUE)

## ----chunk-9-------------------------------------------------------------
get_storm_data("fstadv", link = d, msg = TRUE)

