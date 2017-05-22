#' @title check-errors
#' @description Loop through all storms to look for errors and discrepancies.
#' @details This does not check for data quality; it only looks for any errors
#'     that arise from a given storm. Checking for data quality will have to be
#'     done in other ways.

## ---- Libraries --------------------------------------------------------------
library(dplyr)
library(Hurricanes)
library(purrr)
library(readr)

year <- 1999
basin <- "EP"

data.dir <- sprintf("~/Projects/HurricanesData/data/%d", year)
if (!dir.exists(data.dir))
    dir.create(data.dir)

storms <- get_storms(year = year, basin = basin) %>%
    mutate(n = row_number(),
           Key = paste0(basin, stringr::str_pad(string = n, width = 2, side = "left", pad = "0"), Year)) %>%
    select(Key, Name, Link)

# Storms to get
nums <- c(3:nrow(storms))

write_csv(storms, path = paste(data.dir, paste0(basin, year, ".csv"), sep = "/"))

map(.x = nums, .f = function(x) {
    url <- storms %>% slice(x) %>% .$Link
    key <- storms %>% slice(x) %>% .$Key
    obs.fstadv <- get_fstadv(url)
    obs.prblty <- get_prblty(url)

    storm.dir <- paste(data.dir, key, sep = "/")
    if (!dir.exists(storm.dir))
        dir.create(storm.dir)

    write_csv(obs.fstadv, path = paste(storm.dir, paste0(key, "-fstadv.csv"), sep = "/"))
    write_csv(obs.prblty, path = paste(storm.dir, paste0(key, "-prblty.csv"), sep = "/"))

})
