#' Fetch Products
#'
#' This script is designed to run the latest version of rrricanes and grab
#' every product available for every storm, each basin. If the product dataframe
#' is not empty, the dataframe is saved as a CSV to the directory assigned in
#' `d`. If `s` is TRUE, the dataframe is also returned to the global
#' environment.
#'
#' Each product output file and dataframe is named to make identification as
#' easy as possible. It is formatted as AB_C where A is the basin, B is the
#' year, and C is the product. For example, AL1998_fstadv means all data is
#' related to the fstadv prodcut for the Atlantic basin, 1998.
#'
#' If the product being worked is "fstadv", a summary dataframe is built as
#' well. This summary dataframe contains the Key, Name, StartDate and EndDate
#' for each storm.
#'
#' The summary dataframe and csv file is named similar to the product files with
#' only the basin and year. For example, EP2017 is the summary dataframe/file
#' for all storms in the East Pacific for the year 2017.
#'
#' This script can take some time to run; especially for fstadv products. This
#' time is amplified significantly the more years it is gathered. The biggest
#' issue is a connection timeout; if internet connection breaks or if the NHC
#' archive site experiences a brief disruption. Each yearly file is written
#' on completion. So if you run 1998:2017 and the script is interrupted, review
#' the directory (or your global environment if `s` is TRUE) for the last
#' object built or saved. Then modify all variables in section "Variables"
#' accordingly.
#'
#' For example, if the script stopped executing and the last dataframe (by year)
#' is AL2014, modify the years variable to 2015:2017.
#'
#' Same goes for the products. The products variable is loaded in alphabetical
#' order. So, if the script is interruped and the last dataframe saved is
#' AL2014_fstadv, the products variable must be modified with discus and fstadv
#' removed. Otherwise, you're just being redundant.

## ---- Libraries --------------------------------------------------------------
library(assertr)
library(dplyr)
library(magrittr)
library(purrr)
library(readr)
library(rrricanes)
library(tibble)

## ---- Variables --------------------------------------------------------------
# Save dataframe to global env?
s <- TRUE
# Where is data being saved? Add trailing slash
d <- "./datasets/"
# Basins to retrieve
basins <- c("AL", "EP")
# Years to retrieve
years <- 1998:as.numeric(strftime(Sys.Date(), "%Y"))
# All products to obtain
products <- c("discus", "fstadv", "posest", "prblty", "public", "update",
              "wndprb")
# Get specific storm (by storm number for the year). Either handles all storms
# with a NULL setting or one storm at a time (n <- 1 or n <- 2. n <- 1:2 will
# not work).
n <- NULL
# Set rrricanes.working_msg
opt.msg <- getOption("rrricanes.working_msg")
options(rrricanes.working_msg = TRUE)

## ---- Functionality ----------------------------------------------------------
# Walk through years
walk(years, .f = function(x) {
    # Check if year directory exists; if not, create it
    if (!dir.exists(sprintf("%s%s", d, x)))
        dir.create(sprintf("%s%s", d, x))
    # Walk through each basin for current year
    walk2(.x = x, .y = basins, .f = function(x, y) {
        # Check if basin directory exists; if not, create it
        if (!dir.exists(sprintf("%s%s/%s", d, x, y)))
            dir.create(sprintf("%s%s/%s", d, x, y))
        # Load dataframe of storms for current year
        storms <- get_storms(year = x, basin = y)
        # Walk through each product for current basin, current year
        pwalk(.l = list(year = x, basin = y, product = products),
              .f = function(basin, year, product) {
                  # If requesting a specific storm, get data and save.
                  if (!is.null(n)) {
                      df <- sprintf("get_%s", product) %>%
                          invoke_map_df(.x = storms %>% slice(n) %>% .$Link)
                      if (nrow(df) > 0) {
                          # Storm key. Strike probabilities products did not
                          # have a Key value; only name. So, if the product is
                          # `prblty`, build Key based off basin, n, and year. It
                          # is possible this may be wrong (it shouldn't be). And
                          # the only way to verify would be pulling another
                          # product or pulling the storm summary which sort of
                          # negates the purpose of what we're doing here. Any
                          # discrepancies that are created because of this will
                          # just have to be dealt with another way.
                          if (product == "prblty") {
                              k <- sprintf("%s%s%s", basin,
                                           # Pad to two digits, if necessary
                                           stringr::str_pad(n, 2, side = "left",
                                                            pad = 0),
                                           year)
                          } else {
                              # Otherwise, we can just select Key from the first row
                              k <- df %>% slice(1) %>% .$Key
                          }
                          df.name <- sprintf("%s_%s", k, product)
                          # Bring to global if s is TRUE
                          if (s) assign(df.name, df, envir = .GlobalEnv)
                          key_dir <- sprintf("%s/%s/%s/%s", d, year, basin, k)
                          if (!dir.exists(key_dir)) dir.create(key_dir)
                          if (s) assign(df.name, df, envir = .GlobalEnv)
                          write_csv(df, path = sprintf("%s/%s.csv", key_dir, df.name))
                      }
                  } else {
                      # Otherwise, get all storms for current year
                      df <- sprintf("get_%s", product) %>%
                          invoke_map_df(.x = storms %>% .$Link)
                      # Ignore if no product data available
                      if (nrow(df) > 0) {
                          # If product is fstadv, build a summary dataframe and
                          # save/bring to global env
                          if (product == "fstadv") {
                              summary <- df %>%
                                  group_by(Key) %>%
                                  mutate(StartDate = min(Date),
                                         EndDate = max(Date)) %>%
                                  select(Key, Name, Wind, StartDate, EndDate) %>%
                                  arrange(desc(Wind)) %>%
                                  filter(row_number() == 1) %>%
                                  arrange(Key)
                              # Name of summary dataframe (i.e., AL1998)
                              df.name <- sprintf("%s%s", basin, year)
                              # Bring to global environment, if TRUE
                              if (s) assign(df.name, summary, envir = .GlobalEnv)
                              # Write summary dataframe
                              write_csv(summary,
                                        path = sprintf("%s/%s/%s/%s.csv",
                                                       d, year, basin, df.name))
                          }
                          # Name of product dataframe (i.e., AL1998_fstadv)
                          df.name <- sprintf("%s%s_%s", basin, year, product)
                          # Bring to global environment, if TRUE
                          if (s) assign(df.name, df, envir = .GlobalEnv)
                          # Save product dataframe
                          write_csv(df, path = sprintf("%s/%s/%s/%s.csv",
                                                       d, year, basin, df.name))
                      }
                  }
              })
        })
    })

# Reset rrricanes.working_msg
options(rrricanes.working_msg = opt.msg)
