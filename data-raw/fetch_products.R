#' Fetch Products
#'
#' This script is designed to run the latest version of Hurricanes and grab
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
#'
################################################################################
#'
#' DO NOT PUSH MODIFICATIONS OF THIS SCRIPT TO GITHUB.
#'
################################################################################

## ---- Libraries --------------------------------------------------------------
library(assertr)
library(dplyr)
library(Hurricanes)
library(magrittr)
library(purrr)
library(readr)
library(tibble)

## ---- Variables --------------------------------------------------------------
# Save dataframe to global env?
s <- TRUE
d <- "~/Projects/datasets/Hurricanes/"
basins <- c("AL", "EP")
years <- 1998:as.numeric(strftime(Sys.Date(), "%Y"))
products <- c("discus", "fstadv", "posest", "prblty", "public", "update", "wndprb")

## ---- Functionality ----------------------------------------------------------
# Walk through years
walk(years, .f = function(x) {
    # Would through each basin for current year
    walk2(.x = x, .y = basins, .f = function(x, y) {
        # Load dataframe of storms for current year
        storms <- get_storms(year = x, basin = y)
        # Walk through each product for current basin, current year
        pwalk(.l = list(year = x, basin = y, product = products),
              .f = function(basin, year, product) {
                  # Load products for every storm of year, basin
                  df <- paste("get", product, sep = "_") %>%
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
                          n <- sprintf("%s%s", basin, year)
                          # Bring to global environment, if TRUE
                          if (s) assign(n, summary, envir = .GlobalEnv)
                          # Write summary dataframe
                          write_csv(summary, path = sprintf("%s/%s.csv", d, n))
                      }
                      # Name of product dataframe (i.e., AL1998_fstadv)
                      n <- sprintf("%s%s_%s", basin, year, product)
                      # Bring to global environment, if TRUE
                      if (s) assign(n, df, envir = .GlobalEnv)
                      # Save product dataframe
                      write_csv(df, path = sprintf("%s/%s.csv", d, n))
                  }
              })
        })
    })
