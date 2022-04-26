#'----------------------------------------------------------------------------'#
#'                                                                            '#
#' Update datasets                                                            '#
#'                                                                            '#
#'----------------------------------------------------------------------------'#
#'
#' Update datasets with missing advisories. Currently expects that given a key
#' looks for products with datetimes after the highest datetime value in the
#' dataset. If an advisory is missing but later advisories issued are present,
#' this script will not work.
#'
#' It can handle multiple keys.
#'
#' Consider it a brute-force script.
#'
#' Execution:
#' Rscript ~/Projects/rrricanes/data-raw/update_datasets.R <key> &>> ~/Projects/rrricanes/data-raw/log.txt

cat("\n\n###############################################################\n\n\n")
Sys.time()

## ---- Libraries --------------------------------------------------------------
library(dplyr)
library(purrr)
library(readr)
library(rrricanes)
library(stringr)

## ---- Options ----------------------------------------------------------------
options(scipen = 999)
opts.working_msg <- getOption("rrricanes.working_msg")
options("rrricanes.working_msg" = TRUE)

# Change working directory
wd <- getwd()
setwd("~/Projects/rrricanes")

products <- c("discus", "fstadv", "posest", "public", "prblty", "update",
              "wndprb")

keys <- commandArgs(trailingOnly = TRUE)

if (purrr::is_empty(keys))
    stop("No storm keys provided.")

walk(keys, .f = function(key) {
    walk(products, .f = function(product) {

        # Extract command line arguments
        basin <- str_sub(key, 0L, 2L)
        year_num <- str_sub(key, 3L, 4L) |> as.numeric()
        year <- str_sub(key, 5L, 8L) |> as.numeric()

        # Get storm
        storm_link <- get_storms(year = year, basin = basin) |>
            slice(year_num) |>
            .$Link

        # Get product for current cyclone
        func <- get_storm_data
        func_call <- safely(func)
        ret <- func_call(storm_link, products = product)

        if (!is.null(ret$error)) {
            warning(sprintf("%s\n%s", ret$error, link))
            return(NULL)
        }

        # If no results, exit
        if (purrr::is_empty(ret$result[[product]]))
            return(NULL)

        # Read in full dataset
        full_df <- read_csv(sprintf("./datasets/%s.csv", product))

        max_date <- full_df |> dplyr::filter(Key == key) |> .$Date |> max()

        filtered_df <- ret$result[[product]] |> dplyr::filter(Date > max_date)

        df <- bind_rows(full_df, filtered_df)

        write_csv(df, sprintf("./datasets/%s.csv", product))

    })

})

## ---- Reset Options ----------------------------------------------------------
options(scipen = 0)
options("rrricanes.working_msg" = opts.working_msg)
# Reset working directory
setwd(wd)
