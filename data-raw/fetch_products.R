#'----------------------------------------------------------------------------'#
#'                                                                            '#
#' Fetch Products                                                             '#
#'                                                                            '#
#'----------------------------------------------------------------------------'#
#'
#' Scrape basin index xml files for active storms. If no storms are present,
#' exit script.
#'
#' If storms do exist, extract wallet ID for each storm.
#'
#' In file pub_dates.csv exists each wallet id for each basin along with the
#' last published date processed. If each product in each active wallet has a
#' pubDate greater than the value in pub_dates.csv, parse product, add to
#' product dataframe and push to GitHub repo.
#'
#' This script need not run on a consistent basis but, for the time being, will
#' have to be micromanaged somewhat. If there is no possibility of a storm
#' developing within the next 24-48 hours, then no need to run.
#'
#' However, if there is, say, >50% chance of development then the script should
#' probably run every 10-15 minutes.
#'
#' Execution:
#' R CMD BATCH R/fetch_products.R

## ---- Libraries --------------------------------------------------------------
library(dplyr)
library(git2r)
library(purrr)
library(readr)
library(rrricanes)
library(rvest)
library(sendmailR)
library(stringr)
library(tibble)
library(xml2)

## ---- Options ----------------------------------------------------------------
opts.working_msg <- getOption("rrricanes.working_msg")
options("rrricanes.working_msg" = TRUE)

# Change working directory
wd <- getwd()
setwd("~/Projects/rrricanes")

# GitHub
repo <- repository("./datasets")

## ---- Functions --------------------------------------------------------------
parse_products <- function(x, y) {

    # Load last published dates
    pub_dates <- read_csv("./datasets/pub_dates.csv", col_types = cols())

    # Get title of product
    title <- xml_find_all(y, "title") |> xml_text()

    # Get pubdate of products
    pd <- xml_find_all(y, "pubDate") |> xml_text() |>
        strptime(format = "%a, %d %b %Y %T", tz = "UTC")

    # Set product and column types of existing dataset. Remember Adv is char.
    if (str_detect(title, "Public Advisory")) {
        product = "public"
        col_types = "cccTc"
    } else if (str_detect(title, "Forecast Advisory")) {
        product = "fstadv"
        col_types = paste0("cccTcddiiniiiiiiiiiiiiiiii",
                           # 12 hrs
                           "Tddiiiiiiiiiiiiii",
                           # 24 hrs
                           "Tddiiiiiiiiiiiiii",
                           # 36 hrs
                           "Tddiiiiiiiiiiiiii",
                           # 48 hrs
                           "Tddiiiiiiiiiiiiii",
                           # 72 hrs
                           "Tddiiiiiiiiiiiiii",
                           # Seas
                           "iiii",
                           # 96 hrs
                           "Tddiiiiiiiiiiiiii",
                           #120 hrs
                           "Tddiiiiiiiiiiiiii")
    } else if (str_detect(title, "Forecast Discussion")) {
        product = "discus"
        col_types = "cccTc"
    } else if (str_detect(title, "Wind Speed Probabilities")) {
        product = "wndprb"
        col_types = "ccTciiiiiiiiiiiiii"
    } else if (str_detect(title, "Tropical Cyclone Update")) {
        product = "update"
        col_types = "cccTc"
    }

    last_update <- pub_dates |>
        filter(Product == product) |>
        .[[x]] |>
        as.POSIXct()

    if (pd <= last_update) return(NULL)

    # At this point we have a new product. Get URL to product and scrape
    link <- xml_find_all(y, "link") |> xml_text()

    func <- getAnywhere(product)$objs[[1]]

    func_call <- safely(func)

    ret <- func_call(link)

    if (!is.null(ret$error)) {
        warning(sprintf("%s\n%s", ret$error, link))
        return(NULL)
    }

    if (is.null(ret$result)) {
        # wndprb product may not contain any probabilities making the dataframe
        # empty. Return NULL in this instance.

        # Update pub_dates dataframe and save
        pub_dates[pub_dates$Product == product,][x] <- as.POSIXct(pd)
        write_csv(pub_dates, path = "./datasets/pub_dates.csv")

        add(repo, "./datasets/pub_dates.csv")
        commit(repo, sprintf("pub_dates updated as of %s", pd))
        return(NULL)
    }

    # Import and update latest product datafile
    df <- read_csv(sprintf("./datasets/%s.csv", product), col_types = col_types)

    updated_df <- bind_rows(df, ret$result)
    write_csv(updated_df, path = sprintf("./datasets/%s.csv", product))

    if (product == "fstadv") {
        ## ---- Tidy fstadv ----------------------------------------------------
        adv <- tidy_adv(updated_df)
        write_csv(adv, path = "./datasets/adv.csv")

        fcst <- tidy_fcst(updated_df)
        write_csv(fcst, path = "./datasets/fcst.csv")

        fcst_wr <- tidy_fcst_wr(updated_df)
        write_csv(fcst_wr, path = "./datasets/fcst_wr.csv")

        wr <- tidy_wr(updated_df)
        write_csv(wr, path = "./datasets/wr.csv")
    }

    # Update pub_dates dataframe and save
    pub_dates[pub_dates$Product == product,][x] <- as.POSIXct(pd)
    write_csv(pub_dates, path = "./datasets/pub_dates.csv")

    ## ---- Stage Changes ------------------------------------------------------
    status <- status(repo)
    status$unstaged
    if (!is_empty(status$unstaged)) {
        add(repo, status$unstaged |> flatten() |> sprintf("./datasets/%s", .))
        commit(repo, sprintf("%s updated as of %s", product, pd))
        push(repo)
    }
}

## ---- Scrape Index -----------------------------------------------------------
# Index URLs to scrape for active cyclones
index_urls <- c("http://www.nhc.noaa.gov/index-at.xml",
                "http://www.nhc.noaa.gov/index-ep.xml")

# Load XML as list
indices <- index_urls |> map(read_xml)

# Go through each index output and check for active cyclones. If no storms are
# active then do nothing.

# AL example (active cyclone):
# http://www.nhc.noaa.gov/rss_examples/index-at-20130605.xml
# EP example (active cyclone):
# http://www.nhc.noaa.gov/rss_examples/index-ep.xml

# Based on the examples provided and current indexes (with no active cyclones
# in either basin), there is a nhc:wallet element as a child of nhc::Cyclone.
# The nhc:wallet element contains a string (e.g., AT1) which indicates the basin
# (Atlantic) and wallet number (1). This should tell us there is an active
# cyclone.
wallets <- indices |>
    map(xml_find_all, xpath = ".//nhc:wallet") |>
    map(xml_text) |>
    flatten_chr()

if (is_empty(wallets))
    stop("No active cyclones.")

## ---- Scrape Products --------------------------------------------------------
# At this point we have a vector of wallets that need to be parsed. Inside each
# wallet is an overall pubDate for each product. This pubDate needs to be
# checked against the value in pub_dates.csv to ensure that we do not write
# existing data.
urls <- map_chr(wallets, str_to_lower) |>
    sprintf("http://www.nhc.noaa.gov/nhc_%s.xml", .)

# Extract all items
items <- map(urls, read_xml) |> map(xml_find_all, xpath = ".//item")

# Match the products we need
item_matches <- map(items, str_detect,
                    pattern = paste0("(Public Advisory|Forecast Advisory|",
                                     "Forecast Discussion|",
                                     "Wind Speed Probabilities|",
                                     "Tropical Cyclone Update) ",
                                     "Number"))

# Keep only those matches
items <- map2(items, item_matches, keep)

# Begin parsing products
walk2(wallets, items, walk2, parse_products)

## ---- Reset Options ----------------------------------------------------------
options("rrricanes.working_msg" = opts.working_msg)
# Reset working directory
setwd(wd)

## ---- Send Email -------------------------------------------------------------
from <- "<rrricanes@gmail.com>"
to <- "<rrricanes@gmail.com>"
subject <- "rrricanes fetch products"
body <- read_file("fetch_products.Rout")
mailControl = list(smtpServer = "ASPMX.L.GOOGLE.COM")
sendmail(from = from, to = to, subject = subject, msg = body,
         control = mailControl)
