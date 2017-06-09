## ---- Variables --------------------------------------------------------------
#' ------------------------- Edit this section only ------------------------- '#

#'Key must be [:upper:]{2}[:digit:]{6} format; i.e. AABBCCCC where AA is basin
#'("AL" or "EP"), BB is number of the storm for the year and CCCC is the year.
key <- "EP022017"

#'Set adv if looking for a specific advisory
adv <- NULL

#' Chart resolution. 110nm, 50nm or 10nm. Higher resolution = slower to process
chart_res <- 50

#' Span of lat and lon in degrees
#' I'd like to figure out a way to calculate this based of all the data points.
lat_bounds <- 15
lon_bounds <- 30

#' -------------------------- Do not edit below ----------------------------- '#

## ---- Libraries --------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(knitr)
library(purrr)
library(rrricanes)
library(tidyr)

## ---- Variable Validation ----------------------------------------------------
if (!grepl("AL|EP[[:digit:]{6}]", key))
    stop("Key not properly formatted.")

if (!chart_res %in% c(10, 50, 110))
    stop("Chart resolution must be 10, 50 or 110")

## ---- Data -------------------------------------------------------------------
key <- stringr::str_match(key, "([:upper:]{2})([:digit:]{2})([:digit:]{4})")
storm_vars <- list(key = key[[1]],
                   basin = key[[2]],
                   year_num = as.integer(key[[3]]),
                   year = as.integer(key[[4]]))

## ---- * Get storms -----------------------------------------------------------
storms <- get_storms(year = storm_vars$year, basin = storm_vars$basin)

#'Storm name
#'Must reference year_num with [[".
#'See https://github.com/tidyverse/dplyr/issues/1400
#'Seems corrected in dplyr 0.5.0.9000
storm_vars$name <- storms %>% slice(storm_vars[["year_num"]]) %>% .$Name

## ---- * Get fstadv -----------------------------------------------------------
fstadv <- storms %>% slice(storm_vars[["year_num"]]) %>% .$Link %>% get_fstadv()
#'Set adv
storm_vars$adv <- as.numeric(last(fstadv$Adv))
#'Set date
storm_vars$date <- last(fstadv$Date)

## ---- * * Gather Forecasts ---------------------------------------------------
v <- c("FcstDate", "Lat", "Lon", "Wind", "Gust")
fcst <- map_df(.x = c(12, 24, 36, 48, 72, 96, 120),
                  .f = function(y) {
                      select_(fstadv,
                              .dots = c("Key", "Adv", "Date", paste0("Hr", y, v))) %>%
                          rename_("Key" = "Key", "Adv" = "Adv", "Date" = "Date",
                                  "FcstDate" = paste0("Hr", y, "FcstDate"),
                                  "Lat" = paste0("Hr", y, "Lat"),
                                  "Lon" = paste0("Hr", y, "Lon"),
                                  "Wind" = paste0("Hr", y, "Wind"),
                                  "Gust" = paste0("Hr", y, "Gust"))}) %>%
    arrange_("Key", "Date", "Adv", "FcstDate") %>%
    mutate(Category = if_else(Wind <= 33, "TD", "TS")) %>%
    filter(Adv == storm_vars$adv, !is.na(FcstDate)) %>%
    select(FcstDate, Lat, Lon, Wind, Gust, Category)

## ---- * Get discus -----------------------------------------------------------
discus <- storms %>%
    slice(storm_vars[["year_num"]]) %>%
    .$Link %>%
    get_discus() %>%
    filter(Adv == storm_vars$adv)

## ---- * Get public -----------------------------------------------------------
public <- storms %>%
    slice(storm_vars[["year_num"]]) %>%
    .$Link %>%
    get_public() %>%
    filter(Adv == storm_vars$adv)

#'Clean up public$Contents
pub_adv <- stringr::str_extract(public$Contents,
                                paste0("(?<=[:upper:]{3} [:alpha:]{3} ",
                                       "[:alpha:]{3} [:digit:]{1,2} ",
                                       "[:digit:]{4}\n\n)([[:punct:][:alnum:]",
                                       "[:space:]]+)")) %>%
    stringr::str_replace_all("\n[-]+\n", "\n\n")

## ---- * Get wndprb -----------------------------------------------------------
wndprb <- storms %>%
    slice(storm_vars[["year_num"]]) %>%
    .$Link %>%
    get_wndprb() %>%
    filter(Adv == storm_vars$adv)

## ---- * GIS ------------------------------------------------------------------
#'The biggest weakness with the GIS data right now is the latest XML file
#'is unpredictable. On one refresh you might get two or three datasets. A
#'refresh seconds later gets you ten. At this point in time I'm unsure how to
#'handle this. The alternative is calling the individual GIS functions (which
#'aren't currently enabled).

message("Uncomment GIS when not testing")
#gis <- gis_latest(basins = basin)
#'This dataset was saved earlier.
load(file = "~/Downloads/gis.Rda")

#'Bring GIS datasets to Global with new names. These probably can be renamed in
#'the package but I'm somewhat against modifying original data without user-
#'consent.

#'Dataset name patterns
ptn_wsp34_half <- "[:digit:]{10}_wsp34knt120hr_halfDeg"
ptn_wsp50_half <- "[:digit:]{10}_wsp50knt120hr_halfDeg"
ptn_wsp64_half <- "[:digit:]{10}_wsp64knt120hr_halfDeg"
ptn_fcst_lin <- "[:alpha:]{2}[:digit:]{6}_[:digit:]{1,3}_.+_lin"
ptn_fcst_pgn <- "[:alpha:]{2}[:digit:]{6}_[:digit:]{1,3}_.+_pgn"
ptn_fcst_pts <- "[:alpha:]{2}[:digit:]{6}_[:digit:]{1,3}_.+_pts"
ptn_storm_ww <- "[:alpha:]{2}[:digit:]{6}_[:digit:]{1,3}_ww_wwlin"
ptn_storm_lin <- "[:alpha:]{2}[:digit:]{6}_lin"
ptn_storm_pts <- "[:alpha:]{2}[:digit:]{6}_pts"
ptn_storm_radii <- "[:alpha:]{2}[:digit:]{6}_radii"
ptn_windswath <- "[:alpha:]{2}[:digit:]{6}_windswath"

names(gis[[1]]) <- names(gis[[1]]) %>%
    stringr::str_replace_all(pattern = ptn_wsp34_half, "wsp34_half") %>%
    stringr::str_replace_all(pattern = ptn_wsp50_half, "wsp50_half") %>%
    stringr::str_replace_all(pattern = ptn_wsp64_half, "wsp64_half") %>%
    stringr::str_replace_all(pattern = ptn_fcst_lin, "fcst_lin") %>%
    stringr::str_replace_all(pattern = ptn_fcst_pgn, "fcst_pgn") %>%
    stringr::str_replace_all(pattern = ptn_fcst_pts, "fcst_pts") %>%
    stringr::str_replace_all(pattern = ptn_storm_ww, "storm_ww") %>%
    stringr::str_replace_all(pattern = ptn_storm_lin, "storm_lin") %>%
    stringr::str_replace_all(pattern = ptn_storm_pts, "storm_pts") %>%
    stringr::str_replace_all(pattern = ptn_storm_radii, "storm_radii") %>%
    stringr::str_replace_all(pattern = ptn_windswath, "windswath")
invisible(list2env(gis[[1]], envir = .GlobalEnv))
rm(gis)

## ---- * * storm_pts ----------------------------------------------------------
#'Combine the date variables to valid POSIXct, rename INTENSITY.x (Wind), MLSP
#'(Pressure)
storm_pts <- mutate(storm_pts, Status = status_abbr_to_str(STORMTYPE.x)) %>%
    rename(Lat = LAT.x, Lon = LON.x, Wind = INTENSITY.x, Pressure = MSLP.x) %>%
    mutate(Date = lubridate::ymd_hm(sprintf("%s-%s-%s %s:%s", YEAR.x, MONTH.x,
                                            DAY.x,
                                            stringr::str_sub(HHMM.x, 0L, 2L),
                                            stringr::str_sub(HHMM.x, 3L, 4L))))

## ---- * * Wind and Pressure --------------------------------------------------
#'Build wind and pressure dataframe
wind_press <- storm_pts %>%
    select(Date, Wind, Pressure, Status)

#'Show cumulative distribution between Wind, Pressure
wind_press_cum <- wind_press %>%
    mutate(WindDist = (Wind - min(Wind))/(max(Wind) - min(Wind)),
           PressDist = (Pressure - max(Pressure))/(max(Pressure) - min(Pressure))) %>%
    gather(Var, Val, WindDist, PressDist)

## ---- * * watches/warnings ---------------------------------------------------
storm_ww <- storm_ww %>% mutate_at(.cols = vars(TCWW), .funs = function(x) {
    if_else(x == "TWA", "Tropical Storm Watch",
            if_else(x == "TWR", "Tropical Storm Warning",
                    if_else(x == "HWA", "Hurricane Watch",
                            if_else(x == "HWR", "Hurricane Warning", "NA"))))})

## ---- Base Plot --------------------------------------------------------------
#'Base plot

#' Color schemes (to be developed)

#' Watches/Warnings
tcww_colors <- c("yellow", "blue", "magenta", "red")
#' Status
my_col_scheme <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00",
                   "#7B8A7B", "#0B29D6", "#f781bf", "#999999", "black")

#'Set bounds of chart
lat_min <- fstadv %>% filter(Adv == storm_vars$adv) %>% .$Lat - lat_bounds/2
lat_max <- fstadv %>% filter(Adv == storm_vars$adv) %>% .$Lat + lat_bounds/2
lon_min <- fstadv %>% filter(Adv == storm_vars$adv) %>% .$Lon - lon_bounds/2
lon_max <- fstadv %>% filter(Adv == storm_vars$adv) %>% .$Lon + lon_bounds/2

bp <- sprintf("%s_tracking_chart", stringr::str_to_lower(storm_vars$basin)) %>%
    purrr::invoke(.x = list(res = chart_res, color = "black", size = 0.1, fill = "white")) +
    ggplot2::coord_equal(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    labs(x = "Lon",
         y = "Lat",
         caption = sprintf("rrricanes %s", packageVersion("rrricanes")))

## ---- Builds Dirs ------------------------------------------------------------
#'Build directories to store report, if do not exist.
#'dir year
if (!dir.exists(sprintf("./reports/%s", storm_vars$year)))
    dir.create(sprintf("./reports/%s", storm_vars$year))

#'dir year/key
if (!dir.exists(sprintf("./reports/%s/%s", storm_vars$year, storm_vars$key)))
    dir.create(sprintf("./reports/%s/%s", storm_vars$year, storm_vars$key))

## ---- Render Report ----------------------------------------------------------
rmarkdown::render("./reports/_default.Rmd",
                  output_file = sprintf("%s/%s.html",
                                        storm_vars$year,
                                        storm_vars$key),
                  params = list(title = storm_vars$name))
