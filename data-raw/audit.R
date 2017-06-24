#' ########################################################################## '#
#' Audit
#'
#' Examine all datasets for quality issues. This will primarily be beneficial
#' during cleanup phases but should probably be operated on a weekly basis to
#' ensure integrity.
#'
#' ########################################################################## '#

## ---- Libraries --------------------------------------------------------------
library(dplyr)
library(Hmisc)
library(purrr)
library(readr)
library(rrricanes)

## ---- Options ----------------------------------------------------------------

## ---- Variables --------------------------------------------------------------
valid_options <- c("Hurricane", "Post-Tropical Cyclone",
                   "Potential Tropical Cyclone", "Remnants of",
                   "Subtropical Depression", "Tropical Depression",
                   "Tropical Storm")

invalid_names <- c("Center", "Test")

#' ########################################################################## '#
## ---- Storm Discussions ------------------------------------------------------
#' The `discus` product should only parse `status`, `name`, `adv`, `date` and
#' `contents` of the product.
discus <- read_csv("./datasets/discus.csv", col_types = cols())

## ---- * Status ---------------------------------------------------------------
status <- sort(unique(discus$Status))
if (!(any(status %in% valid_options)))
    warning(sprintf("Invalid Status in discus:\n%s",
                    sort(unique(discus$Status[!(discus$Status %in% valid_options)]))))

## ---- * Names ----------------------------------------------------------------
#' * discus$Name for EP192000 Adv 1 is "Test". This is an err on the NHC's part.
discus_names <- sort(unique(discus$Name))
if (any(discus_names %in% invalid_names)) {
    warning("Invalid Names in discus. See object df_discus_invalid_names.")
    df_discus_invalid_names <- discus %>% filter(Name %in% invalid_names)
}

## ---- * Adv ------------------------------------------------------------------
describe(discus$Adv)

## ---- * Date -----------------------------------------------------------------
describe(discus$Date)

#' ########################################################################## '#
## ---- Forecast/Advisory ------------------------------------------------------
fstadv <- read_csv("./datasets/fstadv.csv", col_types = cols(),
                   guess_max = 13000)

## ---- * Status ---------------------------------------------------------------
status <- sort(unique(fstadv$Status))
if (!(any(status %in% valid_options)))
    warning(sprintf("Invalid Status in fstadv:\n%s",
                    sort(unique(fstadv$Status[!(fstadv$Status %in% valid_options)]))))

## ---- * Names ----------------------------------------------------------------
fstadv_names <- sort(unique(fstadv$Name))
if (any(fstadv_names %in% invalid_names)) {
    warning("Invalid Names in fstadv. See object df_fstadv_invalid_names.")
    df_fstadv_invalid_names <- fstadv %>% filter(Name %in% invalid_names)
}

## ---- * Date -----------------------------------------------------------------
describe(fstadv$Date)

## ---- * Key ------------------------------------------------------------------
if (any(grepl("^[AL|EP][[:digit:]]{6}$", fstadv$Key)))
    warning("Invalid Keys")

## ---- * Lat ------------------------------------------------------------------
#' There may be NA values in Lat if a system is no longer tropical or has a
#' closed center.
fstadv_lat <- select(fstadv, dplyr::contains("Lat"))

if (!every(fstadv_lat, is.double))
    warning("Non-numeric lat values")

if (min(fstadv_lat, na.rm = TRUE) < 0 | max(fstadv_lat, na.rm = TRUE) > 90)
    warning("Possible invalid latitude values")

## ---- * Lon ------------------------------------------------------------------
#' There may be NA values in Lon if a system is no longer tropical or has a
#' closed center.
fstadv_lon <- select(fstadv, dplyr::contains("Lon"))

if (!every(fstadv_lon, is.double))
    warning("Non-numeric lon values")

if (min(fstadv_lon, na.rm = TRUE) < -180 | max(fstadv_lon, na.rm = TRUE) > 180)
    warning("Possible invalid longitude values")

## ---- * Wind, Gust -----------------------------------------------------------
fstadv_winds <- select(fstadv, ends_with("Wind"), ends_with("Gust"))

if (!every(fstadv_winds, is.numeric))
    warning("Non-numeric wind/gust values")

## ---- * Pressure -------------------------------------------------------------
if (!is.numeric(fstadv$Pressure))
    warning("Non-numeric values in Pressure")

## ---- * PosAcc ---------------------------------------------------------------
if (!is.numeric(fstadv$PosAcc))
    warning("Non-numeric values in PosAcc")

## ---- * FwdDir ---------------------------------------------------------------
if (!is.numeric(fstadv$FwdDir))
    warning("Non-numeric values in FwdDir")

## ---- * FwdDir ---------------------------------------------------------------
if (!is.numeric(fstadv$FwdDir))
    warning("Non-numeric values in FwdDir")

## ---- * FwdSpeed -------------------------------------------------------------
if (!is.numeric(fstadv$FwdSpeed))
    warning("Non-numeric values in FwdSpeed")

## ---- * Eye ------------------------------------------------------------------
if (!is.numeric(fstadv$Eye))
    warning("Non-numeric values in Eye")

## ---- * Wind Radii -----------------------------------------------------------
#' Convert to numeric, if not, which sould generate error if non-numeric values
#' exist.
fstadv_wr <- fstadv %>% select(matches(".[NE|SE|SW|NW][34|50|64].")) %>%
    map_df(as.numeric)

#' Forecast hours 48 and 72 should not have 64kt wind radius fields
is_empty(select(fstadv, Hr48NE64:Hr48NW64, Hr72NE64:Hr72NW64) %>% describe())

#' Forecast hours 96 and 120 should not have any wind radius fields
is_empty(select(fstadv, Hr96NE64:Hr96NW34, Hr120NE64:Hr120NW34) %>% describe())

#' ########################################################################## '#
## ---- Position Estimates -----------------------------------------------------
posest <- read_csv("./datasets/posest.csv", col_types = cols())

## ---- * Status ---------------------------------------------------------------
status <- sort(unique(posest$Status))
if (!(any(status %in% valid_options)))
    warning(sprintf("Invalid Status in posest:\n%s",
                    sort(unique(posest$Status[!(posest$Status %in% valid_options)]))))

## ---- * Names ----------------------------------------------------------------
posest_names <- sort(unique(posest$Name))
if (any(posest_names %in% invalid_names)) {
    warning("Invalid Names in posest. See object df_posest_invalid_names.")
    df_posest_invalid_names <- posest %>% filter(Name %in% invalid_names)
}

## ---- * Date -----------------------------------------------------------------
describe(posest$Date)

#' ########################################################################## '#
## ---- Strike Probabilities ---------------------------------------------------
prblty <- read_csv("./datasets/prblty.csv", col_types = cols())

## ---- * Status ---------------------------------------------------------------
status <- sort(unique(prblty$Status))
if (!(any(status %in% valid_options)))
    warning(sprintf("Invalid Status in prblty:\n%s",
                    sort(unique(prblty$Status[!(prblty$Status %in% valid_options)]))))

## ---- * Names ----------------------------------------------------------------
#' * prblty$Name for AL161999 Adv 1 is "Test". This is an err on the NHC's part.
prblty_names <- sort(unique(prblty$Name))
if (any(prblty_names %in% invalid_names)) {
    warning("Invalid Names in prblty. See object df_prblty_invalid_names.")
    df_prblty_invalid_names <- prblty %>% filter(Name %in% invalid_names)
}

## ---- * Wind:Wind120Cum ------------------------------------------------------
if (!every(prblty %>% select(A:E), is.numeric))
    warning("Some probability cols not numeric.")

#' ########################################################################## '#
## ---- Public Advisories ------------------------------------------------------
public <- read_csv("./datasets/public.csv", col_types = cols())

## ---- * Status ---------------------------------------------------------------
status <- sort(unique(public$Status))
if (!(any(status %in% valid_options)))
    warning(sprintf("Invalid Status in public:\n%s",
                    sort(unique(public$Status[!(public$Status %in% valid_options)]))))

## ---- * Names ----------------------------------------------------------------
public_names <- sort(unique(public$Name))
if (any(public_names %in% invalid_names)) {
    warning("Invalid Names in public. See object df_public_invalid_names.")
    df_public_invalid_names <- public %>% filter(Name %in% invalid_names)
}

## ---- * Adv ------------------------------------------------------------------
describe(public$Adv)
if (any(is.na(public$Adv))) {
    warning("Adv contains NA values.")
    df_public_invalid_adv <- public %>% filter(is.na(Adv))
}

## ---- * Date -----------------------------------------------------------------
describe(public$Date)

#' ########################################################################## '#
## ---- Updates ----------------------------------------------------------------
update <- read_csv("./datasets/update.csv", col_types = cols())

## ---- * Status ---------------------------------------------------------------
status <- sort(unique(update$Status))
if (!(any(status %in% valid_options)))
    warning(sprintf("Invalid Status in update:\n%s",
                    sort(unique(update$Status[!(update$Status %in% valid_options)]))))

## ---- * Names ----------------------------------------------------------------
update_names <- sort(unique(update$Name))
if (any(update_names %in% invalid_names)) {
    warning("Invalid Names in update. See object df_update_invalid_names.")
    df_update_invalid_names <- update %>% filter(Name %in% invalid_names)
}

#' ########################################################################## '#
## ---- Wind speed Probabilities -----------------------------------------------
wndprb <- read_csv("./datasets/wndprb.csv", col_types = cols())

## ---- * Key ------------------------------------------------------------------
if (any(grepl("^[AL|EP][[:digit:]]{6}$", wndprb$Key)))
    warning("Invalid Keys")

## ---- * Adv ------------------------------------------------------------------
describe(wndprb$Adv)
if (any(is.na(wndprb$Adv))) {
    warning("Adv contains NA values.")
    df_wndprb_invalid_adv <- wndprb %>% filter(is.na(Adv))
}

## ---- * Date -----------------------------------------------------------------
describe(wndprb$Date)

## ---- * Wind:Wind120Cum ------------------------------------------------------
if (!every(wndprb %>% select(Wind:Wind120Cum), is.numeric))
    warning("Some Wind cols not numeric.")
