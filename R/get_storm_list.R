#' @title get_storm_list
#' @description Get storm list
get_storm_list <- function() {
  storm_list <- readr::read_csv("ftp://ftp.nhc.noaa.gov/atcf/index/storm_list.txt",
                                col_names = c("STORM_NAME", "RE", "X", "R2", "R3", "R4", "R5", "CY", "YYYY", "TY", "I", "YYY1MMDDHH",
                                              "YYY2MMDDHH", "SIZE", "GENESIS_NUM", "PAR1", "PAR2", "PRIORITY", "STORM_STATE", "WT_NUMBER",
                                              "STORMID"),
                                col_types = "ccccccciiccccciccicic") %>%
    mutate_at(c("YYY1MMDDHH", "YYY2MMDDHH"), as.POSIXct, format = "%Y%m%d%H", tz = "UTC")
}
