
#' @title  Get Serial Numbers
#' @description Creates the serial numbers look up
#'
#' This will create a fresh table for serial numbers
#' Since this is constantly updated it should be
#' refreshed regularly especially when seeking recent
#' tracks.
#' @export

get_serial_numbers <- function() {
  lines <- readLines("https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/csv/")
  currentid <- substring(lines[12], 58, 65)
  #today <- gsub("-", "", Sys.Date())
  serial_raw <-
    file(paste0(
      "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/csv/IBTrACS_SerialNumber_NameMapping_v04r00_",
    currentid,".txt"))
  open(serial_raw)
  serial_numbers <-  iotools::dstrfw(stringr::str_pad(readLines(serial_raw),
                                             210, "right"),
                            col_types = c(sid = "character",
                                          id ="character",
                                          name_history = "character"),
                            widths = c(13, 11, 186),
                            strict = FALSE)
   close(serial_raw)
   serial_numbers <- purrr::map_df(serial_numbers, .f = trimws)
   save(serial_numbers,
        file = file.path( find.package("rrricanes"), "data/serial_numbers.rda"))
    data("serial_numbers", package = "rrricanes")
}

#' @title Get Serial Numbers for Basin ID
#'
#' @description Extract serial numbers for a basin
#'
#' @param basin_id  The basin id
#'
#' @keywords internal
serial_from_basin_id <- function(basin_id) {
  if (!exists("serial_numbers")){
    get_serial_numbers()
  }
  row.names(serial_numbers[basin_id == serial_numbers$basin_id,])

}

#' @title Get serial number by storm name
#'
#' @description  Get IDs for a named storm
#' @param  name  Name of the storm
#'
#' @return A character vector of storm IDs.
#' @keywords internal
serial_from_name <- function(name, basin) {
  if (!exists("serial_numbers")){
    get_serial_numbers()
  }
  sids <- serial_numbers[grepl(pattern = toupper(name),
                                 x = toupper(serial_numbers$name_history),
                                fixed = TRUE), ]
  sids <- sids |>
  dplyr::filter(tolower(substr(id, 2, 3)) == tolower(basin)) |>
  dplyr::pull( sid)
}


#'@title get_storm_track
#'
#' @param serials vector of serial numbers for a storm
#' @param source  Short name for source, allows use of smaller file.
#'
#' @return data frame of storm track

get_storm_track <- function(serials,
                    source = c("ACTIVE", "last3years", "since1980", "ALL",
                               "EP", "NA", "NI", "SA", "SI",
                               "SP", "WP")){
    # The headers are two lines so we need a workaround
     cn <- tolower(c("SID", "SEASON", "NUMBER", "BASIN", "SUBBASIN", "NAME", "ISO_TIME",
        "NATURE", "LAT", "LON", "WMO_WIND", "WMO_PRES", "WMO_AGENCY",
        "TRACK_TYPE", "DIST2LAND", "LANDFALL", "IFLAG"))
     grepstring <- paste(serials, collapse = "|^")

    source <- match.arg(source)
    con1 <- file(paste0(
     "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/csv/ibtracs.",
     source,
     ".list.v04r00.csv"), "rb")

    track_data <- iotools::read.csv.raw(con1,
          header = FALSE,
          strict = FALSE,
          skip = 2,
          colClasses = c("character", "integer", "integer",
                         "character", "character",
                         "POSIXct", "character",
                         "numeric", "numeric",
                         "character", "character",
                         "integer", "character",
                         "character", "integer",
                         "integer", "character" ,
                         rep(NULL, 146)
                         )
    )
 #track_data
    track_data <- track_data[grepstring,] |>
        filter(V1 %in% serials)
    if (isOpen(con1)) {close(con1)}
    colnames(track_data) <- cn
    track_data
}
