#' @title get_storm_list
#' @description Get storm list
#' @export
get_storm_list <- function() {

  # 2018-12-29 - On Dec. 9, an update was made putting a newline at the
  # beginning of the  text file. This threw off the original code generating
  # warnings. Instead of reading directly as CSV, read in as character, trim
  # whitespace, then read CSV.

  # Read in file as string
  txt <- readChar(
    "ftp://ftp.nhc.noaa.gov/atcf/index/storm_list.txt",
    nchars = 252928)

  # Remove any trailing white space
  clean_txt <- stringr::str_trim(txt)

  # Return dataframe
  readr::read_csv(
    file = clean_txt,
    col_names = c(
      "STORM_NAME", "RE", "X", "R2", "R3", "R4", "R5", "CY", "YYYY", "TY",
      "I", "YYY1MMDDHH", "YYY2MMDDHH", "SIZE", "GENESIS_NUM", "PAR1", "PAR2",
      "PRIORITY", "STORM_STATE", "WT_NUMBER", "STORMID"
    ),
    col_types = "ccccccciiccccciccicic"
  ) |>
    dplyr::across(
      .cols = c("YYY1MMDDHH", "YYY2MMDDHH"),
      .fns = as.POSIXct,
      format = "%Y%m%d%H",
      tz = "UTC"
    )
}

#' @title get_ftp_dirs()
#' @description Get a list of the FTP directors in /atcf/archive
#' @keywords internal
get_ftp_dirs <- function(x) {
  url <- stringr::str_c(get_nhc_ftp_link(), x)
  con <- curl::curl(url, "r")
  ftp_dirs <-
    con |>
    utils::read.table(stringsAsFactors = FALSE, fill = TRUE) |>
    dplyr::rename(Name = .data$V9) # Name is the link
  close(con)
  ftp_dirs
}

#' @title get_ftp_storm_data
#' @description Retrieve text products from the National Hurricane Center's FTP
#'   server. Not all products may exist for certain storms.
#' @param stormid A six-character alphanumeric string formatted as AABBCCCC
#'   where
#'   \describe{
#'     \item{AA}{The basin of the storm; AL or EP}
#'     \item{BB}{Storm number for the year as decimal number
#'       (e.g., 01, 02, ..., 10, ...)}
#'     \item{CCCC}{Year with century)}
#'   }
#' @inheritParams get_storm_data
#' @export
#' @seealso \code{\link{get_storm_data}}
get_ftp_storm_data <- function(stormid,
                               products = c("discus", "fstadv", "posest",
                                            "public", "prblty", "update",
                                            "wndprb")) {

  if (!grepl("(AL|EP)\\d{6}", stormid))
    stop(
      stringr::str_c(
        "stormid should be an alphanumeric string with the basin abbreviation ",
        "(AL or EP) followed by a two-digit storm number ending with a ",
        "four-digit year, e.g., AL092017",
        .call = FALSE
      )
    )

  # What year is the storm?
  yyyy <- as.integer(stringr::str_match(stormid, "^.+(\\d{4})$")[,2])

  # List all directories in the ftp's archives
  archives <- get_ftp_dirs(x = "/atcf/archive/")

  if (yyyy %in% archives$Name) {
    # If the year is listed in the **atcf/archive** directory then, depending
    # what product we want will determine where we go from there.
    #
    # In the year's root dir, there are four types of packages. Let's say we're
    # working Harvey, 2017 (AL092017). We'll have:
    # - aal092017.dat.gz - this is computer forecast model data
    # - bal092017.dat.gz - best-track data
    # - eal092017.dat.gz - more forecast model data
    # - fal092017.dat.gz - maybe more forecast model data
    #
    # We need to go one level deeper, into **messages**, to get the text
    # products. For 2017, there appear to be duplicates (appended with a
    # timestamp - likely, the date and time (UTC) issued).
    #
    # So, we'll have
    # - discus: al092017.discus.\d\d\d (ignore the timestamped files)
    # - fstadv: al092017.fstadv.\d\d\d (ignore duplicates)
    # - icaoms: al092017.icao.\d\d\d (ignore duplicates; these are aviation
    #   advisories)
    # - public: al092017.public.\d\d\d (ignore duplicates)
    # - public_a: al092017.public_a.\d\d\d (ignore duplicates, intermediate
    #   advisories)
    # - update
    # - wndprb
    #
    ftp_subdir <- sprintf("atcf/archive/%s/messages/", yyyy)
    ftp_contents <- get_ftp_dirs(ftp_subdir)

    # At this point we have a list of ALL messages in the ftp directory. Let's
    # filter out on what we want.
    links <-
      ftp_contents |>
      dplyr::filter(
        grepl(
          pattern =
            sprintf(
              fmt = "^%s\\.%s\\.\\d{3}$",
              stringr::str_to_lower(stormid),
              products
            ),
          x = .data$Name
        )
      ) |>
      dplyr::pull(.data$Name)

    if (purrr::is_empty(links)) {
      # For years previous 1998, text products are wrapped into zip files. In
      # these instances, we'll take a detour. We'll download the zip files to
      # a temp directory, read in the products requested, scrape then do an
      # early return.
      links <-
        ftp_contents |>
        dplyr::filter(
          grepl(
            pattern = sprintf("^%s_msg.zip", stringr::str_to_lower(stormid)),
            x = .data$Name
          )
        ) |>
        dplyr::pull(.data$Name)

      pkg <- sprintf(
        fmt = "ftp://ftp.nhc.noaa.gov/atcf/archive/%s/messages/%s",
        yyyy,
        links
      )

      destdir <- tempdir()
      tmp_file <- tempfile(
        pattern = tools::file_path_sans_ext(links),
        tmpdir = destdir,
        fileext = ".zip"
      )

      res <- utils::download.file(
        url = file.path(pkg),
        destfile = tmp_file
      )

      list_files <- utils::unzip(tmp_file, list = TRUE)$Name

      utils::unzip(tmp_file, exdir = destdir)

      named_products <- list(
        "discus" = "n",
        "fstadv" = "m",
        "public" = "p",
        "prblty" = "l"
      )

      files <- list.files(
        path = destdir,
        pattern = sprintf(
          fmt = "^%s%s%s%s\\.\\d{3}$",
          # What product?
          named_products[products],
          # Lower-case basin abbreviation
          stringr::str_to_lower(stringr::str_sub(stormid, 1L, 2L)),
          # storm number
          stringr::str_to_lower(stringr::str_sub(stormid, 3L, 4L)),
          # year without century, 4-digits
          stringr::str_to_lower(stringr::str_sub(stormid, 7L, 8L))
        )
      )

      files <- file.path(destdir, files)
      files_length <- purrr::map(.x = files, .f = file.info) |>
        purrr::map_dbl("size")
      res_txt <- purrr::map2_chr(.x = files, .y = files_length, readChar)

    } else {
      links <- sprintf(
        fmt = "ftp://ftp.nhc.noaa.gov/atcf/archive/%s/messages/%s",
        yyyy,
        links
      )

      res <- hurricanes:::get_url_contents(links)
      res_parsed <- purrr::map(res, ~xml2::read_html(.$content))
      res_txt <- purrr::map_chr(res_parsed, rvest::html_text)

    }
  } else {
    # If the `yyyy` value is not in the ftp archives, then it is in a product
    # folder directly under **atcf** directory.
    #
    # There are a number of directories that contain valuable info. But, I'll
    # concentrate on the standard products the `rrricanes` package currently
    # works:
    # - discus: **atcf/dis**
    # - fstadv: **atcf/mar**
    # - posest: Does not exist; none issued for 2018
    # - public: **atcf/public**
    # - update: Does not exist; 59 issued for 2018
    # - wndprb: **atcf/wndprb**
    #
    named_products <- list(
      "discus" = "dis",
      "fstadv" = "mar",
      "public" = "pub",
      "wndprb" = "wndprb"
    )

    ftp_subdir <- sprintf("atcf/%s/", named_products[products])
    ftp_contents <- get_ftp_dirs(ftp_subdir)

    links <-
      ftp_contents |>
      dplyr::filter(
        grepl(
          pattern =
            sprintf(
              fmt = "^%s\\.%s\\.\\d{3}$",
              stringr::str_to_lower(stormid),
              products
            ),
          x = .data$Name
        )
      ) |>
      dplyr::pull(.data$Name)

    links <- sprintf(
      fmt = "ftp://ftp.nhc.noaa.gov/atcf/%s/%s",
      named_products[products],
      links
    )

    res <- hurricanes:::get_url_contents(links)
    res_parsed <- purrr::map(res, ~xml2::read_html(.$content))
    res_txt <- purrr::map_chr(res_parsed, rvest::html_text)

  }

  df <- purrr::invoke_map_df(
    .f = utils::getFromNamespace(x = products, ns = "rrricanes"),
    .x = res_txt
  )

}
