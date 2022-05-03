#' @title scrape_date
#' @description Scrape date/time of product issuance from header.
#' @param header Header text of product.
#' @seealso \code{\link{scrape_header}}
#' @keywords internal
scrape_date <- function(header) {

  maketime <- function(h, m, p) {

    h <- as.numeric(h)
    m <- as.numeric(m)

    if (any(is.na(m))) {
      i <- which(is.na(m))
      m[i] <- 0
    }

    # If !is.na(p), convert h appropriately
    h[which(p == "PM")] <- h[which(p == "PM")] + 12

    h <- stringr::str_pad(h, 2, side = "left", pad = "0")
    m <- stringr::str_pad(m, 2, side = "left", pad = "0")

    stringr::str_c(h, m, sep = ":")

  }

  # The time value in the headers can vary depending on the product. In
  # forecast advisories time is in \%H\%M format with a trailing Z (for Zulu
  # orUTC time). In other products it can be "\%I \%r" or "\%I%M %r" except
  # in the latter "%I" may not have a leading 0.

  # Timezones also vary. Forecasts use UTC while public advisories may be in
  # the local time zone (possibly CDT) while discussions may be in the time
  # zone of the issuing office (the NHC in Miami, so EDT).

  # What is standard is that time comes first followed by time zone, day of
  # the week, month, date and year. So, find the pattern that matches.

  # In some instances the time value in the header may be listed as "NOON"
  # rather than "12 PM". This is documented in Issue #59. In these cases,
  # correct header.
  if (
    any(
      stringr::str_count(
        header,
        pattern = stringr::str_c("\nNOON [:upper:]{3} [:upper:]{3} ",
                                 "[:upper:]{3} [:digit:]{2} ",
                                 "[:digit:]{4}\n"))))

    header <- stringr::str_replace(
      header,
      pattern = stringr::str_c("\n(NOON)",
                               "( [:upper:]{3}",
                               " [:upper:]{3} ",
                               "[:upper:]{3} ",
                               "[:digit:]{2} ",
                               "[:digit:]{4})\n"),
      "\n12 PM\\2\n")

  # Same thing for "MIDNIGHT"
  if (
    any(
      stringr::str_count(
        header,
        pattern = stringr::str_c("\nMIDNIGHT",
                                 " [:upper:]{3} [:upper:]{3} ",
                                 "[:upper:]{3} [:digit:]{2} ",
                                 "[:digit:]{4}\n"))))

    header <- stringr::str_replace(
      header,
      pattern = stringr::str_c("\n(MIDNIGHT)( ",
                               "[:upper:]{3}",
                               " [:upper:]{3} ",
                               "[:upper:]{3} ",
                               "[:digit:]{2} ",
                               "[:digit:]{4})\n"),
      "\n12 AM\\2\n")

  # And yes there is actually an entry of 12 NOON; see AL132002 public adv 49A
  if (
    any(
      stringr::str_count(
        header,
        pattern = stringr::str_c("\n12 NOON",
                                 " [:upper:]{3} [:upper:]{3} ",
                                 "[:upper:]{3} [:digit:]{2} ",
                                 "[:digit:]{4}\n"))))

    header <- stringr::str_replace(
      header,
      pattern = stringr::str_c("\n(12 NOON)( ",
                               "[:upper:]{3}",
                               " [:upper:]{3} ",
                               "[:upper:]{3} ",
                               "[:digit:]{2} ",
                               "[:digit:]{4})\n"),
      "\n12 PM\\2\n")

  ptn <- stringr::str_c(
    "(?<=(?:\n|\r))",
    "([:digit:]{1,2})",                 # Hour
    "(?<=[:digit:]{1})([:digit:]{2})?", # Minute
    "(?:Z)?",                           # For forecast; Z is (UTC)
    "[:blank:]",
    "(?:AM|PM)?[:blank:]?",
    "([:alpha:]{3})*?",                 # Time zone, optional
    "[:blank:]?",
    "(?:[:alpha:]{3})",                 # Day of week, no capture
    "[:blank:]",
    "([:alpha:]{3})",                   # Month, abbreviated uppercase
    "[:blank:]",
    "([:digit:]{1,2})",                 # Date
    "[:blank:]",
    "([:digit:]{4})",                   # Year
    "[[:blank:]\n\r]*"
  )

  datetime.extracted <- stringr::str_match(header, ptn)

  # Capture period (AM|PM), if exists
  period <- stringr::str_match(datetime.extracted, "[:blank:](AM|PM)[:blank:]")

  # Convert time values to 24-hour format, UTC
  t <- maketime(datetime.extracted[,2], # Hour
                datetime.extracted[,3], # Minute
                period[,2])

   tz <- datetime.extracted[,4]
  if (any(is.na(tz))) {
    i <- which(is.na(tz))
    tz[i] <- "UTC"
  }

  # Format date
  d <- as.Date(stringr::str_c(datetime.extracted[,7], # Year, four-digit format
                              datetime.extracted[,5], # Month, abbreviated
                              datetime.extracted[,6], # Date, w/wo leading 0

                              sep = "-"),
               format = "%Y-%b-%d")

  # If time zone is NA, make UTC. Is NA because in forecast products time is
  # immeidately followed by Z which is not captured. Z is military code for
  # Zulu time which is equivalent of Z.

  # That should be the reason...

  # Make date/time string
  x <- stringr::str_c(d, t, sep = " ")

  # To ensure we get the proper timezone I'm going to use OlsonNames()
  # instead of the abbreviation. The timezones will need to be converted to
  # UTC/GMT for some products. But using EDT for example will not convert
  # properly whereas "America/New_York" will.

  timezones <- c(
    "UTC" = "UTC",
    "GMT" = "UTC",
    "ADT" = "Etc/GMT+3",
    "AST" = "Etc/GMT+4",
    "CDT" = "Etc/GMT+5",
    "CST" = "Etc/GMT+6",
    "EDT" = "Etc/GMT+4",
    "EST" = "Etc/GMT+5",
    "HDT" = "Etc/GMT+9",
    "HST" = "Etc/GMT+10",
    "MDT" = "Etc/GMT+6",
    "MST" = "Etc/GMT+7",
    "PDT" = "Etc/GMT+7",
    "PST" = "Etc/GMT+8"
  )

  dt <- vector(mode = "integer", length = length(x))
  class(dt) <- c("POSIXct", "POSIXt")

  for (i in 1:(length(dt))) {
    dt[i] <- as.POSIXct(strftime(x[i], format = "%Y-%m-%d %H:%M"),
                        tz = ifelse(length(tz[i] == 0),
                                    "UTC",
                                    unname(timezones[tz[i]]))
                        )

  }

  # Now convert to UTC
  lubridate::with_tz(dt, tzone = "UTC")

}

#' @title scrape_header
#' @description Extract status, name, and advisory from products header.
#' @param contents Text product
#' @param ptn_product_title Pattern of product title to match
#' @param advisory_number Default is true; set to false if product does not
#'   have an advisory number.
#' @keywords internal
scrape_header <- function(contents, ptn_product_title,
                          advisory_number = TRUE) {

  # See test_scrapers.R for patterns that must be matched.
  # Extract header. Use the format of the date/time line to close out header.
  # There may be additional line breaks inside the header. Must account for.
  # Use day, month, date and year which seems to be consistent across all
  # products.
  # (timtrice): Added backtick for AL162005 public #18
  ptn_header <- paste0("^[\\w\\d\\s\\W]*?\\w{3}\\s*\\w{3}\\s*\\d{1,2}\\s*\\d{4}[\\s\n\r]*")

  header <- stringr::str_extract(contents, ptn_header)

  # Storm status patterns
  ptn_status <- "((?:POST-|POTENTIAL\\s|SUB)?TROPICAL (?:CYCLONE|DEPRESSION|DISTURBANCE|STOMR|STORM)|HURRICANE|REMNANTS)(?: OF)?"

  # Pattern for storm names
  ptn_names <- stringr::str_c("([\\w-]*?)")

  ptn_adv = "NUMBER\\s+(\\d{1,3}\\w?)"

  # Combine patterns
  ptn <- stringr::str_c(
    ptn_status, ptn_names, ptn_product_title, sep = "\\s"
  )

  if (advisory_number) {
    ptn <-  stringr::str_c(ptn, ptn_adv, sep = "\\s")
    matches <- stringr::str_match(header, ptn)[,2:4]
  } else {
    matches <- stringr::str_match(header, ptn)[,2:3]
    status <- apply(stringr::str_match(header, ptn)[,2:3], 2, stringr::str_to_title)
  }

  # String-to-title Status and Name

  if (is.null(ncol(matches))) {
    # working with a vector
    matches[1:2] <- stringr::str_to_title(matches[1:2])
  } else {
    # Working with a matrix
    matches[,c(1:2)] <- apply(matches[,c(1:2)], 2, stringr::str_to_title)
  }

   matches

}

#' @title scrape_key
#' @description Extract StormKey from header
#' @param header Header text of product.
#' @seealso \code{\link{scrape_header}}
#' @keywords internal
scrape_key <- function(header) {

  # There are several possibilities that can preceed StormKey in the storm header.
  # ptn should capture each possibility, but only one of.
  ptn <- stringr::str_c(
    "(?:(?:NATIONAL HURRICANE CENTER|",
    "NATIONAL[:blank:]WEATHER[:blank:]SERVICE)?",
    "[:blank:]+MIAMI FL[:blank:]+|",
    "NATIONAL WEATHER SERVICE HONOLULU HI[:blank:]+|",
    "NWS CENTRAL PACIFIC HURRICANE CENTER HONOLULU HI[:blank:]+)",
    "([:alnum:]{6,8})"
  )

  ptn <- stringr::str_c(ptn, collapse = '')

  stringr::str_match(header, ptn)[,2]

}

