#' @title scrape_adv_num
#' @description Scrape advisory number from header.
#' @param header Header text of product.
#' @seealso \code{\link{scrape_header}}
#' @keywords internal
scrape_adv_num <- function(header) {
    ptn <- paste0("(?:ADVISORY|DISCUSSION|PROBABILITIES)*[:blank:]+",
                  "(?:INTERMEDIATE[:blank:])*NUMBER",
                  # Advisory number. Could alphanum; i.e., 1, 1A, 2, 2A, 2B
                  "[:blank:]+([:digit:]{1,3}[:alpha:]*)",
                  "(?:[[:space:][:punct:][:alpha:]]*)+")
    adv <- trimws(stringr::str_match(header, ptn)[,2])
    return(adv)
}

#' @title scrape_contents
#' @description Extract text product from HTML
#' @param link URL to product page
#' @return Contents of product
#' @keywords internal
scrape_contents <- function(link) {

    if (length(link) == 0)
        stop("No valid links.")

    contents <- get_url_contents(link) %>%
        rvest::html_nodes(xpath = "//pre") %>%
        rvest::html_text()

    if (purrr::is_empty(contents))
        contents <- get_url_contents(link) %>% rvest::html_text()

    return(contents)

}

#' @title scrape_date
#' @description Scrape date/time of product issuance from header.
#' @param header Header text of product.
#' @seealso \code{\link{scrape_header}}
#' @keywords internal
scrape_date <- function(header) {

    maketime <- function(h, m, p) {

        h <- as.numeric(h)
        m <- as.numeric(m)

        if (is.na(m))
            m <- 0

        # If !is.na(p), convert h appropriately
        if (all(!is.na(p), p == "PM", h < 12))
            h <- h + 12

        h <- stringr::str_pad(h, 2, side = "left", pad = "0")
        m <- stringr::str_pad(m, 2, side = "left", pad = "0")

        x <- paste(h, m, sep = ":")

        return(x)
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
    if (stringr::str_count(header,
                           pattern = paste0("\nNOON [:upper:]{3} [:upper:]{3} ",
                                            "[:upper:]{3} [:digit:]{2} ",
                                            "[:digit:]{4}\n")))
        header <- stringr::str_replace(header,
                                       pattern = paste0("\n(NOON)( [:upper:]{3}",
                                                        " [:upper:]{3} ",
                                                        "[:upper:]{3} ",
                                                        "[:digit:]{2} ",
                                                        "[:digit:]{4})\n"),
                                                        "\n12 PM\\2\n")

    # Same thing for "MIDNIGHT"
    if (stringr::str_count(header,
                           pattern = paste0("\nMIDNIGHT [:upper:]{3} [:upper:]{3} ",
                                            "[:upper:]{3} [:digit:]{2} ",
                                            "[:digit:]{4}\n")))
        header <- stringr::str_replace(header,
                                       pattern = paste0("\n(MIDNIGHT)( ",
                                                        "[:upper:]{3}",
                                                        " [:upper:]{3} ",
                                                        "[:upper:]{3} ",
                                                        "[:digit:]{2} ",
                                                        "[:digit:]{4})\n"),
                                       "\n12 AM\\2\n")

    # And yes there is actually an entry of 12 NOON (see AL132002 public adv 49A)
    if (stringr::str_count(header,
                           pattern = paste0("\n12 NOON [:upper:]{3} [:upper:]{3} ",
                                            "[:upper:]{3} [:digit:]{2} ",
                                            "[:digit:]{4}\n")))
        header <- stringr::str_replace(header,
                                       pattern = paste0("\n(12 NOON)( ",
                                                        "[:upper:]{3}",
                                                        " [:upper:]{3} ",
                                                        "[:upper:]{3} ",
                                                        "[:digit:]{2} ",
                                                        "[:digit:]{4})\n"),
                                       "\n12 PM\\2\n")
    # I'll clean all that up later. Too tired right now...

    ptn <- paste0("(?<=(?:\n|\r))",
                  "([:digit:]{1,2})", # Hour
                  "(?<=[:digit:]{1})([:digit:]{2})?", # Minute
                  "(?:Z)?", # For forecast; Z is military, no offset for UTC
                  "[:blank:]",
                  "(?:AM|PM)?[:blank:]?",
                  "([:alpha:]{3})*?", # Time zone, optional
                  "[:blank:]?",
                  "(?:[:alpha:]{3})", # Day of week, no capture
                  "[:blank:]",
                  "([:alpha:]{3})", # Month, abbreviated uppercase
                  "[:blank:]",
                  "([:digit:]{1,2})", # Date
                  "[:blank:]",
                  "([:digit:]{4})",  # Year
                  "[[:blank:]\n\r]*")

    datetime.extracted <- stringr::str_match(header, ptn)

    # Capture period (AM|PM), if exists
    period <- stringr::str_match(header, "[:blank:](AM|PM)[:blank:]")

    # Convert time values to 24-hour format, UTC
    t <- maketime(datetime.extracted[,2], # Hour
                  datetime.extracted[,3], # Minute
                  period[,2])

    # Format date
    d <- as.Date(paste(datetime.extracted[,5], # Month, abbreviated
                       datetime.extracted[,6], # Date, w/wo leading 0
                       datetime.extracted[,7], # Year, four-digit format
                       sep = "-"),
                 format = "%b-%d-%Y")

    # If time zone is NA, make UTC. Is NA because in forecast products time is
    # immeidately followed by Z which is not captured. Z is military code for
    # Zulu time which is equivalent of Z.

    # That should be the reason...
    tz <- datetime.extracted[,4]
    if (is.na(tz))
        tz <- "UTC"

    # Make date/time string
    x <- paste(d, t, sep = " ")

    # To ensure we get the proper timezone I'm going to use OlsonNames()
    # instead of the abbreviation. The timezones will need to be converted to
    # UTC/GMT for some products. But using EDT for example will not convert
    # properly whereas "America/New_York" will.

    if (tz %in% c("GMT", "UTC")) {
        dt <- as.POSIXct(x, tz = "UTC")
    } else if (tz %in% c("ADT")) {
        dt <- as.POSIXct(x, tz = "Etc/GMT+3")
    } else if (tz %in% c("AST")) {
        dt <- as.POSIXct(x, tz = "Etc/GMT+4")
    } else if (tz %in% c("CDT")) {
        dt <- as.POSIXct(x, tz = "Etc/GMT+5")
    } else if (tz %in% c("CST")) {
        dt <- as.POSIXct(x, tz = "Etc/GMT+6")
    } else if (tz %in% c("EDT")) {
        dt <- as.POSIXct(x, tz = "Etc/GMT+4")
    } else if (tz %in% c("EST")) {
        dt <- as.POSIXct(x, tz = "Etc/GMT+5")
    } else if (tz %in% c("HDT")) {
        dt <- as.POSIXct(x, tz = "Etc/GMT+9")
    } else if (tz %in% c("HST")) {
        dt <- as.POSIXct(x, tz = "Etc/GMT+10")
    } else if (tz %in% c("MDT")) {
        dt <- as.POSIXct(x, tz = "Etc/GMT+6")
    } else if (tz %in% c("MST")) {
        dt <- as.POSIXct(x, tz = "Etc/GMT+7")
    } else if (tz %in% c("PDT")) {
        dt <- as.POSIXct(x, tz = "Etc/GMT+7")
    } else if (tz %in% c("PST")) {
        dt <- as.POSIXct(x, tz = "Etc/GMT+8")
    } else {
        stop(sprintf("Timezone %s not available.", tz), call. = TRUE)
    }
    # Now convert to UTC
    # dt <- format(dt, tz = "UTC", usetz = TRUE)
    dt <- lubridate::with_tz(dt, tzone = "UTC")

    return(dt)
}

#' @title scrape_name_header
#' @description Extract values from products header.
#' @details A typical header will look like this:
#' \preformatted{
#'   ZCZC MIATCPAT1 ALL
#'   TTAA00 KNHC DDHHMM
#'   BULLETIN
#'   TROPICAL DEPRESSION ONE ADVISORY NUMBER   1
#'   NATIONAL WEATHER SERVICE MIAMI FL
#'   11 AM AST MON JUL 27 1998
#' }
#' The first three lines are irrelevant. This function is only concerned about
#' returning Status, Name, Advisory Number, and Date/Time values.
#' @param contents text content of product
#' @param ret Choose either status, name, adv or date to return.
#' \describe{
#'   \item{status}{status of cyclone (Hurricane, Tropical Storm, Subtropical
#'     Storm...)}
#'   \item{name}{return name of storm}
#'   \item{adv}{return advisory number}
#'   \item{date}{Date/Time of product issuance.}
#'   \item{key}{Key, unique identifier for each storm. May not be available in
#'     some packages}
#' }
#' @return Returns integer advisory number, character status or name, or date
#'   in \%F \%R (yyyy-mm-dd hh:mm) format, UTC time zone.
#' @keywords internal
scrape_header <- function(contents, ret = NULL) {

    # Extract header. Use the format of the date/time line to close out header.
    # There may be additional line breaks inside the header. Must account for.
    # Use day, month, date and year which seems to be consistent across all
    # products.
    # (timtrice): Added backtick for AL162005 public #18
    ptn_header <- paste0("^[[:alnum:][:blank:][:punct:]`\n\r]*?",
                         "[:alpha:]{3}", # Day of week
                         "[:blank:]*",
                         "[:alpha:]{3}", # Month, abbreviated
                         "[:blank:]*",
                         "[:digit:]{1,2}", # Date
                         "[:blank:]*",
                         "[:digit:]{4}", # Year
                         "[[:blank:]\n\r]*") # Close off date/time line
    header <- stringr::str_extract(contents, ptn_header)

    # Convert header to upper as some products may use proper/lower case
    header <- stringr::str_to_upper(header)

    if (is.null(ret)) {
        return(header)
    } else if (ret == "status") {
        status <- scrape_status(header)
        return(status)
    } else if (ret == "name") {
        name <- scrape_name(header)
        return(name)
    } else if (ret == "adv") {
        adv <- scrape_adv_num(header)
        return(adv)
    } else if (ret == "date") {
        date <- scrape_date(header)
        return(date)
    } else if (ret == "key") {
        key <- scrape_key(header)
        return(key)
    }

}

#' @title scrape_key
#' @description Extract Key from header
#' @param header Header text of product.
#' @seealso \code{\link{scrape_header}}
#' @keywords internal
scrape_key <- function(header) {
    # Get year
    y <- lubridate::year(scrape_header(header, ret = "date"))

    # There are several possibilities that can preceed Key in the storm header.
    # ptn should capture each possibility, but only one of.
    ptn <- paste0("(?:(?:NATIONAL HURRICANE CENTER|",
                  "NATIONAL[:blank:]WEATHER[:blank:]SERVICE)?",
                  "[:blank:]+MIAMI FL[:blank:]+|",
                  "NATIONAL WEATHER SERVICE HONOLULU HI[:blank:]+|",
                  "NWS CENTRAL PACIFIC HURRICANE CENTER HONOLULU HI[:blank:]+)")

    # For <= 2003 Identifier is 6-digits with a 2-digit year. Append either
    # option to ptn based on year of cyclone.
    if (y <= 2003) {
        ptn <- c(ptn, '([:alnum:]{6})')
    } else {
        ptn <- c(ptn, '([:alnum:]{8})')
    }
    ptn <- paste0(ptn, collapse = '')
    x <- stringr::str_match(header, ptn)[,2]

    # If year is 1999 and Key is "EP9099", send warning.
    # This is a temp correction for Issue #55 in GitHub repo.
    if (all(y == 1999, x == "EP9099"))
        warning(paste0("Known data quality error. Key for Advisory 1 ",
                       "is incorrect. See GitHub Issue #55"),
                call. = FALSE)

    # In some instances x is NA. Stop and research.
    if (nchar(x) != 6 & nchar(x) != 8) {
        stop('Identifier is improperly formatted.')
    } else {
        # Reformat Identifer to 8 digits, if necessary
        if (nchar(x) == 6)
            x <- paste0(substr(x, 0, 4), y)
        return(x)
    }
}

#' @title scrape_name
#' @description Scrape name from product header.
#' @param header Header text of product.
#' @seealso \code{\link{scrape_header}}
#' @keywords internal
scrape_name <- function(header) {
    # Status helps to find name, so get status.
    status <- scrape_status(header)
    # Because status returns toproper() string, have to convert toupper() for
    # pattern match.
    ptn <- paste0(toupper(status), "[:blank:]([[:alpha:]-]*)[:blank:]")
    name <- stringr::str_to_title(trimws(stringr::str_match(header, ptn)[,2]))
    return(name)
}

#' @title scrape_status
#' @description Scrape status from product header.
#' @param header Header text of product.
#' @seealso \code{\link{scrape_header}}
#' @keywords internal
scrape_status <- function(header) {
    options <- c("SUBTROPICAL DEPRESSION",
                 "TROPICAL DISTURBANCE",
                 "TROPICAL DEPRESSION",
                 "TROPICAL STORM",
                 "HURRICANE",
                 "POST-TROPICAL CYCLONE",
                 "POTENTIAL TROPICAL CYCLONE",
                 "REMNANTS OF")
    if (!any(stringr::str_count(header, paste(options, sep = "|"))))
        stop(sprintf("Options not in header. %s", header))
    ptn <- paste(options, collapse = "|")
    status <- stringr::str_to_title(trimws(stringr::str_extract(header, ptn)))
    return(status)
}
