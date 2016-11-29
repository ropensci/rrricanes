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
#' @export
scrape_header <- function(contents, ret = NULL) {
  
  if(!(ret %in% c("status", "name", "adv", "date", "key"))) {
    stop('\"ret\" must be one of status, name, adv, date or key.')
  }
  
  # Extract header
  ptn_header <- "^[[:alnum:][:blank:][:punct:]\n]*?\n[:blank:]*?\n"
  header <- stringr::str_extract(contents, ptn_header)
  
  if(ret == "status") {
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
  } else {
    stop('NA values in name header.')
    return(FALSE)
  }
  
}

#' @title scrape_status
#' @description Scrape status from product header.
#' @param header Header text of product.
#' @seealso \code{\link{scrape_header}}
#' @export
scrape_status <- function(header) {
  options <- c("TROPICAL DISTURBANCE", 
               "TROPICAL DEPRESSION", 
               "TROPICAL STORM", 
               "HURRICANE")
  if(!any(stringr::str_count(header, paste(options, sep = "|"))))
    stop("Options not in header.")
  ptn <- paste(options, collapse = "|")
  status <- trimws(stringr::str_extract(header, ptn))
  return(status)
}

#' @title scrape_name
#' @description Scrape name from product header.
#' @param header Header text of product.
#' @seealso \code{\link{scrape_header}}
#' @export
scrape_name <- function(header) {
  # Status helps to find name, so get status.
  status <- scrape_status(header)
  ptn <- paste0(status, "[:blank:]([:alpha:]+)[:blank:]")
  name <- trimws(stringr::str_match(header, ptn)[,2])
  return(name)
}

#' @title scrape_adv_num
#' @description Scrape advisory number from header.
#' @param header Header text of product.
#' @seealso \code{\link{scrape_header}}
#' @export
scrape_adv_num <- function(header) {
  ptn <- paste0("(?:ADVISORY|DISCUSSION|PROBABILITIES)", 
                "[:blank:]", 
                "NUMBER", 
                "[:blank:]+", 
                "([:digit:]+[:alpha:]*?)", # Advisory number
                "\n")
  adv <- trimws(stringr::str_match(header, ptn)[,2])
  return(adv)
}

#' @title scrape_date
#' @description Scrape date/time of product issuance from header.
#' @param header Header text of product.
#' @seealso \code{\link{scrape_header}}
#' @export
scrape_date <- function(header) {
  
  maketime <- function(h, m, p) {
    
    h <- as.numeric(h)
    m <- as.numeric(m)

    if(is.na(m))
      m <- 0
    
    # If !is.na(p), convert h appropriately
    if(all(!is.na(p), p == "PM"))
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
  
  ptn <- paste0("(?<=\n)", 
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
                "([:digit:]{2})", # Date
                "[:blank:]", 
                "([:digit:]{4})",  # Year
                "\n")
  
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
  
  # That should be the reason... ¯\_(ツ)_/¯
  tz <- datetime.extracted[,4]
  if(is.na(tz))
    tz <- "UTC"

  # Make date/time string
  x <- paste(d, t, sep = " ")
  
  # Properly formatted date/time string
  dt <- lubridate::ymd_hm(x, tz = tz)
  
  return(dt)
}

#' @title scrape_key
#' @description Extract Key from header
#' @param header Header text of product.
#' @seealso \code{\link{scrape_header}}
#' @export
scrape_key <- function(header) {
  # Get year
  y <- lubridate::year(scrape_header(header, ret = "date"))
  
  # For <= 2003 Identifier is 6-digits with a 2-digit year. 
  ptn <- list(paste0('(?:NATIONAL[:blank:]HURRICANE[:blank:]CENTER|', 
                     'NATIONAL[:blank:]WEATHER[:blank:]SERVICE)?', 
                     '[:blank:]+MIAMI[:blank:]FL[:blank:]+'))
  if(y <= 2003) {
    ptn <- c(ptn, '([:alnum:]{6})')
  } else {
    ptn <- c(ptn, '([:alnum:]{8})')
  }
  ptn <- paste0(ptn, collapse = '')
  x <- stringr::str_match(header, ptn)[,2]
  
  # In some instances x is NA. Stop and research. 
  if(nchar(x) != 6 & nchar(x) != 8) {
    stop('Identifier is improperly formatted.')
  } else {
    # Reformat Identifer to 8 digits, if necessary
    if(nchar(x) == 6)
      x <- paste0(substr(x, 0, 4), y)
    return(x)
  }
}
