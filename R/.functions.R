#' @title build_datetime
build_datetime <- function(y = 0, mo = 0, d = 0, h = 0, mi = 0, s = 0, tz = 'UTC') {

  if(is.character(mo)) {
    mo <- month_str_to_num(mo)
  } else {
    mo <- as.numeric(mo)
  }

  y <- as.numeric(y)
  d <- as.numeric(d)
  h <- as.numeric(h)
  mi <- as.numeric(mi)
  s <- as.numeric(s)

  dtdate <- paste(y, mo, d, sep = '-')
  dttime <- paste(h, mi, s, sep = ':')
  dt <- lubridate::ymd_hms(paste(dtdate, dttime, sep = ' '))
  return(dt)

}

#' #' @title filter_key
#' #' @description If key provided on startup, filter lists on specific storm. Not all URLs will
#' #'   have key but will have other data that can be used to find correct storm.
#' #'   Nonetheless, no guarantee all products will be retrieved (i.e., typos)
#' #' @param key alphanumeric(8)
#' filter_key <- function(k) {
#'   basin <- toupper(substr(k, 0, 2))
#'   num <- as.numeric(substr(k, 3, 4))
#'   kyear <- as.numeric(substr(k, 5, 8))
#'
#'   # Pad num with leading 0's
#'   snum <- str_pad(num, 2, pad = '0')
#'
#'   # forecasts, publics, discussions, strike_probs, wind_probs, updates, pos_estimate
#'   if(kyear <= 2000) {
#'     fcst_keys <- grep(paste0(kyear, '[[:alpha:]/]+mar/M', basin, snum, '.'), forecasts)
#'     pub_keys <- grep(paste0(kyear, '[[:alpha:]/]+pub/P', basin, snum, '.'), publics)
#'     dis_keys <- grep(paste0(kyear, '[[:alpha:]/]+dis/N', basin, snum, '.'), discussions)
#'     sp_keys <- grep(paste0(kyear, '[[:alpha:]/]+prb/L', basin, snum, '.'), strike_probs)
#'   } else {
#'     if(kyear <= 2005) {
#'       # Now can use Identifier
#'       fcst_keys <- grep(paste0('mar/', k), forecasts)
#'       pub_keys <- grep(paste0('pub/', k), publics)
#'       dis_keys <- grep(paste0('dis/', k), discussions)
#'       sp_keys <- grep(paste0(kyear, '[[:alpha:]/]+prb/L', basin, snum, '.'), strike_probs)
#'     } else {
#'       fcst_keys <- grep(paste0(k, '\\.fstadv'), forecasts)
#'       pub_keys <- grep(paste0(k, '\\.public'), publics)
#'       dis_keys <- grep(paste0(k, '\\.discus'), discussions)
#'       # Strike probabliities became Wind probabilities in 2006
#'       wp_keys <- grep(paste0(k, '\\.wndprob'), wind_probs)
#'     }
#'   }
#'
#'   forecasts <<- forecasts[fcst_keys]
#'   publics <<- adv_list[pub_keys]
#'   discussions <<- discussions[dis_keys]
#'   if(exists('sp_keys')) {
#'     strike_probs <<- strike_probs[sp_keys]
#'   } else {
#'     wind_probs <<- wind_probs[wp_keys]
#'   }
#'
#'   return(TRUE)
#'
#' }
#'
#' @title find_na
#' @description Stop execution if there are any NA values in the list
find_na <- function(ds, ds_name) {
  if(sum(is.na(ds)) > 0) {
    # There are NA values; stop execution
    stop(paste('There are NA values in ', ds_name))
  }
}

#' @title get_key
#' @description Returns 8-character alphanum string like AL012016 where AL is
#' the basin, 01 is the number of the storm for that year (first) and 2016 is
#' the year
#' @param contents text content of product
#' @return character alphanumeric
get_key <- function(contents) {

  # Get year
  y <- lubridate::year(get_time_header(contents))

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
  x <- stringr::str_match(contents, ptn)[,2]

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

#' @title get_product_contents
#' @description Retrieves advisory text from link
#' @param link current working url, set in global env
#' @return contents text of product
get_product_contents <- function(link) {

  # Get year
  year <- extract_year_archive_link(link)

  # Get URL
  txt_url <- xml2::read_html(link)

  # Get contents of txt_url
  if(year == 1998) {
    page <- txt_url %>%
      rvest::html_nodes('body') %>%
      rvest::html_children()
  } else if (year == 1999) {
    page <- txt_url %>%
      rvest::html_nodes('body') %>%
      rvest::html_nodes('pre')
  } else {
    page <- txt_url %>%
      rvest::html_nodes('.center') %>%
      rvest::html_nodes('.content') %>%
      rvest::html_nodes('pre')
  }

  contents <- rvest::html_text(page[1])

  return(contents)

}

#' @title get_storm_id
#' @description Return id or row index of storm in df_summaries
#'   Not to be confused with identifier, an 8-length alphanum string
#'   (see get_identifier)
#' @param storm_key Identifier of storm
#' @return numeric
get_storm_id <- function(storm_key) {

  if(!is.character(storm_key) | nchar(storm_key) != 8) {
    stop("storm_key(Identifier) invalid!")
  }

  id <- grep(storm_key, df_summaries$Identifier)

  if(length(id) != 1) {
    # Either too many or none
    return(NA)
  } else if (length(id) == 1) {
    # One entry. Good
    return(id)
  } else {
    # Something's not right.
    return(NULL)
  }

}

#' @title get_storm_name
#' @description Get storm's name from text product
#' @return character
get_storm_name <- function(content) {
  name <- get_name_header(content, what = 'name')
  return(name)
}

#' @title get_time_header()
#' @description Parse time line from header of anyproduct. Time line looks like:
#'
#' 0300 UTC THU AUG 16 2007
#'
#' Those should be pretty self-explanatory. Returns a properly formatted
#'   POSIXct date formatted with lubridate::ymd_hm
#'
#' @param contents text content of product
#' @return character Date
get_time_header <- function(contents) {

  ptn <- paste0('([0-9]{4})', # Time
                '[ ]*(?:Z|UTC)', # Timezone
                '[ ]+[A-Z]{3}', # Day
                '[ ]+([A-Z]{3})', # Month
                '[ ]+([0-9]{1,2})', # Date
                '[ ]+([0-9]{4})')# Year

  x <- stringr::str_match(contents, ptn)

  # Get formatted date
  dt <- build_datetime(y = x[,5],
                       mo = x[,3],
                       d = x[,4],
                       h = substr(x[,2], 0, 2),
                       mi = substr(x[,2], 3, 4))

  return(dt)

}

#' @title write_issues
#' @description Write warnings and errors to dataframes
#' @param c condition
#' @param type c('error', 'warning')
#' @return boolean
write_issues <- function(c, type = NULL) {

  if(!(type %in% c('warning', 'error'))) {stop('type must be warning or error')}

  if(type == 'warning') {
    message(paste0('WARNING: ', c))
  } else {
    message(paste0('ERROR: ', c))
    print(traceback())
  }

  return(TRUE)

}
