#' @title crul_discus
#' @description Parse storm Discussion products
#' @param contents text of Storm Discussion product
#' @keywords internal
crul_discus <- function(contents) {

  # Replace all carriage returns with empty string.
  contents <- stringr::str_replace_all(contents, "\r", "")

  # Make sure this is a discussion product
  if (!any(stringr::str_count(contents,
                              c("MIATCD", "MIATCM", "TCD", "WTPA", "WTPZ",
                                "MIAWRKAD1")))) {
    # Check if the term "DISCUSSION" appears in header
    if (!stringr::str_detect(scrape_header(contents), "DISCUSSION"))
      stop(sprintf("Invalid Discussion link. %s", link))
  }

  df <- create_df_discus()

  status <- scrape_header(contents, ret = "status")
  name <- scrape_header(contents, ret = "name")
  adv <- scrape_header(contents, ret = "adv") %>% as.numeric()
  date <- scrape_header(contents, ret = "date")

  # Keys were added to discus products beginning 2006. Prior, it doesn't
  # exist. safely run scrape_header for key. If error, then use NA. Otherwise,
  # add it.
  safely_scrape_header <- purrr::safely(scrape_header)
  key <- safely_scrape_header(contents, ret = "key")
  if (is.null(key$error)) {
    key <- key$result
  } else {
    key <- NA
  }

  if (getOption("rrricanes.working_msg"))
    message(sprintf("Working %s %s Storm Discussion #%s (%s)",
                    status, name, adv, date))

  df <- df %>%
    tibble::add_row("Status" = status,
                    "Name" = name,
                    "Adv" = adv,
                    "Date" = date,
                    "Key" = key,
                    "Contents" = contents)

  return(df)
}

#' @title crul_extract_storms
#' @description Extract storms for the given basin
#' @param basin AL or EP
#' @param contents Contents of archive pages
#' @export
crul_extract_storms <- function(basin, contents) {

  if (basin == "AL") {
    link_xpath <- "//td[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]//a"
  } else if (basin == "EP") {
    link_xpath <- "//td[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]//a"
  } else {
    stop("No basin")
  }

  contents <- purrr::map(contents, ~.$parse("UTF-8")) %>%
    purrr::map(xml2::read_html)

  years <- purrr::map(contents, rvest::html_nodes, xpath = "//title") %>%
    purrr::map(rvest::html_text) %>%
    stringr::str_sub(0L, 4L) %>%
    as.numeric()

  storms <- purrr::map(contents, rvest::html_nodes, xpath = link_xpath)
  names <- purrr::map(storms, rvest::html_text) %>%
    purrr::map(stringr::str_to_title)
  links <- purrr::map(storms, rvest::html_attr, name = "href") %>%
    purrr::map2(years, ~paste0(year_archives_link(.y), .x))
  basins <- purrr::map(names, purrr::rep_along, basin)
  years <- purrr::map2(names, years, purrr::rep_along)

  df <- tibble::data_frame("Year" = years %>% purrr::flatten_dbl(),
                           "Name" = names %>% purrr::flatten_chr(),
                           "Basin" = basins %>% purrr::flatten_chr(),
                           "Link" = links %>% purrr::flatten_chr())

  return(df)
}

#' @title crul_fstadv
#' @description Extrapolate data from FORECAST/ADVISORY products.
#' @param contents text of product
#' @keywords internal
crul_fstadv <- function(contents) {

  # Replace all carriage returns with empty string.
  contents <- stringr::str_replace_all(contents, "\r", "")

  # Make sure this is a public advisory product
  if (!any(stringr::str_count(contents,
                              c("MIATCM", "[W]*TPA", "TCMAT", "WTPZ",
                                "HFOTCMEP", "HFOTCMCP"))))
    stop(sprintf("Invalid Forecast/Advisory link. %s", link))

  df <- create_df_fstadv()

  status <- scrape_header(contents, ret = "status")
  name <- scrape_header(contents, ret = "name")
  adv <- scrape_header(contents, ret = "adv") %>% as.numeric()
  date <- scrape_header(contents, ret = "date")

  if (getOption("rrricanes.working_msg"))
    message(sprintf("Working %s %s Forecast/Advisory #%s (%s)",
                    status, name, adv, date))

  key <- scrape_header(contents, ret = "key")
  lat <- fstadv_lat(contents)
  lon <- fstadv_lon(contents)
  posacc <- fstadv_pos_accuracy(contents)
  fwd_dir <- fstadv_fwd_dir(contents)
  fwd_speed <- fstadv_fwd_speed(contents)
  pressure <- fstadv_pressure(contents)
  eye <- fstadv_eye(contents)
  wind <- fstadv_winds(contents)
  gust <- fstadv_gusts(contents)

  df <- df %>%
    tibble::add_row("Status" = status, "Name" = name, "Adv" = adv,
                    "Date" = date, "Key" = key, "Lat" = lat,
                    "Lon" = lon, "Wind" = wind, "Gust" = gust,
                    "Pressure" = pressure, "PosAcc" = posacc,
                    "FwdDir" = fwd_dir, "FwdSpeed" = fwd_speed,
                    "Eye" = eye)

  # Add current wind radius
  wind_radius <- fstadv_wind_radius(contents, wind)

  df[, names(wind_radius)] <- wind_radius[, names(wind_radius)]

  # Add current sea radius
  seas <- fstadv_seas(contents, wind)
  # AL161999 has two rows of Seas data for advisory 5. The second row is
  # within the forecast area where typically does not exist. Assumption is a
  # typo. Would like to save this into attributes or something...
  if (all(!is.null(seas), nrow(seas) > 1)) {
    warning(sprintf("Too many rows of sea data for %s %s #%s.\n%s",
                    status, name, adv, seas[2:nrow(seas),]),
            call. = FALSE)
    seas <- seas[1,]
  }

  df[, names(seas)] <- seas[, names(seas)]

  # Add forecast positions and wind radii
  forecasts <- fstadv_forecasts(contents, date)

  df[, names(forecasts)] <- forecasts[, names(forecasts)]

  return(df)
}

#' @title crul_get_discus
#' @param links URLs to cyclone archive pages
#' @export
crul_get_discus <- function(links) {
  df <- crul_get_storm_data(links, products = "discus")
  return(df$discus)
}

#' @title crul_get_fstadv
#' @param links URLs to cyclone archive pages
#' @export
crul_get_fstadv <- function(links) {
  df <- crul_get_storm_data(links, products = "fstadv")
  return(df$fstadv)
}

#' @title crul_get_posest
#' @param links URLs to cyclone archive pages
#' @export
crul_get_posest <- function(links) {
  df <- crul_get_storm_data(links, products = "posest")
  return(df$posest)
}

#' @title crul_get_prblty
#' @param links URLs to cyclone archive pages
#' @export
crul_get_prblty <- function(links) {
  df <- crul_get_storm_data(links, products = "prblty")
  return(df$prblty)
}

#' @title crul_get_products
#' @description Retrieve a list of one or multiple products from a cyclones
#' archive page
#' @param links links to storm archive page
#' @export
crul_get_products <- function(links) {

  # Get year
  year <- extract_year_archive_link(link)

  nhc_url <- get_nhc_link(withTrailingSlash = FALSE)

  # Depending on the year these URL's are formatted various ways.
  if (year == 1998) {
    products <- stringr::str_c(nhc_url, '/archive/', year, '/', products)
  } else {
    products <- stringr::str_c(nhc_url, products)
  }

  return(products)
}

#' @title crul_get_public
#' @param links URLs to cyclone archive pages
#' @export
crul_get_public <- function(links) {
  df <- crul_get_storm_data(links, products = "public")
  return(df$public)
}

#' @title crul_get_storm_data
#' @description Retrieve storm data for specified products
#' @param links Links to a cyclones archive pages
#' @param products c("discus", "fstadv", "posest", "public", "prblty", "update",
#' "wndprb")
#' @export
crul_get_storm_data <- function(links,
                                products = c("discus", "fstadv", "posest",
                                             "public", "prblty", "update",
                                             "wndprb")) {

  products <- match.arg(products, several.ok = TRUE)

  # There is an 80-hit/10-second limit to the NHC pages (note Issue #94), or 8
  # requests/second. The below request will process 4 links every 0.5 seconds.
  links <- split(links, ceiling(seq_along(links)/4))
  # Set progress bar
  p <- dplyr::progress_estimated(n = length(links))
  if (getOption("rrricanes.working_msg"))
    message("Gathering storm product links.")
  res <- purrr::map(links, .f = function(x) {crul_get_url_contents(x, p)}) %>%
    purrr::flatten()

  # Get contents of all storms
  storm_contents <- purrr::map(res, ~.$parse("UTF-8")) %>%
    purrr::map(xml2::read_html)

  years <- purrr::map(res, ~.$url) %>%
    purrr::flatten_chr() %>%
    stringr::str_extract("[[:digit:]]{4}") %>%
    as.numeric()

  # Extract all links
  product_links <- purrr::map(storm_contents,
                              ~rvest::html_nodes(x = .x, xpath = "//td//a")) %>%
    purrr::map(~rvest::html_attr(x = .x, name = "href"))

  # 1998 product links are relative and prefixed with "/archive/1998/" whereas
  # other years, product_links are absolute. If product_links exist for 1998
  # they must be modified. After, all product_links must be prefixed with
  # nhc_domain
  nhc_domain <- get_nhc_link(withTrailingSlash = FALSE)
  product_links[years == 1998] <- purrr::map(product_links[years == 1998],
                                             ~sprintf("/archive/1998/%s", .))
  product_links <- purrr::map(product_links, ~sprintf("%s%s", nhc_domain, .))

  # Filter links based on products and make one-dimensional
  product_links <- purrr::invoke_map(.f = sprintf("filter_%s", products),
                                     .x = list(list(links = product_links))) %>%
    purrr::map(purrr::flatten_chr) %>%
    purrr::flatten_chr()

  # Loop through each product getting link contents
  if (getOption("rrricanes.working_msg"))
    message("Working individual products.")
  product_links <- split(product_links, ceiling(seq_along(product_links)/4))
  # set progress
  p <- dplyr::progress_estimated(length(product_links))
  res <- purrr::map(product_links, crul_get_url_contents, p) %>%
    purrr::flatten()
  #res_parsed <- purrr::map(res, ~.$parse("UTF-8"))
  # res_parsed <- purrr::map(res, ~xml2::read_html(.$content)) %>%
  #   purrr::map(rvest::html_nodes, xpath = "//pre") %>%
  #   purrr::map(rvest::html_text)
  res_parsed <- purrr::map(res, ~xml2::read_html(.$content)) %>%
    purrr::map(.f = function(x) {
      if (is.na(txt <- rvest::html_node(x, xpath = "//pre") %>% rvest::html_text()))
          txt <- rvest::html_text(x)
      return(txt)
    })

  list_products <- list(
    "discus" = purrr::map(res, ~.$url) %>%
      filter_discus() %>%
      purrr::map(~(!purrr::is_empty(.))) %>%
      purrr::flatten_lgl() %>% res_parsed[.],
    "fstadv" = purrr::map(res, ~.$url) %>%
      filter_fstadv() %>%
      purrr::map(~(!purrr::is_empty(.))) %>%
      purrr::flatten_lgl() %>%
      res_parsed[.],
    "posest" = purrr::map(res, ~.$url) %>%
      filter_posest() %>%
      purrr::map(~(!purrr::is_empty(.))) %>%
      purrr::flatten_lgl() %>%
      res_parsed[.],
    "prblty" = purrr::map(res, ~.$url) %>%
      filter_prblty() %>%
      purrr::map(~(!purrr::is_empty(.))) %>%
      purrr::flatten_lgl() %>%
      res_parsed[.],
    "public" = purrr::map(res, ~.$url) %>%
      filter_public() %>%
      purrr::map(~(!purrr::is_empty(.))) %>%
      purrr::flatten_lgl() %>%
      res_parsed[.],
    "update" = purrr::map(res, ~.$url) %>%
      filter_update() %>%
      purrr::map(~(!purrr::is_empty(.))) %>%
      purrr::flatten_lgl() %>%
      res_parsed[.],
    "wndprb" = purrr::map(res, ~.$url) %>%
      filter_wndprb() %>%
      purrr::map(~(!purrr::is_empty(.))) %>%
      purrr::flatten_lgl() %>%
      res_parsed[.])

  empty_list_products <- purrr::map(list_products, ~!purrr::is_empty(.)) %>%
    purrr::flatten_lgl()

  filtered_list_products <- list_products[empty_list_products]

  ds <- purrr::map(products, .f = function(x) {
    purrr::invoke_map_df(.f = sprintf("crul_%s", x),
                         .x = filtered_list_products[[x]]) %>%
      dplyr::arrange(Date)
  })

  names(ds) <- names(filtered_list_products)

  return(ds)
}

#' @title crul_get_storms
#' @description Returns storms and product link.
#' @param years numeric or vector, four digits (\%Y format)
#' @param basins One or both of c("AL", "EP")
#' @export
crul_get_storms <- function(years = format(Sys.Date(), "%Y"),
                            basins = c("AL", "EP")) {

  years <- validate_year(years)

  # No archives earlier than 1998 for now
  if (any(years < 1998))
    stop('Archives currently only available for 1998 to current year.')

  if (!all(basins %in% c("AL", "EP")))
    stop("Basin must 'AL' and/or 'EP'")

  links <- purrr::map(years, .f = year_archives_link) %>%
    purrr::flatten_chr()

  # 1998 is only year with slightly different URL. Modify accordingly
  links[grep("1998", links)] <- paste0(links[grep("1998", links)],
                                       "1998archive.shtml")

  # There is an 80-hit/10-second limit to the NHC pages (note Issue #94), or 8
  # requests/second. The below request will process 4 links every 0.5 seconds.
  links <- split(links, ceiling(seq_along(links)/4))
  p <- dplyr::progress_estimated(n = length(links))
  contents <- purrr::map(links, .f = function(x) {crul_get_url_contents(x, p)}) %>%
    purrr::flatten()

  storm_df <- purrr::map_df(basins, crul_extract_storms, contents) %>%
    dplyr::group_by(Year, Basin) %>%
    dplyr::arrange(Year, Basin) %>%
    dplyr::ungroup()

  return(storm_df)
}

#' @title crul_get_update
#' @param links URLs to cyclone archive pages
#' @export
crul_get_update <- function(links) {
  df <- crul_get_storm_data(links, products = "update")
  return(df$update)
}

#' @title crul_get_url_contents
#' @description Get contents from URL
#' @param link URL to download
#' @export
crul_get_url_contents <- function(links, p) {
  p$pause(0.5)$tick()$print()
  links <- crul::Async$new(urls = links)
  res <- links$get()
  # Check status codes
  if (purrr::map(res, ~.$status_code) %>% purrr::flatten_dbl() %>% unique() != 200)
    warning("Bad status codes.")
  return(res)
}

#' @title crul_get_wndprb
#' @param links URLs to cyclone archive pages
#' @export
crul_get_wndprb <- function(links) {
  df <- crul_get_storm_data(links, products = "wndprb")
  return(df$wndprb)
}

#' @title crul_posest
#' @description Extrapolate data from Position Estimate products.
#' @param contents URL of a specific position estimate product
#' @export
crul_posest <- function(contents) {

  # Replace all carriage returns with empty string.
  contents <- stringr::str_replace_all(contents, "\r", "")

  # Make sure this is a public advisory product
  if (!any(stringr::str_count(contents,
                              c("MIATCE", "MEATIEST", "WTNT"))))
    stop(sprintf("Invalid Position Estimate link. %s", link))

  df <- create_df_posest()

  status <- scrape_header(contents, ret = "status")
  name <- scrape_header(contents, ret = "name")
  date <- scrape_header(contents, ret = "date")

  if (getOption("rrricanes.working_msg"))
    message(sprintf("Working %s %s Position Estimate #%s (%s)",
                    status, name, date))

  df <- df %>%
    tibble::add_row("Status" = status,
                    "Name" = name,
                    "Date" = date,
                    "Contents" = contents)

  return(df)
}

#' @title crul_prblty
#' @description Parse strike probability products
#' @param contents Link to a storm's specific strike probability advisory product.
#' @export
crul_prblty <- function(contents) {

  # Replace all carriage returns with empty string.
  contents <- stringr::str_replace_all(contents, "\r", "")

  # Make sure this is a strike probability product
  if (!any(stringr::str_count(contents, c("MIASPFAT", "MIASPFEP", "SPFAT",
                                          "MIAWRKSP"))))
    stop(sprintf("Invalid Strike Probability link. %s", link))

  status <- scrape_header(contents, ret = "status")
  name <- scrape_header(contents, ret = "name")
  adv <- scrape_header(contents, ret = "adv")
  date <- scrape_header(contents, ret = "date")

  if (getOption("rrricanes.working_msg"))
    message(sprintf("Working %s %s Strike Probability #%s (%s)",
                    status, name, adv, date))

  # 15.0N  43.4W      43  1  X  X 44   16.8N  48.2W       X  4 16  2 22
  # 15.8N  45.9W       1 26  1  X 28

  ptn <- paste0("(?<=[:blank:]{3}|\n)",
                "([[:alpha:][:digit:][:punct:][:blank:]]{17})",   # Location
                "[:blank:]+",                                     # Delimiter
                "([:digit:]{1,2}|X)",                             # A
                "[:blank:]+",                                     # Delimiter
                "([:digit:]{1,2}|X)",                             # B
                "[:blank:]+",                                     # Delimiter
                "([:digit:]{1,2}|X)",                             # C
                "[:blank:]+",                                     # Delimiter
                "([:digit:]{1,2}|X)",                             # D
                "[:blank:]+",                                     # Delimiter
                "([:digit:]{1,2}|X)")                             # E

  matches <- stringr::str_match_all(contents, ptn)

  prblty <- tibble::as_data_frame(matches[[1]])

  names(prblty) <- c("Del", "Location", "A", "B", "C", "D", "E")

  prblty$Del <- NULL

  # Trim whitespace
  prblty <- purrr::map_df(.x = prblty, .f = stringr::str_trim)

  # If no strike probabilities, return NULL
  if (nrow(prblty) == 0)
    return(NULL)

  # Many values will have "X" for less than 1% chance. Make 0
  prblty[prblty == "X"] <- 0

  # dplyr 0.6.0 renames .cols parameter to .vars. For the time being,
  # accomodate usage of both 0.5.0 and >= 0.6.0.
  if (packageVersion("dplyr") > "0.5.0") {
    prblty <- dplyr::mutate_at(.tbl = prblty,
                               .vars = c(2:6),
                               .funs = "as.numeric")
  } else {
    prblty <- dplyr::mutate_at(.tbl = prblty,
                               .cols = c(2:6),
                               .funs = "as.numeric")
  }

  prblty <- prblty %>%
    dplyr::mutate("Status" = status,
                  "Name" = name,
                  "Adv" = adv,
                  "Date" = date) %>%
    dplyr::select_("Status", "Name", "Adv", "Date", "Location", "A", "B",
                   "C", "D", "E") %>%
    dplyr::arrange_("Date", "Adv")

  return(prblty)

}

#' @title crul_public
#' @description Parse Public Advisory products
#' @param contents Link to a storm's specific public advisory product.
#' @export
crul_public <- function(contents) {

  # Replace all carriage returns with empty string.
  contents <- stringr::str_replace_all(contents, "\r", "")

  # Make sure this is a public advisory product
  if (!any(stringr::str_count(contents, c("MIATCP", "TCP", "WTPA",
                                          "MIAWRKAP"))))
    stop(sprintf("Invalid Public Advisory link. %s", link))

  df <- create_df_public()

  status <- scrape_header(contents, ret = "status")
  name <- scrape_header(contents, ret = "name")
  adv <- scrape_header(contents, ret = "adv")
  date <- scrape_header(contents, ret = "date")

  safely_scrape_header <- purrr::safely(scrape_header)
  key <- safely_scrape_header(contents, ret = "key")
  if (is.null(key$error)) {
    key <- key$result
  } else {
    key <- NA
  }

  if (getOption("rrricanes.working_msg"))
    message(sprintf("Working %s %s Public Advisory #%s (%s)",
                    status, name, adv, date))

  df <- df %>%
    tibble::add_row("Status" = status,
                    "Name" = name,
                    "Adv" = adv,
                    "Date" = date,
                    "Key" = key,
                    "Contents" = contents)

  return(df)
}

#' @title crul_update
#' @description Parse cyclone update products
#' @param contents Link to a storm's specific update advisory product.
#' @export
crul_update <- function(contents) {

  # Replace all carriage returns with empty string.
  contents <- stringr::str_replace_all(contents, "\r", "")

  # Make sure this is a update advisory product
  if (!any(stringr::str_count(contents, c("MIATCU", "TCU", "WTNT"))))
    stop(sprintf("Invalid Cyclone Update link. %s", link))

  df <- create_df_update()

  status <- scrape_header(contents, ret = "status")
  name <- scrape_header(contents, ret = "name")
  date <- scrape_header(contents, ret = "date")

  safely_scrape_header <- purrr::safely(scrape_header)
  key <- safely_scrape_header(contents, ret = "key")
  if (is.null(key$error)) {
    key <- key$result
  } else {
    key <- NA
  }

  if (getOption("rrricanes.working_msg"))
    message(sprintf("Working %s %s Update #%s (%s)",
                    status, name, date))

  df <- df %>%
    tibble::add_row("Status" = status,
                    "Name" = name,
                    "Date" = date,
                    "Key" = key,
                    "Contents" = contents)

  return(df)
}

#' @title crul_wndprb
#' @description Parse wind probability products
#' @param contents Link to a storm's specific wind probability product.
#' @export
crul_wndprb <- function(contents) {

  # Replace all carriage returns with empty string.
  contents <- stringr::str_replace_all(contents, "\r", "")

  # Make sure this is a wndprb advisory product
  if (!any(stringr::str_count(contents, c("MIAPWS", "PWS"))))
    stop(sprintf("Invalid Wind Probability link. %s", link))

  status <- scrape_header(contents, ret = "status")
  key <- scrape_header(contents, ret = "key")
  adv <- scrape_header(contents, ret = "adv") %>% as.numeric()
  date <- scrape_header(contents, ret = "date")
  name <- scrape_header(contents, ret = "name")

  if (getOption("rrricanes.working_msg"))
    message(sprintf("Working %s %s Wind Speed Probability #%s (%s)",
                    status, name, adv, date))

  ptn <- paste0("(?<=\n)", # Look-behind
                # Location - first value must be capital letter.
                "([:upper:]{1}[[:alnum:][:blank:][:punct:]]{14})",
                # Wind
                "([[:digit:]]{2})",
                # Wind12
                "[:blank:]+([:digit:]{1,2}|X)",
                # Delim
                "[:blank:]+",
                # Wind24
                "([:digit:]{1,2}|X)",
                # Wind24 cumulative
                "+\\([:blank:]*([:digit:]{1,2}|X)\\)",
                # Delim
                "[:blank:]+",
                # Wind36
                "([:digit:]{1,2}|X)",
                # Wind36 cumulative
                "+\\([:blank:]*([:digit:]{1,2}|X)\\)",
                # Delim
                "[:blank:]+",
                # Wind48
                "([:digit:]{1,2}|X)",
                # Wind48 cumulative
                "+\\([:blank:]*([:digit:]{1,2}|X)\\)",
                # Delim
                "[:blank:]+",
                # Wind72
                "([:digit:]{1,2}|X)",
                # Wind72 cumulative
                "+\\([:blank:]*([:digit:]{1,2}|X)\\)",
                # Delim
                "[:blank:]+",
                # Wind96
                "([:digit:]{1,2}|X)",
                # Wind96 cumulative
                "+\\([:blank:]*([:digit:]{1,2}|X)\\)",
                # Delim
                "[:blank:]+",
                # Wind120
                "([:digit:]{1,2}|X)",
                # Wind120 cumulative
                "+\\([:blank:]*([:digit:]{1,2}|X)\\)",
                # End
                "[[:blank:]\n]+")

  matches <- stringr::str_match_all(contents, pattern = ptn)

  # Load matches into dataframe
  wndprb <- tibble::as_data_frame(matches[[1]][,2:16])

  # If only one row, need to transpose wndprb
  if (ncol(wndprb) == 1)
    wndprb <- wndprb %>% t() %>% tibble::as_data_frame()

  # If no wnd speed probabilities, return NULL
  if (nrow(wndprb) == 0)
    return(NULL)

  # Rename variables
  names(wndprb) <- c("Location", "Wind", "Wind12", "Wind24", "Wind24Cum",
                     "Wind36", "Wind36Cum", "Wind48", "Wind48Cum", "Wind72",
                     "Wind72Cum", "Wind96", "Wind96Cum", "Wind120",
                     "Wind120Cum")

  # Trim whitespace
  wndprb <- purrr::map_df(.x = wndprb, .f = stringr::str_trim)

  # Make "X" values 0
  wndprb[wndprb == "X"] <- 0

  # Make Wind:Wind120Cum numeric
  # dplyr 0.6.0 renames .cols parameter to .vars. For the time being,
  # accomodate usage of both 0.5.0 and >= 0.6.0.
  if (packageVersion("dplyr") > "0.5.0") {
    wndprb <- dplyr::mutate_at(.tbl = wndprb,
                               .vars = c(2:15),
                               .funs = "as.numeric")
  } else {
    wndprb <- dplyr::mutate_at(.tbl = wndprb,
                               .cols = c(2:15),
                               .funs = "as.numeric")
  }

  # Add Key, Adv, Date and rearrange.
  wndprb <- wndprb %>%
    dplyr::mutate("Key" = key,
                  "Adv" = adv,
                  "Date" = date) %>%
    dplyr::select_("Key:Date", "Location:Wind120Cum") %>%
    dplyr::arrange_("Key", "Date", "Adv")

  return(wndprb)
}
