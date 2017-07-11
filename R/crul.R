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
  res <- crul_get_storm_data(links, products = "posest")
  return(res)
}

#' @title crul_get_prblty
#' @param links URLs to cyclone archive pages
#' @export
crul_get_prblty <- function(links) {
  res <- crul_get_storm_data(links, products = "prblty")
  return(res)
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
  res <- crul_get_storm_data(links, products = "public")
  return(res)
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
  res <- purrr::map(product_links, crul_get_url_contents, p) %>% purrr::flatten()
  res_parsed <- purrr::map(res, ~.$parse("UTF-8"))

  list_products <- list(
    "discus" = purrr::map(res, ~.$url) %>% filter_discus() %>% purrr::map(~(!purrr::is_empty(.))) %>% purrr::flatten_lgl() %>% res_parsed[.],
    "fstadv" = purrr::map(res, ~.$url) %>% filter_fstadv() %>% purrr::map(~(!purrr::is_empty(.))) %>% purrr::flatten_lgl() %>% res_parsed[.],
    "posest" = purrr::map(res, ~.$url) %>% filter_posest() %>% purrr::map(~(!purrr::is_empty(.))) %>% purrr::flatten_lgl() %>% res_parsed[.],
    "prblty" = purrr::map(res, ~.$url) %>% filter_prblty() %>% purrr::map(~(!purrr::is_empty(.))) %>% purrr::flatten_lgl() %>% res_parsed[.],
    "public" = purrr::map(res, ~.$url) %>% filter_public() %>% purrr::map(~(!purrr::is_empty(.))) %>% purrr::flatten_lgl() %>% res_parsed[.],
    "update" = purrr::map(res, ~.$url) %>% filter_update() %>% purrr::map(~(!purrr::is_empty(.))) %>% purrr::flatten_lgl() %>% res_parsed[.],
    "wndprb" = purrr::map(res, ~.$url) %>% filter_wndprb() %>% purrr::map(~(!purrr::is_empty(.))) %>% purrr::flatten_lgl() %>% res_parsed[.])

  empty_list_products <- purrr::map(list_products, ~!purrr::is_empty(.)) %>% purrr::flatten_lgl()

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
#' @param year numeric or vector, four digits (\%Y format)
#' @param basin One or both of c("AL", "EP")
#' @export
crul_get_storms <- function(year = format(Sys.Date(), "%Y"),
                            basin = c("AL", "EP")) {

  year <- validate_year(year)

  # No archives earlier than 1998 for now
  if (any(year < 1998))
    stop('Archives currently only available for 1998 to current year.')

  if (!all(basin %in% c("AL", "EP")))
    stop("Basin must 'AL' and/or 'EP'")

  links <- purrr::map(year, .f = year_archives_link) %>%
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

  storm_df <- purrr::map_df(basin, crul_extract_storms, contents) %>%
    dplyr::group_by(Year, Basin) %>%
    dplyr::arrange(Year, Basin) %>%
    dplyr::ungroup()

  return(storm_df)
}

#' @title crul_get_update
#' @param links URLs to cyclone archive pages
#' @export
crul_get_update <- function(links) {
  res <- crul_get_storm_data(links, products = "update")
  return(res)
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
  #purrr::map(res, ~.$status_code) %>% purrr::flatten_dbl() %>% unique()
  return(res)
}

#' @title crul_get_wndprb
#' @param links URLs to cyclone archive pages
#' @export
crul_get_wndprb <- function(links) {
  res <- crul_get_storm_data(links, products = "wndprb")
  return(res)
}
