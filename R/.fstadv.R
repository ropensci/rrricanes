#' @title fstadv
#' @description Extrapolate data from FORECAST/ADVISORY products. 
#' @param l URL of a FORECAST/ADVISORY product
#' @return boolean TRUE if no errors
#' @export
fstadv <- function(l) {
  
  if(!.status(l))
    stop(sprintf("Link unavailable. %d", l))
  
  contents <- l %>% 
    xml2::read_html()
  
  name <- scrape_header(contents, ret = "name")
  
  return(TRUE)

}

#' @title fstadv_advisories
#' @description Extract basic storm data from FORECAST/ADVISORY products
#' @param contents text content of FORECAST/ADVISORY
#' @return boolean
fstadv_advisories <- function(content) {
  
  key <- get_key(content)
  name <- get_storm_name(content)
  adv <- fstadv_adv_num(content)
  ob_date <- get_time_header(content)
  status <- fstadv_status(content)
  lat <- fstadv_lat(content)
  lon <- fstadv_lon(content)
  posacc <- fstadv_pos_accuracy(content)
  fwd_dir <- fstadv_fwd_dir(content)
  fwd_speed <- fstadv_fwd_speed(content)
  pressure <- fstadv_pressure(content)
  eye <- fstadv_eye(content)
  wind <- fstadv_winds(content)
  gust <- fstadv_gusts(content)
  
  df_advisories <<- df_advisories %>% 
    tibble::add_row('Key' = key,
                    'Name' = name, 
                    'Adv' = adv,
                    'ObDate' = ob_date,
                    'Status' = status,
                    'Lat' = lat,
                    'Lon' = lon,
                    'Wind' = wind,
                    'Gust' = gust, 
                    'Pressure' = pressure,
                    'PosAcc' = posacc,
                    'FwdDir' = fwd_dir,
                    'FwdSpeed' = fwd_speed,
                    'Eye' = eye)
  
  return(TRUE)
  
}

#' @title fstadv_adv_num
fstadv_adv_num <- function(content) {
  adv_num <- get_name_header(content, what = 'adv_num')
  return(as.numeric(adv_num))
}

#' @title fstadv_data
fstadv_data <- function(link, msg = FALSE) {
  
  if(msg)
    message(paste0('Working: ', link))
  
  # Get contents of link
  contents <- get_product_contents(link)
  
  # Initialize dataframes if don't exist
  if(!exists('df_advisories'))
    assign('df_advisories', create_df_advisories(), envir = .GlobalEnv)
  
  if(!exists('df_forecasts'))
    assign('df_forecasts', create_df_forecasts(), envir = .GlobalEnv)
  
  if(!exists('df_forecast_winds'))
    assign('df_forecast_winds', create_df_forecast_winds(), envir = .GlobalEnv)
  
  if(!exists('df_winds'))
    assign('df_winds', create_df_winds(), envir = .GlobalEnv)
  
  if(!exists('df_seas'))
    assign('df_seas', create_df_seas(), envir = .GlobalEnv)
  
  # Collect data
  advs <- fstadv_advisories(contents)
  fcsts <- fstadv_forecasts(contents)
  winds <- fstadv_wind_radius(contents)
  seas <- fstadv_seas(contents)
  
  return(TRUE)  
}

#' @title fstadv_extract_forecasts
#' @description Extract all forecast data from FORECAST/ADVISORY product#' 
#' @param content text of FORECAST/ADVISORY product
#' @return list of forecasts
fstadv_extract_forecasts <- function(content) {
  
  # First I want to get the groups of each FORECAST or OUTLOOK. I'll do some 
  # text manipulations to make pattern-finding easier.
  
  # In some texts there is a space character between the returns. In others, 
  # none. 
  a <- stringr::str_replace_all(content, '\n[:blank:]*?\n', '\t')
  
  # Now any newline characters I'll replace with a simple space
  b <- stringr::str_replace_all(a, '[\n]{1}', ' ')
  
  # Put the tab character back to newline
  c <- stringr::str_replace_all(b, '\t', '\n')
  
  # Remove any ellipses
  d <- stringr::str_replace_all(c, '\\.\\.\\.', ' ')
  
  # Some products have periods in them after KT or NW. Some don't. To be 
  # consistent, I'll remove them, too. But have to be careful not to remove 
  # those in Lat and Lon
  e <- stringr::str_replace_all(d, 'KT\\.', 'KT')
  f <- stringr::str_replace_all(e, 'NW\\.', 'NW')
  
  # Extract all lines that begin with FORECAST VALID or OUTLOOK VALID and end 
  # with newline character
  g <- stringr::str_match_all(f, (paste0('[:space:]*(?:FORECAST|OUTLOOK) VALID ', 
                                         '[[:alnum:] /\\.-]+')))
  
  # at this point we *should* have all of our forecasts. 
  h <- unlist(g)
  
  # Replace any extra verbiage like INLAND or EXTRATROPICAL
  i <- stringr::str_replace_all(h, '([E|W])[A-Z ]+(MAX WIND)', '\\1 \\2')
  
  return(i)
  
}

#' @title fstadv_fcst
#' @description Retrieve forecast data from FORECAST/ADVISORY products. Loads into 
#'   respective dataframes (df_forecasts, df_forecast_winds)
#' @param contents text content of FORECAST/ADVISORY
#' @return boolean
fstadv_forecasts <- function(content) {
  
  # At this point all I'm doing is extracting every subset that begins with 
  # either FORECAST VALID or OUTLOOK VALID
  ds <- fstadv_extract_forecasts(content)
  
  # Break forecasts into a list by observation time; Extracts date/time to 
  # gusts (keys 2:9) and then wind fields, if avail (key 10)
  ds <- fstadv_parse_forecasts(ds)
  
  # If no forecasts, exit gracefully
  if(length(ds) == 0) 
    return(TRUE)
  
  # Reformat to load into dataframe
  ds <- lapply(ds, data.frame, stringsAsFactors = FALSE)
  
  # Load only from X2 (Date) to X10 (Gust)
  df <- data.table::rbindlist(ds)[,2:11]
  # Rename
  data.table::setattr(df, 
                      'names', 
                      c('d', 'h', 'mi', 'lat', 'lath', 'lon', 'lonh', 'w', 'g', 'wf'))
  
  # Start cleaning up
  # ObDate
  # Get current observation date. Will need month
  ob_date <- get_time_header(content)
  ob_year <- lubridate::year(ob_date)
  ob_month <- lubridate::month(ob_date)
  ob_day <- lubridate::day(ob_date)
  
  df$d <- as.numeric(df$d) # Day
  df$h <- as.numeric(df$h) # Hour
  df$mi <- as.numeric(df$mi) # Minute
  df$lat <- as.numeric(df$lat) # Latitude
  df$lon <- as.numeric(df$lon) # Longitude
  df$w <- as.numeric(df$w) # Wind
  df$g <- as.numeric(df$g) # Gust
  
  # Here I account for the possibility of the forecast date crossing months 
  # or even years
  df <- df %>% 
    dplyr::mutate(mo = ifelse(d < ob_day, ob_month + 1, ob_month), 
                  y = ifelse(mo < ob_month, ob_year + 1, ob_year), 
                  FcstDate = lubridate::ymd_hm(paste(paste(y, mo, d, sep = '-'), 
                                                     paste(h, mi, sep = ':'), 
                                                     sep = ' ')))
  
  # Clean up lat, lath, lon, lonh. Though no storms should exist in southern 
  # hemisphere there will be some crossing from western hemi to east. 
  # Southern/Western hemi lat/lon are negative. Northern/Eastern are positive. 
  df <- df %>% 
    dplyr::mutate(Lat = ifelse(lath == 'S', lat * -1, lat), # Lat reformatted
                  Lon = ifelse(lonh == 'W', lon * -1, lon)) # Lon reformatted
  
  # Finish renaming/reclassifying vars Wind and Gust, add Key, Adv, ObDate
  df <- df %>% 
    dplyr::mutate(Wind = as.numeric(w), # reformatted
                  Gust = as.numeric(g), # reformatted
                  Key = get_key(content), 
                  Adv = fstadv_adv_num(content), 
                  ObDate = get_time_header(content))
  
  # Reorganize df into tmp, save df for forecast winds
  tmp <- df %>% 
    dplyr::select(Key, Adv, ObDate, FcstDate, Lat, Lon, Wind, Gust)
  
  # Add df to df_forecasts
  df_forecasts <<- dplyr::bind_rows(tmp, df_forecasts)
  
  # Start building dataframe for forecast winds
  tmp <- df %>% 
    dplyr::select(Key, Adv, ObDate, FcstDate, wf)
  
  # If there is data...
  if(nrow(tmp) > 0) {
    # Flush out empty rows
    tmp[tmp$wf == '',] <- NA
    tmp <- na.omit(tmp)
    
    # Trim any leading/trailling whitespace
    tmp$wf <- lapply(tmp$wf, trimws)
    
    # Pad tmp$wf if missing one of the (64|50) fields (At this point we have at 
    # least the 34 KT field). Not padding will generate 'Too few values' errors 
    # from tidyr::separate. So here I provide consistency. Instead of 64 and 50 
    # I've used 0 KT. The 0 will become NA making it easier to drop since invalid.
    tmp$wf <- lapply(tmp$wf, function(x) {
      if(nchar(x) == 29) {
        x <- paste0('0  KT   0NE   0SE   0SW   0NW 0  KT   0NE   0SE   0SW   0NW ', x)
      } else if (nchar(x) == 59) {
        x <- paste0('0  KT   0NE   0SE   0SW   0NW ', x)
      } else {
        x <- x
      }
    })
    
    # Replace consecutive blanks
    tmp$wf <- lapply(tmp$wf, stringr::str_replace_all, pattern = '[ ]+', replace = ' ')
    
    # Split out wind fields. 
    tmp <- tmp %>% 
      tidyr::separate(wf, 
                      into = c(paste0('X', seq(1:18))), 
                      sep = '[ ]+')
    
    # Move wind field vars beneath each other
    tmp <- data.table::rbindlist(list(dplyr::select(tmp, Key, Adv, ObDate, FcstDate, X1:X6), 
                                      dplyr::select(tmp, Key, Adv, ObDate, FcstDate, X7:X12), 
                                      dplyr::select(tmp, Key, Adv, ObDate, FcstDate, X13:X18)))
    
    # Drop X2 which is 'KT'
    tmp$X2 <- NULL
    
    # Rename X1:X5
    data.table::setnames(tmp, 
                         old = c('X1', 'X3', 'X4', 'X5', 'X6'), 
                         new = c('WindField', 'NE', 'SE', 'SW', 'NW'))
    
    # Strip NE, SE, SW, NW. Have to allow for typos, e.g., "SW" in the SE field
    tmp$NE <- as.numeric(stringr::str_replace_all(tmp$NE, '([:digit:]{1,2})[:alpha:]{2}+', '\\1'))
    tmp$SE <- as.numeric(stringr::str_replace_all(tmp$SE, '([:digit:]{1,2})[:alpha:]{2}+', '\\1'))
    tmp$SW <- as.numeric(stringr::str_replace_all(tmp$SW, '([:digit:]{1,2})[:alpha:]{2}+', '\\1'))
    tmp$NW <- as.numeric(stringr::str_replace_all(tmp$NW, '([:digit:]{1,2})[:alpha:]{2}+', '\\1'))
    tmp$WindField <- as.numeric(tmp$WindField)
    
    # Set NA where any vars == 0
    if(nrow(tmp) > 0) {
      tmp[tmp == 0] <- NA
      # Remove any rows where is.na(WindField)
      tmp <- tmp[complete.cases(tmp$WindField),]
      
    }
    
    if(nrow(tmp) > 0)
      # Bind tmp to df_forecast_winds
      df_forecast_winds <<- dplyr::bind_rows(tmp, df_forecast_winds)
  }
  
  return(TRUE)
  
}

#' @title fstadv_get_wind_radius
#' @description Extract wind radius obs if exists
#' 
#' Expects input like:
#'   64 KT  . 20NE  20SE   0SW   0NW. 50 KT  . 60NE  60SE  20SW   0NW. 
#' 
#' Input may be shorter or longer depending on strength of storm
#'   
#' @param x character one line of wind field observations
#' @param fld numeric one of 64, 50 or 34
#' @return list or NA
fstadv_get_wind_radius <- function(x, fld = NULL) {
  
  if(!is.numeric(fld)) {stop('\'fld\' must be numeric (64, 50, 34)')}
  
  # By this point we *should* have one line containing the entire wind field. 
  # Look for each fld (64, 50, 34) and extract.
  ptn <- paste0(fld, # Wind field of which we are searching for 
                ' KT[\\. ]+', 
                '([0-9]{1,3})NE', # Radius to the northeast
                '[ ]+([0-9]{1,3})SE', # Radius to the southeast
                '[ ]+([0-9]{1,3})SW', # Radius to the southwest
                '[ ]+([0-9]{1,3})NW', # Radius to the northwest
                '\\.')
  
  w <- unlist(stringr::str_match_all(x, ptn))
  
  # Check if list is empty
  if(length(w) > 0) {
    # We have a wind field
    wds <- list('NE' = as.numeric(w[2]), 
                'SE' = as.numeric(w[3]), 
                'SW' = as.numeric(w[4]), 
                'NW' = as.numeric(w[5]))
    return(wds)
  } else {
    return(NA)
  }
}

#' @title fstadv_parse_forecasts
#' @description Breaks forecast field strings into multidimensional list.
#' @details Expects input like:
#' 
#'   [1] "FORECAST VALID 10/1200Z 22.6N  85.0W MAX WIND  40 KT GUSTS  50 KT 34 KT 110NE   0SE   0SW   0NW"                                               
#'   
#'   [2] "FORECAST VALID 11/0000Z 25.0N  86.3W MAX WIND  45 KT GUSTS  55 KT 34 KT 120NE  70SE   0SW  70NW"                                               
#'   
#'   [3] "FORECAST VALID 11/1200Z 27.7N  87.9W MAX WIND  50 KT GUSTS  60 KT 50 KT  25NE  25SE   0SW  25NW 34 KT 130NE  80SE   0SW  80NW"                 
#'   
#'   [4] "FORECAST VALID 12/0000Z 30.5N  88.6W NEAR MS/AL COAST MAX WIND  55 KT GUSTS  65 KT 50 KT  35NE  35SE   0SW  25NW 34 KT 130NE 100SE  50SW  80NW"
#'   
#'   [5] "FORECAST VALID 13/0000Z 36.0N  87.0W MAX WIND  20 KT GUSTS  25 KT"                                                                             
#'   
#'   [6] "OUTLOOK VALID 14/0000Z 41.0N  82.5W MAX WIND  15 KT GUSTS  20 KT"                                                                              
#'   
#'   [7] "OUTLOOK VALID 15/0000Z DISSIPATED INLAND"  
#' 
#' The date, time, lat, lat hemi, lon, lon hemi, max wind, gusts and wind field, 
#' if avialable, will be grouped. This function will return a list with each 
#' field as it's own variable; for example:
#' 
#' [[1]]
#' 
#' [,1]                                                                                                [,2] [,3]   [,4]   [,5] [,6]  
#' 
#' [1,] "FORECAST VALID 10/1200Z 22.6N  85.0W MAX WIND  40 KT GUSTS  50 KT 34 KT 110NE   0SE   0SW   0NW" "10" "1200" "22.6" "N"  "85.0"
#' 
#' [,7] [,8] [,9] [,10]                           
#' 
#' [1,] "W"  "40" "50" " 34 KT 110NE   0SE   0SW   0NW"
#' 
#' So you can access date in x[[1]][,2], time in x[[1]][,2], etc.
#' 
#' There are some cases such as AL051999, Adv 1 that has a 50KT wind field but 
#' not a 34KT wind field which is unusual.
#' 
#' @param content is a list of forecasts/outlooks extracted from the advisory 
#' product.
fstadv_parse_forecasts <- function(content) {
  
  ptn <- paste0('^[:blank:]*[\n[:alpha:]]+[ ]+VALID[ ]+', 
                '([:digit:]{2})/([:digit:]{2})([:digit:]{2})Z', # Date/Hour/Minute
                '[ ]+([[:digit:]\\.]{3,4})([N|S])', # Lat
                '[ ]+([[:digit:]\\.]{3,5})([E|W])', # Lon
                # Following is option text; don't collect, e.g.:
                # POST-TROP/EXTRATROP
                '[:blank:]*(?:[[:alpha:]-/]+)*[:blank:]*', 
                '[:blank:]MAX WIND[ ]+([:digit:]{2,3})[ ]+KT', # Winds 
                '[:blank:]+GUSTS[ ]+([:digit:]{2,3})[ ]+KT', # Gusts
                '[:blank:]*(?:EXTRATROPICAL)*[:blank:]*', # Optional text; don't collect
                '[:blank:]*((?:[[:digit:]{2}[ ]+KT[ ]+[:alnum:][:blank:]]+)?$)') # Wind field, if avail
  
  x <- stringr::str_match_all(content, ptn)
  
  # Some data has forward and trailing blanks. Remove them
  x <- lapply(x, trimws)
  
  return(x)
  
}

#' @title fstadv_seas
#' @description There is only one line of sea data, 12FT seas in each quadrant. So this 
#'   should go easier than the wind fields
#' @param content text of product
#' @return boolean
fstadv_seas <- function(content) {
  
  # 12 FT SEAS..125NE  90SE  90SW 175NW.
  ptn <- paste0('12 FT SEAS[\\.[:blank:]]+', 
                '([0-9]{1,3})NE', 
                '[ ]+([0-9]{1,3})SE', 
                '[ ]+([0-9]{1,3})SW', 
                '[ ]+([0-9]{1,3})NW')
  
  x <- stringr::str_match_all(content, ptn)
  
  # If there is Seas data, continue, otherwise ignore
  if(length(x[[1]]) > 0) {
    
    # Load into dataframe and get advisory's Key, Adv and ObDate
    #    tmp <- as.data.frame(x[[1]][,2:5])
    tmp <- as.data.frame(x)
    tmp$Key = get_key(content)
    tmp$Adv = fstadv_adv_num(content)
    tmp$ObDate = get_time_header(content)
    
    # Rename X1:X5
    data.table::setnames(tmp, 
                         old = c('X2', 'X3', 'X4', 'X5'), 
                         new = c('NE', 'SE', 'SW', 'NW'))
    
    tmp <- tmp %>% 
      dplyr::select(Key, Adv, ObDate, NE, SE, SW, NW)
    
    # Reclass vars
    tmp$NE <- as.numeric(tmp$NE)
    tmp$SE <- as.numeric(tmp$SE)
    tmp$SW <- as.numeric(tmp$SW)
    tmp$NW <- as.numeric(tmp$NW)
    
    # Bind tmp to df_seas
    df_seas <<- dplyr::bind_rows(tmp, df_seas)
    
  }
  
  return(TRUE)
  
}

#' @title fstadv_status
fstadv_status <- function(content) {
  status <- get_name_header(content, what = 'status')
  return(status)
}

#' @title fstadv_wind_radius
#' @description Parse wind radius data from product, if exists. This is somewhat tricky as the 
#'   wind fields are 64KT, 50KT and 34KT and are listed in descending order. So, 
#'   the first line will not always be 64KT, 50KT or even 34KT depending on 
#'   strength of storm. What I do here is just extract the entire blob and work 
#'   through it. I'll continue to look for ways to improve it.
#' 
#' Complimentary to fstadv_get_wind_radius
#' 
#' @param contents text of product
#' @return dataframe
fstadv_wind_radius <- function(content) {
  
  ptn <- 'MAX SUSTAINED WINDS[[:blank:][:digit:]]+KT WITH GUSTS TO[[:blank:][:digit:]]+KT[\\.[:space:]]*([A-Z0-9\\. \n]+)12 FT SEAS'
  
  # Do some reformatting to make extraction easier
  a <- stringr::str_replace_all(content, '\n \n', '\t')
  b <- stringr::str_replace_all(a, '\n', ' ')
  c <- stringr::str_replace_all(b, '\t', '\n')
  d <- stringr::str_replace_all(c, '\\.\\.\\.', ' ')
  e <- unlist(stringr::str_match_all(d, ptn))
  # Isolate on key 2
  f <- stringr::str_replace_all(e[2], '\\.', '')
  g <- stringr::str_replace_all(f, '[KT|NE|SE|SW|NW]', '')
  h <- stringr::str_replace_all(trimws(g), '[ ]+', '\t')
  
  # move to dataframe
  df <- data.frame('text' = h)
  
  # split out df$text with generic cols for now.
  df <- df %>% 
    tidyr::separate(text, into = c(paste0('x', c(1:15))), sep = '\t', 
                    fill = 'right') # Suppress warnings
  
  # Consolidate each wind field
  df <- data.table::rbindlist(list(dplyr::select(df, x1:x5), 
                                   dplyr::select(df, x6:x10), 
                                   dplyr::select(df, x11:x15)))
  
  # Rename variables
  data.table::setnames(df, 
                       old = c('x1', 'x2', 'x3', 'x4', 'x5'), 
                       new = c('WindField', 'NE', 'SE', 'SW', 'NW'))
  
  # Now get advisory's Key, Adv and ObDate
  key <- get_key(content)
  adv <- fstadv_adv_num(content)
  ob_date <- get_time_header(content)
  
  # Add vars to df
  df$Key = key
  df$Adv = adv
  df$ObDate = ob_date
  
  # Reposition columns
  df <- df %>% 
    dplyr::select(Key, Adv, ObDate, WindField, NE, SE, SW, NW)
  
  # Remove any rows where is.na(WindField)
  df <- df[complete.cases(df$WindField),]
  
  # Add df to df_winds
  df_winds <<- dplyr::bind_rows(df, df_winds)
  
  return(TRUE)
  
}

