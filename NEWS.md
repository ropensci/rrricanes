rrricanes 0.2.0-4 (2017-06-25)
==================================

### NEW FEATURES

* NA

### MINOR IMPROVEMENTS

* `Key` variable added to `discus` dataframes. `Key` will be NA for all cyclones >= 2005. Should not be <= 2006. (#80)
* Removed `Adv` variable from `posest` dataframes. Position estimates do not have advisory numbers. (#81)
* Removed `Adv` variable from `update`. Updates do not have advisory numbers. (#84)
* Added variable `Key` to `get_public` dataframes. (#85)
* Added variable `Key` to `get_update` dataframes. (#86)
* Removed non-existent wind radii variables in `get_fstadv`. Hrs 48 and 72 hours only have 34 and 50kt wind fields. Hrs 96 and 120 have none. (#89)

### BUG FIXES

* NA

### DEPRECATED AND DEFUNCT

* NA

rrricanes 0.2.0-3 (2017-06-22)
==================================

### NEW FEATURES

* NA

### MINOR IMPROVEMENTS

* Examples for functions `knots_to_mph`, `mb_to_in`, `status_abbr_to_str`, `get_discus`, `get_fstadv`, `tidy_fstadv`, `tidy_wr`, `tidy_fcst` and `tidy_fcst_wr`.
* Minor documentation updates
* Small enhancements to tests, vignettes

### BUG FIXES

* NA

### DEPRECATED AND DEFUNCT

* NA

rrricanes 0.2.0-2 (2017-06-22)
==================================

### NEW FEATURES

* NA

### MINOR IMPROVEMENTS

* NA

### BUG FIXES

* Advisories now issued when tropical cyclone development is anticipated, but not yet occurred, and watches and warnings need to be issued. See AL022017, AL032017. 
* Added additional time zones (HDT, HST, MDT, MST)
* Appended additional headers (TCE, WTPA, MIATCM, MIAWRKAD1, MIAWRKAP)

### DEPRECATED AND DEFUNCT

* NA

rrricanes 0.2.0-1 (2017-06-16)
==================================

### NEW FEATURES

* GIS functions now available. Please note some of these products may not exist for every available cyclone/advisory.
 + `gis_advisory` Typically will include current and past track data, forecast track data, forecast cone (margin of error) and wind radius data. 
 + `gis_breakpoints` List of breakpoints typically used for watch/warning areas but is not a requirement.
 + `gis_latest` Retrieves the latest GIS products for all active storms. 
 + `gis_outlook` Retrives the latest tropical weather outlook in shapefile format.
 + `gis_prob_storm_surge` Probabilistic storm surge; a polygon dataset for psurge and esurge products with various criteria.
 + `gis_windfield` Wind radius datasets.
 + `gis_wsp` Wind speed probabilities
 + `gis_download` Use this function to download the URLs returned from the above functions.
 + `shp_to_df` added to convert lines and polygons spatial dataframes to dataframes. Points dataframes can be converted using `tibble::as_dataframe` (target the @data object).

### MINOR IMPROVEMENTS

* [Enhanced documentation](https://timtrice.github.io/rrricanes/) added online using `pkgdown`. 
* `load_storm_data` directly returns dataframes. Additionally, retrieval by basin and years removed in favor of importing complete product datasets. Additionally, documentation has been added to the website on [using data.world](https://timtrice.github.io/rrricanes/articles/articles/data_world.html) as a third option. The difference between these two options is `load_storm_data` will return complete datasets. Using data.world will allow users to write custom queries to retrieve data.  (#76)

### BUG FIXES

* NA

### DEPRECATED AND DEFUNCT

* Not yet deprecated but a warning that `al_prblty_stations`, `cp_prblty_stations` and `ep_prblty_stations` may be removed on a future release. (#46)
* Support for dplyr 0.5.0 will be removed in future releases in favor of dplyr 0.7.0.

rrricanes 0.1.3 (2017-06-11)
============================

### NEW FEATURES

* NA

### MINOR IMPROVEMENTS

* `rrricanes.http_sleep` to control time to sleep between multiple HTTP requests.
* Clarified documentation for `get_fstadv`, `get_prblty`, `get_wndprb`, `tidy_fstadv`, `tidy_wr`, `tidy_fcst` and `tidy_fcst_wr`.

### BUG FIXES

* `tidy_fcst` and `tidy_fcst_wr` would err if all forecast periods were not available for a cyclone. Functions now analyze dataframe to determine what forecast fields exist, then tidies based on the result. (#73)

### DEPRECATED AND DEFUNCT

* NA

rrricanes 0.1.2 (2017-06-08)
============================

### NEW FEATURES

* Changed name from `Hurricanes` to `rrricanes`.

* `get_storm_data` can now be chained to other commands and returns a list of dataframes.

* `load_storm_data` accesses pre-scraped datasets and returns requested products through the github repo `rrricanesdata`. This was done to make it quicker to get data. It should not be relied on to get the most immediate data for current storms. However, it should be fairly up-to-date. Original functions can be used if for some reason immediate data access is needed.

* `saffir` returns Saffir-Simpson classification of tropical cyclones; abbreviated.

* `status_abbr_to_str` converts storm status abbreviations (i.e., TD, TS, HU) to string.

* `twoal` and `twoep` parse tropical weather outlook XML files. Gives current status, if any, of areas of interest in either basin.

### MINOR IMPROVEMENTS

* Modified numerous regex patterns to ensure data quality.
* `tidy_fstadv`, `tidy_wr`, `tidy_fcst` and `tidy_fcst_wr` have been added to replaced now-removed `fstadv_split()`.
* Added loop to make multiple attempts at extracting contents from NHC archives. Options `rrricanes.http_timeout` and `rrricanes.http_attempts` added to give user more control over this. Default is 3 attempts with no more than 5 permitted.

### BUG FIXES

* Too many to recall. Apologies. 
* Call to `get_storms` on some linux distros generated xpath_element error. Corrected. (#67)
* Modify call to `get_storm_data`. Replaced css parameter in `rvest::html_nodes` calls with xpath parameter. Some products (notably, `get_prblty`) do not have a "pre" tag but are text documents (not HTML). Modified `scrape_contents` to return full contents if "pre" tag doesn't exist. Tested `get_discus` and `get_public`; no errors generated. (#68)

### DEPRECATED AND DEFUNCT

* `fstadv_split`. See MINOR IMPROVEMENTS for alternatives.

Hurricanes 0.1.0 (2017-05-12)
================

## Major new features

Retrieve all storm's for a given year (>=1998) and access data from a given storm's history. Can access "current" storm position, structure details, forecast, and discussions.

This release should be considered beta. While I've made every effort to ensure quality there may be an issue here or there. I will work on developing QA/QC scripts as time permits.

Please send any issues or questions to: https://github.com/timtrice/Hurricanes/issues.

### Getting Annual Storm Data

Use `get_storms` to access storm's for a given year.

### Getting Storm Data

Use `get_storm_data` to access one or multiple products for a specific storm.

#### Storm Discussions (discus)

Not parsed but contains technical information on the cyclone, development tendencies and forecast model tendencies.

#### Forecast/Advisory (fstadv)

Contains the meat of data. Current storm information, forecast information, wind and sea data. Can use `fstadv_split()` to break the wide dataframe to multiple, relational dataframes.

#### Position Estimate (posest)

Contains current position estimate for a given storm. Usually issued during threats to land. Not issued for all storms. Not parsed. 

#### Strike Probabilities (prblty)

Strike probabilities for given locations prior to 2006 (See Wind Speed Probabilities for >= 2006). 

#### Public Advisory (public)

General information on a storm. Not parsed.

#### Updates (update)

Quick information given when a storm is threatening or undergoes a significant change. Not issued for all storms.

#### Wind Probabilities (wndprb)

Replaced Strike Probabilities after the 2005 hurricane season. Lists the chances of a location seeing 34kt, 50kt and 64kt winds within a given time frame.
