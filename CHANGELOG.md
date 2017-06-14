# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/) and this project adheres to [Semantic Versioning](http://semver.org/).

## [0.2.0-alpha] yyyy-mm-dd

### Added
  - NA

### Changed
  - NA

### Removed
  - NA

### Deprecated
  - NA

### Fixed
  - NA

### Security
  - NA

## [0.1.3] 2017-06-11

### Added
  - `rrricanes.http_sleep` to control time to sleep between multiple HTTP requests.

### Changed
  - Update documentation for `get_fstadv`, `get_prblty`, `get_wndprb`, `tidy_fstadv`, `tidy_wr`, `tidy_fcst` and `tidy_fcst_wr`.

### Removed
  - NA

### Deprecated
  - NA

### Fixed
  - Correct `tidy_fcst` and `tidy_fcst_wr` when all forecast periods do not exist. Previously, was expected that all forecast fields would exist. This may not always be the case. Now works only available forecast periods. (#73)

### Security
  - NA

## [0.1.2] - 2017-06-08

### Added
  - dplyr.progress_bar for all products
  - rrricanes.working_msg option to show current working advisory.
  - `tracking_chart()` for a base world plot. `al_tracking_chart()` for chart centered on Atlantic basin. `ep_tracking_chart()` for chart centered on northeast Pacific.
  - `load_storm_data()` helps get datasets that have already been scraped and processed. Designed to make it more efficient to get data faster.
  - `status_abbr_to_str` converts storm status abbreviations (i.e., TD, TS, HU) to string.
  - `saffir` returns Saffir-Simpson classification of tropical cyclones; abbreviated.
  - `twoal` and `twoep` for Atlantic and east Pacific tropical weather outlooks.
  - Added options `rrricanes.http_timeout` and `rrricanes.http_attempts` to give user more control over failures.

### Changed
  - `get_storm_data` now takes link as first parameter for chaining. Returns a list of dataframes for each product.
  - `tidy_fstadv`, `tidy_wr`, `tidy_fcst` and `tidy_fcst_wr` have been added to replaced now-removed `fstadv_split()`.
  - Completed package top-level documentation.
  

### Removed
  - `fstadv_split`. Dataframe can be split if desired by user. 

### Deprecated
  - NA

### Fixed
  - Fix call to `get_storms` on some Linux distros which generated xpath_element fun error. (#67)
  - Fix call to `get_storm_data`. Issue similar to #67. (#68)
  - Fix call to `gis_wsp`. Call in `rvest::html_nodes` generated "xpath_attrib" error. Add test for `gis_wsp`. (#70)

### Security
  - NA

## [0.1.0] - 2017-05-12

### Added
  - `al_prblty_stations` Get list of locations for wind speed probabilities in Atlantic basin.
  - `cp_prblty_stations` Get list of locations for wind speed probabilities in central Pacific basin.
  - `fstadv_split` split dataframe returned from `fstadv()` to four narrow dataframes.
  - `get_discus` get storm discussions from a storm's archive page.
  - `get_fstadv` get forecast/advisory products from a storm's archive page.
  - `get_nhc_link` returns link to NHC homepage
  - `get_posest` get position estimates from a storm's archive page.
  - `get_prblty` get strike probabilities from a storm's archive page.
  - `get_products` get links to all products for a storm.
  - `get_public` get public advisory statements from a storm's archive page.
  - `get_storms` get a list for storms from a year's archive page.
  - `get_storm_data` get one or multiple products for a storm
  - `get_update` get updates from a storm's archive page.
  - `get_wndprb` get wind speed probabilities from a storm's archive page.
  - `knots_to_mph` Convert values from knots to mph (for wind and gust values).
  - `mb_to_in` convert barometric pressure from millibars to inches.
  - `wndprb` Access a specific wind speed probability for a storm.

### Changed
  - Correct version, CHANGELOG and NEWS from previous "release".

### Removed
  - NA

### Deprecated
  - NA

### Fixed
  - NA

### Security
  - NA
