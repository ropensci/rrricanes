# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/) and this project adheres to [Semantic Versioning](http://semver.org/).

## [0.2.0-4] 2017-06-25

### Added
  - NA

### Changed
  - Added variable `Key` to `discus` dataframes. (#80)
  - Removed variable `Adv` from `posest`. Position estimates do not have advisory numbers. (#81)
  - Fix `scrape_adv_num` to accomodate possible "INTERMEDIATE" text in Public Advisory headers. (#83)
  - Remove variable `Adv` from `update`. Updates do not have advisory numbers. (#84)
  - Added variable `Key` to `get_public` dataframes. (#85)
  - Added variable `Key` to `get_update` dataframes. (#86)
  - Removed non-existent wind radii variables in `get_fstadv`. Hrs 48 and 72 hours only have 34 and 50kt wind fields. Hrs 96 and 120 have none. (#89)

### Removed
  - NA

### Deprecated
  - NA

### Fixed
  - NA

### Security
  - NA

## [0.2.0-3] 2017-06-22

### Added
  - Examples for functions `knots_to_mph`, `mb_to_in`, `status_abbr_to_str`, `get_discus`, `get_fstadv`, `tidy_fstadv`, `tidy_wr`, `tidy_fcst` and `tidy_fcst_wr`.

### Changed
  - Added data files to make building vignettes quicker.
  - Added `skip_on_cran` to tests. Additionally, slimmed down some tests. Previous tests exist in branch `tests` and will be redeveloped.
  - Minor documentation updates and corrections.

### Removed
  - NA

### Deprecated
  - NA

### Fixed
  - NA

### Security
  - NA

## [0.2.0-2] 2017-06-22

### Added
  - NA

### Changed
  - NA

### Removed
  - NA

### Deprecated
  - NA

### Fixed
  - Advisories now issued when tropical cyclone development is anticipated, but not yet occurred, and watches and warnings need to be issued. See AL022017, AL032017. 
  - Added additional time zones (HDT, HST, MDT, MST)
  - Appended additional headers (TCE, WTPA, MIATCM, MIAWRKAD1, MIAWRKAP)

### Security
  - NA

## [0.2.0-1] 2017-06-16

### Added
  - GIS functions `gis_advisory`, `gis_breakpoints`, `gis_latest`, `gis_outlook`, `gis_prob_storm_surge`, `gis_windfield` and `gis_wsp` added. These functions return one or more URLs to datasets that can be downloaded with `gis_download`.
  - `shp_to_df` added to convert lines and polygons spatial dataframes to dataframes. Points dataframes can be converted using `tibble::as_dataframe` (target the @data object).

### Changed
  - `load_storm_data` now returns full datasets from the `rrricanesdata` repo including tidied `fstadv` data. See documentation for notes on other products. (#76)

### Removed
  - NA

### Deprecated
  - Not yet deprecated but a warning that `al_prblty_stations`, `cp_prblty_stations` and `ep_prblty_stations` may be removed on a future release. (#46)

### Fixed
  - dplyr 0.6.0 has renamed the .cols parameter of `mutate_at` to .vars. Have modified pkg to accept both dplyr 0.5.0 and >= 0.6.0. This will be removed in future releases. (#74)

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
