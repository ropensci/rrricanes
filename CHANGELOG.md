# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/) and this project adheres to [Semantic Versioning](http://semver.org/).

## Unreleased

### Added
  - dplyr.progress_bar for all products
  - rrricanes.working_msg option to show current working advisory.

### Changed
  - `get_storm_data` now takes link as first parameter for chaining. Returns a list of dataframes for each product.

### Removed
  - `fstadv_split`. Dataframe can be split if desired by user. 

### Deprecated
  - NA

### Fixed
  - NA

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