# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/) and this project adheres to [Semantic Versioning](http://semver.org/).

## [issues_1] - 2016-11-28

### Added
  - `get_storms()` Access NHC annual archives and extract names of storms, year of development, basin and link to the storm's archive pages. Gather data into dataframe (skeleton created in `.create_df_archives()`.)
  - Add tests for `get_storms()`. Data for some tests exists in `tests/testthat/data` and created as of 2016-11-28.
  - Initialize `getting_started` vignette.

### Changed
  - NA

### Removed
  - NA

## [v0.0.0.9001] - yyyy-mm-dd

### Added
  - Package description.
  - `get_nhc_link()`
  - `month_str_to_num()`
  - `.status()`
  - `toproper()`
  - `.validate_year()`
  - `.extract_year_archive_link()`
  - `convert_lat_lon()`
  - `knots_to_mph()`
  - `mb_to_in()`
  - Tests for base functions listed above.

### Changed
  - NA

### Removed
  - NA
