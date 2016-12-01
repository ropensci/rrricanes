# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/) and this project adheres to [Semantic Versioning](http://semver.org/).

## [v0.0.0.9002] - yyyy-mm-dd

### Added
  - [8a687a2] Add filters to filter out specific product links from storm archive page.
  - [090ecff, c593e20] Add scraping functions, documentation.
  - [500aa41, 33db9df, d5d7aae, 63f0905, 51e316c, d48a153, 302d422] Add base functionality to parse public advisory data.
  - [d9a8e1d, 84e4d76, db910ae] Add base functionality to parse storm discussion data.
  - [cd0985a, e66c246, 290ff1a] Add base functionality to parse forecast/advisory products.
  - [5c06d98, 24a6ee2, be21d46, c54851d] Add base functionality for position estimate products.
  - [e217ee0] Add base functionality for strike probability products.
  - [8856799] Add base functionality for cyclone update products.
  - [8920800] Add base functionality for wind probability products.
  - [21930b6] Add wrapper function for products.
  - [409d00c] Add initial tests for `get_storm_data()`.

### Changed
  - [56c1e92] Allow for intermediate advisory numbers (See GH issue #3)
  - [c6840b5] Allow vector of links in product functions to work multiple storms. Addresses GH issue #4
  - [a0009a0] Add xml2 as package import; addresses GH issue #5
  - [2d9ede0] Removed documentation on obtaining storm data (resolves GH issue #6). Planned GH issue #9.
  - [30ee4fa] Correct parameter typos, add documentation for function parameters (GH issue #8).
  - [8f9da45] Address issue #11.
  - [12d70c2] Modify ptn_header in `scrape_header()`. Addresses issue #12.

### Removed
  - NA

## [issues_1] - 2016-11-28

### Added
  - `get_storms()` Access NHC annual archives and extract names of storms, year of development, basin and link to the storm's archive pages. Gather data into dataframe (skeleton created in `.create_df_archives()`.)
  - Add tests for `get_storms()`. Data for some tests exists in `tests/testthat/data` and created as of 2016-11-28.
  - Initialize `getting_started` vignette.

### Changed
  - NA

### Removed
  - NA

## [v0.0.0.9001] - 2016-11-28

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
