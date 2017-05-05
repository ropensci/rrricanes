# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/) and this project adheres to [Semantic Versioning](http://semver.org/).

## Unreleased

### Added
  - NA

### Changed
  - Changed or added `display_link` parameter to `msg`; when individual products are being pulled, if `msg` is TRUE then the link being worked will be displayed. Otherwise, it is hidden. This is for the following functions:
    + `get_storm_data`
    + `get_discus`
    + `discus`
    + `get_fstadv`
    + `fstadv`
    + `get_posest`
    + `posest`
    + `get_prblty`
    + `prblty`
    + `get_public`
    + `public`
    + `get_updates`
    + `updates`
    + `get_wndprb`
    + `wndprb`
  - Reassign default values of Date variables in built dataframes to POSIXct class.

### Removed
  - NA

### Deprecated
  - NA

### Fixed
  - `get_storms`; Removed error if no storms for current year. Displays message after each basin check.

### Security
  - NA