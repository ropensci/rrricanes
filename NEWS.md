# Hurricanes 0.1.1

## Major Changes

## Minor Changes

## Deprecated

# Hurricanes 0.1.0

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
