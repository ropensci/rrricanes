# Hurricanes 0.0.0.9002

## Major new features

### Getting Annual Storm Data

`get_storms` Pass multiple years, basins to get names and archive links of storms.

### Getting Storm Data

`get_storm_data` Pass at least a link to the storm's archive page (from `get_storms`) and a product as listed below to get data from that product. Can pass multiple products, multiple links.

#### Storm Discussions (discus)

Extracts header data and returns content.

#### Forecast/Advisory (fstadv)

The fstadv dataframe now contains the following values:
  * Status
  * Name
  * Adv
  * Date
  * Key
  * Latitude
  * Longitude
  * Wind
  * Gust
  * Pressure
  * Position accuracy
  * Forward direction
  * Forward speed
  * Eye

#### Position Estimate (posest)

Extracts header data and returns content.

#### Public Advisory (public)

Extracts header data and returns content.

#### Strike Probabilities (prblty)

Extracts header data and returns content.

#### Updates (update)

Extracts header data and returns content.

#### Wind Probabilities (wndprb)

Extracts header data and returns content.

