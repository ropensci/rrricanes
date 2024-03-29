% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gis.R
\name{gis_windfield}
\alias{gis_windfield}
\title{gis_windfield}
\usage{
gis_windfield(key, advisory = as.character())
}
\arguments{
\item{key}{Key of storm (i.e., AL012008, EP092015)}

\item{advisory}{Advisory number. If NULL, all advisories are returned.
Intermediate advisories are acceptable.}
}
\description{
Advisory Wind Field and Forecast Wind Radii
}
\details{
Tropical Cyclone Advisory Wind Field
 http://www.nhc.noaa.gov/gis/archive_forecast_info_results.php?id=al14&year
 =2016
 http://www.nhc.noaa.gov/gis/forecast/archive/
Example file name: al012017_fcst_001.zip
[basin]{2}[year_num]{2}[year]{4}_fcst_[advisory]{3}.zip
Many storms do not appear to have this data; especially earlier.

Not all advisories will be available for storms. For example,
\href{http://www.nhc.noaa.gov/gis/archive_forecast_info_results.php?id=al14&year=2016}{Hurricane Matthew (AL142016)}
is missing several advisories.
}
\seealso{
\code{\link{gis_download}}
}
