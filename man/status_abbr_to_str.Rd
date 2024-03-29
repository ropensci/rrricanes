% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{status_abbr_to_str}
\alias{status_abbr_to_str}
\title{status_abbr_to_str}
\usage{
status_abbr_to_str(x)
}
\arguments{
\item{x}{character vector of status abbreviations}
}
\value{
character vector of strings
}
\description{
Convert Status abbreviation to string
}
\details{
Status abbreviations
\describe{
  \item{DB}{Disturbance (of any intensity)}
  \item{EX}{Extratropical cyclone (of any intensity)}
  \item{HU}{Tropical cyclone of hurricane intensity (> 64 knots)}
  \item{LO}{A low that is neither a tropical cyclone, a subtropical
        cyclone, nor an extratropical cyclone (of any intensity)}
  \item{SD}{Subtropical cyclone of subtropical depression intensity
        (< 34 knots)}
  \item{SS}{Subtropical cyclone of subtropical storm intensity
        (> 34 knots)}
  \item{TD}{Tropical cyclone of tropical depression intensity (< 34 knots)}
  \item{TS}{Tropical cyclone of tropical storm intensity (34-63 knots)}
  \item{WV}{Tropical Wave (of any intensity)}
}
}
\examples{
# Extratropical Cyclone
status_abbr_to_str("EX")

# Hurricane
status_abbr_to_str("HU")
}
\seealso{
\url{http://www.aoml.noaa.gov/hrd/hurdat/newhurdat-format.pdf}
}
