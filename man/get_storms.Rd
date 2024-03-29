% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_storms.R
\name{get_storms}
\alias{get_storms}
\title{get_storms}
\format{
A 4xN dataframe
\describe{
  \item{Year}{Numeric, four-digit year of the storm}
  \item{Name}{Character, name of storm mixed-case}
  \item{Basin}{AL (Atlantic) or EP (East Pacific)}
  \item{Link}{URL to storms' product pages}
}
}
\source{
\url{http://www.nhc.noaa.gov/archive/2016/}
}
\usage{
get_storms(years = format(Sys.Date(), "\%Y"), basins = c("AL", "EP"))
}
\arguments{
\item{years}{numeric or vector, four digits (\%Y format)}

\item{basins}{One or both of c("AL", "EP")}
}
\value{
Dataframe of storms.
}
\description{
Returns storms and product link.
}
\details{
By default returns all storms for the current year. If no storms
have developed will return an empty dataframe.
}
\examples{
# Default. Get all storms, both basins, for last year.
\dontrun{
storms <- get_storms(year = 2016, basin = c("AL", "EP"))

# Get storms for two different years
storms.2010 <- get_storms(c(2010, 2015))

# Get storms for two consecutive years, Atlantic basin only
storms.al.2005 <- get_storms(2005:2007, basin = "AL")
}
}
