% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_tides.R
\name{get_tides}
\alias{get_tides}
\title{Retrieves tide extremes data}
\usage{
get_tides(lat, lon, start_date, end_date, tzone = "GMT", API_key,
  stationDistance = 5)
}
\arguments{
\item{lat}{latitude coordinates in decimal degrees.}

\item{lon}{longitude coordinates in decimal degress.}

\item{start_date}{starting date of sampling period in 'yyyy-mm-dd' format.}

\item{end_date}{ending date of sampling period in 'yyyy-mm-dd' format.}

\item{tzone}{time zone for the sampling location. Default is "GMT".}

\item{API_key}{a string with the API key obtained from www.worldtides.info.}

\item{stationDistance}{maximum distance (in kilometers) from a tidal station. See Details.}
}
\value{
A dataframe with sampling location, date and time, height and type of tide extremes at the requested location.
}
\description{
\code{get_tides} retrieves information on low and high tides (time and height) for a given location
and time period from \url{www.worldtides.info}.
}
\details{
\code{get_tides} returns information on measurement location, date and time (GMT)
of tide extreme occurrence, type of tide extreme (*i.e.* low or high tide peak) and height of extreme
(meters) for the period requested.

To obtain an API key, registration on www.worldtides.info is required. Each account
provides a monthly trial period with 100 API calls; any further requests require the
purchase of credits or a subscription. Please see \url{https://www.worldtides.info/billing}
for more details.

A single API call retrieves up to 7 days of tide extremes data. If more than 7 days of tide
extremes data are required, the number of API calls will be specified accordingly
internally. Take care while selecting dates to ensure control over API usage. To help doing so, 
the number of API calls is printed in the console with each function call.

The parameter \code{stationDistance} defines the maximum distance from which to return tidal
data from a tidal station instead of using the global background data. When this parameter
is set to 0 no results from stations will be returned but only global background data.

Note that \code{tzone} is required to ensure data is returned for the appropriate time period
at the given location as the API works with UTC time (which is equal to the GMT time zone). This also ensures compatibility with
data retrieved by \code{\link{get_weather}}. For details on accepted timezones, see
\code{\link{OlsonNames}}.

For more details regarding API usage and data quality, please visit \url{https://www.worldtides.info/apidocs}.
}
\examples{
## Pass a standard request to WorldTides API
get_tides(52.000, 3.283, "2018-01-01", "2018-01-03", "example_API_key")

## Obtaining data for a longer time period
get_tides(52.000, 3.283, "2018-01-01", "2018-01-31", "example_API_key")

## Obtaining data for a different time zone
## Change the acceptable station distance to within 10km
get_tides(-9.494, -35.567, "2018-01-01", "2018-01-03", "example_API_key",
tz="Brazil/East", stationDistance=10)

}
