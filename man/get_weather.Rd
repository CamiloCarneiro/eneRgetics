% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_weather.R
\name{get_weather}
\alias{get_weather}
\title{Retrieves weather and irradiance data}
\usage{
get_weather(lat, lon, start_date, end_date, tzone = "GMT",
  reli_score = 0.5, API_key, cams_user)
}
\arguments{
\item{lat}{latitude coordinates in decimal degrees, between -66 to 66.}

\item{lon}{longitude coordinates in decimal degrees, between -66 to 66.}

\item{start_date}{starting date of sampling period in 'yyyy-mm-dd' format.}

\item{end_date}{ending date of sampling period in 'yyyy-mm-dd' format.}

\item{tzone}{time zone for the sampling location. Default is "GMT".}

\item{reli_score}{reliability score of irradiation data. Default is 0.5
(other values will return NA). Please read CAMS Radiation Service User's
Guide for more information.}

\item{API_key}{DarkSky API key.}

\item{cams_user}{e-mail address of CAMS radiation service account.}
}
\value{
Returns a data frame with date and time, temperature,
  wind speed and global horizontal irradiation.
}
\description{
Retrieves average temperature (in celsius degrees), average wind speed (in
m/s) and global horizontal irradiance (in Wh/m^2) hourly data for a given
location and time period.
}
\details{
This function uses resources from \pkg{darksky} and \pkg{camsRAD} and
provides a single output for user convenience, containing the data required
to input in \code{m_maint}. The geographical coverage is spatially
restricted from -66 to 66 degrees latitude and longitude, and has a temporal
coverage starting 2004-02-01 and ending 2 days prior to the data retrieval
following CAMS (Copernicus Atmosphere Monitoring Service) data coverage.
Temperature (at 183 cm ~ 6 ft above ground, in Celsius degrees) and wind
speed (at 10m above ground, in meters per second) are downloaded from DarkSky
(\url{https://darksky.net/dev/docs}). Registration in this platform is
required to obtain an API key. Each API key allows up to 1000 free calls each
day. Each day of hourly data corresponds to one call (\emph{e.g.} the period
2015-05-01 to 2015-05-31 corresponds to 31 calls).

Global irradiation on horizontal plane at ground level (in Wh/m2) is obtained
from CAMS radiation service
(\url{http://www.soda-pro.com/en_GB/web-services/radiation/cams-radiation-service/info}).
Registration in this platform is also required. There is a limit of 25 calls
per day, but a single API call allows data collection for a large time period
(\emph{e.g.} the period between 2015-05-01 and 2015-05-31, requested at once,
corresponds to a single API call). Each measurement has a reliability score
which, in this case, corresponds to the proportion of satelite images
received in that hour, in relation to the maximum possible (please refer to
CAMS Radiation Service User's Guide for more information).

Note that \code{tzone} is required to ensure data is returned for the
appropriate time period at the given location as the API works with UTC time
(which is equal to the GMT time zone). This also ensures compatibility with
data retrieved by \code{\link{get_tides}}. For details on accepted timezones,
see OlsonNames.

IMPORTANT NOTES: Even if data on temperature or wind speed do not exist for
the requested location, a DarkSky API call will nevertheless be used. Hence,
we advise users to first check data availability, by requesting data for a
subset of the period of interest before assembling the complete data. This is
to avoid using API calls and have no data returned.
}
\examples{
get_weather(52.970, 4.740, "2015-05-01", "2015-05-09", "GMT",
"write_API_key_here", "email_cams_user@here.com")
}
