#' Retrieves weather and irradiance data
#'
#' Retrieves average temperature (in celsius degrees), average wind speed (in
#' m/s) and global horizontal irradiance (in Wh/m^2) hourly data for a given
#' location and time period.
#'
#' This function uses resources from \pkg{darksky} and \pkg{camsRAD} and
#' provides a single output for user convenience, containing the data required
#' to input in \code{m_maint}. The geographical coverage is spatially
#' restricted from -66 to 66 degrees latitude and longitude, and has a temporal
#' coverage starting 2004-02-01 and ending 2 days prior to the data retrieval
#' following CAMS (Copernicus Atmosphere Monitoring Service) data coverage.

#' Temperature (at 183 cm ~ 6 ft above ground, in Celsius degrees) and wind
#' speed (at 10m above ground, in meters per second) are downloaded from DarkSky
#' (\url{https://darksky.net/dev/docs}). Registration in this platform is
#' required to obtain an API key. Each API key allows up to 1000 free calls each
#' day. Each day of hourly data corresponds to one call (\emph{e.g.} the period
#' 2015-05-01 to 2015-05-31 corresponds to 31 calls).
#' 
#' Global irradiation on horizontal plane at ground level (in Wh/m2) is obtained
#' from CAMS radiation service
#' (\url{http://www.soda-pro.com/en_GB/web-services/radiation/cams-radiation-service/info}).
#' Registration in this platform is also required. There is a limit of 25 calls
#' per day, but a single API call allows data collection for a large time period
#' (\emph{e.g.} the period between 2015-05-01 and 2015-05-31, requested at once,
#' corresponds to a single API call). Each measurement has a reliability score
#' which, in this case, corresponds to the proportion of satelite images
#' received in that hour, in relation to the maximum possible (please refer to
#' CAMS Radiation Service User's Guide for more information).
#' 
#' Note that \code{tzone} is required to ensure data is returned for the
#' appropriate time period at the given location as the API works with UTC time
#' (which is equal to the GMT time zone). This also ensures compatibility with
#' data retrieved by \code{\link{get_tides}}. For details on accepted timezones,
#' see OlsonNames.
#'
#' IMPORTANT NOTES: Even if data on temperature or wind speed do not exist for
#' the requested location, a DarkSky API call will nevertheless be used. Hence,
#' we advise users to first check data availability, by requesting data for a
#' subset of the period of interest before assembling the complete data. This is
#' to avoid using API calls and have no data returned.
#' 
#'
#' @param lat latitude coordinates in decimal degrees, between -66 to 66.
#' @param lon longitude coordinates in decimal degrees, between -66 to 66.
#' @param start_date starting date of sampling period in 'yyyy-mm-dd' format.
#' @param end_date ending date of sampling period in 'yyyy-mm-dd' format.
#' @param tzone time zone for the sampling location. Default is "GMT".
#' @param reli_score reliability score of irradiation data. Default is 0.5
#'   (other values will return NA). Please read CAMS Radiation Service User's
#'   Guide for more information.
#' @param API_key DarkSky API key.
#' @param cams_user e-mail address of CAMS radiation service account.
#' 
#' @return Returns a data frame with date and time, temperature,
#'   wind speed and global horizontal irradiation.
#'   
#' @examples
#' get_weather(52.970, 4.740, "2015-05-01", "2015-05-09", "GMT",
#' "write_API_key_here", "email_cams_user@here.com")
#' @export
#'
#'

get_weather<-function(lat, lon, start_date, end_date, tzone="GMT", reli_score = 0.5, API_key, cams_user){

  # check geographical and date limits (these are from CAMS)
  if (lat > 66 | lat < -66){
    stop("Latitude must be between -66 and 66", call. = FALSE)
  }

  if (lon > 66 | lon < -66){
    stop("Longitude must be between -66 and 66", call. = FALSE)
  }

  if (as.POSIXct(start_date, format = "%Y-%m-%d") < as.POSIXct("2004-02-01", format = "%Y-%m-%d")){
    stop("Start date must be 2004-02-01 or later", call. = FALSE)
  }

  if (as.POSIXct(end_date, format = "%Y-%m-%d") > as.POSIXct((Sys.Date()-2), format = "%Y-%m-%d")){
    stop("End date must be two days ago or earlier", call. = FALSE)
  }

  if (reli_score < 0 | reli_score > 1){
    stop("Reliability score must be between 0 and 1", call. = FALSE)
  }
  
  if((tzone %in% OlsonNames())==F){
    stop("Time zone not recognized, please check OlsonNames() for accepted formats", call. = FALSE)
  }

  darksky_api_key_altered(API_key)

  start_date.PSX<-as.POSIXct(start_date,format="%Y-%m-%d",tz=tzone)
  end_date.PSX<-as.POSIXct(end_date,format="%Y-%m-%d",tz=tzone)+86399
  
  # create a sequence for the period to be downloaded, in days
  a <- c(seq(as.Date(start_date.PSX,tz="GMT"), as.Date(end_date.PSX,tz="GMT"), "days"))

  # transform the above dates into a format to be used in the get_forecast_for() function below
  b <- as.data.frame(paste0(a, "T00:00:00Z"))

  # create an empty list into which data will be added during the following loop
  data <- list()
  for (i in 1:nrow(b)){
    tmp <- darksky::get_forecast_for(lat, lon, b[i, 1], units = "si",
                           language = "en", exclude = NULL, add_json = FALSE,
                           add_headers = FALSE)
    data[[paste(i)]] <- tmp[[1]]
  }
  
  # bind all the sub lists into a data frame
  weather <- dplyr::bind_rows(data)

  # check if weather variables are retirved for the given location. If not, location must be changed
  if (any("temperature" %in% names(weather) ==F | "windSpeed" %in% names(weather) ==F)){
    stop("Temperature and/or wind speed do not exist for that location.
         Please change coordinates", call. = FALSE)
  }

  # select only variables of interest and rename date+time and windSpeed
  weather <- weather[, c("time", "temperature", "windSpeed")]
  names(weather)<- c("date", "temperature", "wind_speed")
  

  # set CAMS user
  camsRad::cams_set_user(cams_user)

  # retrieve radiation data
  A <- capture.output(rad <- camsRad::cams_get_radiation( # "A<-capture.output" is to get the output massages and get into an object instead of being printed on the console
    lat = lat, lng = lon,
    date_begin = as.Date(start_date, tz="GMT"), date_end = as.Date(end_date, tz="GMT"),
    time_step = "PT01H"))
  rm(A) # remove A

  rad$timestamp <- rad$timestamp-3600 # minus 3600seconds (i.e. one hour), because the hour in the output was the end limit (e.g. for data colected between 00:00 and 01:00, timestamp would be 01:00)

  # rename date+time and reliability and select variables of interest
  rad <- rad[, c("timestamp", "GHI", "Reliability")]
  names(rad)<-c("date", "GHI", "reliability_radiation")
 
  
  # merge weather and radiation, using only radiation with reliability score > reli_score
  weatherdata <- merge(weather, rad[rad$reliability_radiation > reli_score,
                                   c("date", "GHI")], by = "date", all.x = T)
  weatherdata$date<-as.POSIXct(weatherdata$date, format="%Y-%m-%d %H:%:%S", tz="GMT")
  
  if(tzone!="GMT"){
    weatherdata$date<-lubridate::with_tz(weatherdata$date, tzone = tzone)
  }
  
  
  return(weatherdata)
}
