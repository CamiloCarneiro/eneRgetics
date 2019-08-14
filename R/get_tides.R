#' Retrieves tide extremes data
#'
#' \code{get_tides} retrieves information on low and high tides (time and height) for a given location
#' and time period from \url{www.worldtides.info}.
#'
#' @param lat latitude coordinates in decimal degrees.
#' @param lon longitude coordinates in decimal degress.
#' @param start_date starting date of sampling period in 'yyyy-mm-dd' format.
#' @param end_date ending date of sampling period in 'yyyy-mm-dd' format.
#' @param tzone time zone for the sampling location. Default is "GMT".
#' @param API_key a string with the API key obtained from www.worldtides.info.
#' @param stationDistance maximum distance (in kilometers) from a tidal station. See Details.
#'
#' @details
#' \code{get_tides} returns information on measurement location, date and time (GMT)
#' of tide extreme occurrence, type of tide extreme (*i.e.* low or high tide peak) and height of extreme
#' (meters) for the period requested.
#'
#' To obtain an API key, registration on www.worldtides.info is required. Each account
#' provides a monthly trial period with 100 API calls; any further requests require the
#' purchase of credits or a subscription. Please see \url{https://www.worldtides.info/billing}
#' for more details.
#'
#' A single API call retrieves up to 7 days of tide extremes data. If more than 7 days of tide
#' extremes data are required, the number of API calls will be specified accordingly
#' internally. Take care while selecting dates to ensure control over API usage. To help doing so, 
#' the number of API calls is printed in the console with each function call.
#'
#' The parameter \code{stationDistance} defines the maximum distance from which to return tidal
#' data from a tidal station instead of using the global background data. When this parameter
#' is set to 0 no results from stations will be returned but only global background data.
#'
#' Note that \code{tzone} is required to ensure data is returned for the appropriate time period
#' at the given location as the API works with UTC time (which is equal to the GMT time zone). This also ensures compatibility with
#' data retrieved by \code{\link{get_weather}}. For details on accepted timezones, see
#' \code{\link{OlsonNames}}.
#'
#' For more details regarding API usage and data quality, please visit \url{https://www.worldtides.info/apidocs}.
#'
#' @return A dataframe with sampling location, date and time, height and type of tide extremes at the requested location.
#'
#' @examples
#' ## Pass a standard request to WorldTides API
#' get_tides(52.000, 3.283, "2018-01-01", "2018-01-03", "example_API_key")
#'
#' ## Obtaining data for a longer time period
#' get_tides(52.000, 3.283, "2018-01-01", "2018-01-31", "example_API_key")
#' 
#' ## Obtaining data for a different time zone
#' ## Change the acceptable station distance to within 10km
#' get_tides(-9.494, -35.567, "2018-01-01", "2018-01-03", "example_API_key",
#' tz="Brazil/East", stationDistance=10)
#'
#' @export

get_tides<-function(lat,lon,start_date,end_date,tzone="GMT",API_key,stationDistance=5){

  #Check coordinates are valid
  if(any(lat>=90|lat<=(-90)|lon>=180|lon<=(-180))){
    stop("Coordinates provided are not valid - should be decimal degress between -90/90 Lat and -180/180 Long", call. = FALSE)
  }

  if(any(is.numeric(lat)==F|is.numeric(lon)==F)){
    stop("Coordinates provided are not valid - should be decimal degress between -90/90 Lat and -180/180 Long", call. = FALSE)
  }
  
  if((tzone %in% OlsonNames())==F){
    stop("Time zone not recognized, please check OlsonNames() for accepted formats", call. = FALSE)
  }

  #Define call link
  API.link<-"https://www.worldtides.info/api?extremes"

    #Add coordinates
  API.link<-paste0(API.link,"&lat=",lat,"&lon=",lon)

  #Station distance
  if(is.numeric(stationDistance)==F){
    stop("Parameter value for stationDistance should be in numeric format", call. = FALSE)
  }else{
    API.link<-paste0(API.link,"&stationDistance=",stationDistance)
  }

  #Date of start for sampling period
  if(grepl("(\\d{4})-(\\d{2})-(\\d{2})",start_date)==F){
    stop("Parameter start_date should be provided and in \"yyyy-mm-dd\" format", call. = FALSE)
  }else{
    start.date.PSX<-as.POSIXct(start_date,format = "%Y-%m-%d", tz=tzone)
    API.link<-paste0(API.link,"&start=",as.numeric(start.date.PSX))
  }

  #Length of sampling period
  if(grepl("(\\d{4})-(\\d{2})-(\\d{2})",end_date)==F){
    stop("Parameter end_date should be provided and in \"yyyy-mm-dd\" format", call. = FALSE)
  }else{
    end.date.PSX<-as.POSIXct(end_date,format = "%Y-%m-%d", tz=tzone)
    end.date.PSX<-end.date.PSX+86399
    length<-difftime(end.date.PSX,start.date.PSX,units="secs")
    API.link<-paste0(API.link,"&length=",length)
  }

 #Defines maximum number of calls - Required if more than 336 points are requested
  maxcalls<-round(as.numeric(difftime(end.date.PSX,start.date.PSX,units="days")/7))+1
  API.link<-paste0(API.link,"&maxcalls=",maxcalls)
  
  #Add API key
  API.link<-paste0(API.link,"&key=",API_key)

  #Get URL info
  tides.url<-RCurl::getURL(API.link)
  js<-jsonlite::fromJSON(tides.url)

  if(js$status!=200){
    stop(js$error)
  }

  message(js$copyright)
  message(paste("Call count: ",js$callCount))

  tide.data<-data.frame(responseLon=js$responseLon,
                        responseLat=js$responseLat,
                        date=as.POSIXct(js$extremes$date,tz="GMT",format="%Y-%m-%dT%H:%M"),
                        js$extremes[,3:4])
  if(tzone!="GMT"){
    tide.data$date<-lubridate::with_tz(tide.data$date, tzone = tzone)
  }
  
  return(tide.data)
}
