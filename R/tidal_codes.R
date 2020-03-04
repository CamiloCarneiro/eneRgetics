#' Creates tidal codes
#'
#' Creates a numerical code for the tide state in relation to the closest low
#' tide peak.
#'
#' For each observation, the time difference to the nearest low tide is
#' calculated (in hours) and coded into a numerical category where 0
#' represents low tide and 6 high tide. If tide is ebbing, values are negative.
#' If tide is rising values are positive.
#' 
#' Note: To ensure tide code is calculated accurately for the observations given,
#' you must ensure that observation dates contain a time zone attribute. You can
#' define this with the field \code{tz} in \code{\link{as.POSIXct}}. For help with
#' accepted time zone codes, see \code{\link{OlsonNames}}.
#'
#' @param observations data to which tide code will be appended; must have a
#'   column named "date" with datetime info in POSIXct format.
#' @param tide_table a tide table upon tide codes will be calculated; must
#'   have a column named "date" with datetime info in POSIXct format and
#'   another column named "type" with values 'High' or 'Low'.
#' @param round_digits defines the accuracy of tide codes. If round_digits = 1
#'   (default), observations are coded per hour. If round_digits = 0.5, creates
#'   half-hour tide codes.
#' @return The \code{observations} data frame entered with a new column named
#'   'tide_code'.
#' @examples
#' tidalCodes(observations, tides_january)
#' @export

tidal_codes <- function(observations, tide_table, round_digits = 1) {

  # check data.frames are provided
  if (missing(observations) | missing(tide_table))
  {stop("Observations and/or tide_table is missing")
    }

  if(checkmate::testDataFrame(observations) == F)
  {stop("Observations data must be a data frame", call. = FALSE)
    }

  if(checkmate::testDataFrame(tide_table) == F)
  {stop("Tide table must be a data frame", call. = FALSE)
  }
  
  # create the code collumn in observations data frame
  observations$tide_code <- NA

  # identify datetime columns

  obs_dt_col<-sapply(observations,lubridate::is.POSIXct)
  tide_dt_col<-sapply(tide_table,lubridate::is.POSIXct)

  if(length(which(obs_dt_col==T))==0){
    stop("Failed to identify observation dates - no POSIXct data")
  }else{
    obs_dt<-which(obs_dt_col==T)
  }

  if(length(which(tide_dt_col==T))==0){
    stop("Failed to identify observation dates - no POSIXct data")
  }else{
    tide_dt<-which(tide_dt_col==T)
  }
  
  if(attr(observations[,obs_dt],"tzone")==""){
    warning("Time zone attribute is missing in Observations, assuming \"GMT\"")
    observations[,obs_dt]<-lubridate::with_tz(observations[,obs_dt],tzone="GMT")
  }
  
  if(attr(observations[,obs_dt],"tzone")!=attr(tide_table[,tide_dt],"tzone")){
    warning("Different time zones detected in observations and tide_table")
  }
  
  
  # loop to calculate the differences in relation to low tide
  for (i in 1:nrow(observations)) {
    # select only low tides
    low_tides <- tide_table[tide_table$type == "Low", ]
    # calculate the differences between the observation time and the values in the tides dataset
    ind <- as.numeric(difftime(observations[i, obs_dt], low_tides[,tide_dt], units = "hours"))
    # adritubte the code to the new column
    observations[i, "tide_code"] <- ind[which(abs(ind)==min(abs(ind)))][1]
  }
  # function to round values
  r_any <- function(x, accuracy, f=round) {f(x / accuracy) * accuracy}
  # round values
  observations$tide_code <- r_any(observations$tide_code, accuracy = round_digits)
  return(observations)
}
