# changes to original funtion (darksky::darksky_api_key()) in order to allow
# the API_key to be entered from the main function (with the original (which
# was interactive) it was just pasting the given value name)

darksky_api_key_altered<-function (API_key, force = TRUE) {
  env <- Sys.getenv("DARKSKY_API_KEY")
  if (!identical(env, "") && !force)
    return(env)
  env <- Sys.getenv("FORECASTIO_API_KEY")
  if (!identical(env, "") && !force) {
    message("FORECASTIO_API_KEY is deprecated, please update environment
              variable to DARKSKY_API_KEY")
    return(env)
  }
  if (!interactive()) {
    stop("Please set env var DARKSKY_API_KEY to your Dark Sky API key",
         call. = FALSE)
  }
  pat <- API_key
  if (identical(pat, "")) {
    stop("Dark Sky API key entry failed", call. = FALSE)
  }
  Sys.setenv(DARKSKY_API_KEY = pat)
  pat
}