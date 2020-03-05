context("tidal_codes")

date <- seq.Date(as.Date("2017-01-05 05:57:00"), as.Date("2017-02-01 22:23:00"), by = 0.1)
observations <- data.frame(date = as.POSIXct(date, tz = "GMT"))


test_that("tidal_codes returns output", {
  expect_named(tidal_codes(observations, tides_jan), c("date","tide_code"))
}
)