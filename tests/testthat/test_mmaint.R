context("mmaint")

body_mass <- 460
weatherData <- weather_jan
tide_table <- tides_jan
timeBudget <- summary_data
breast_height <- 0.17

test_that("mmaint returns output", {
  expect_output(mmaint(weatherData, tide_table, timeBudget, breast_height, body_mass))
}
)
