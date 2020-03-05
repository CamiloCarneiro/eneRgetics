context("bmr")

test_that("bmr returns appropriate output", {
  expect_gt(bmr(350,"trop"),0)
}
)
