test_that("Aggregate queries successfully", {
  expect_no_error(
    request_agg(
      ticker = "AAPL", multiplier = 1, timespan = "day", from = "2023-01-09", to = "2023-01-09", limit = 120))
})
