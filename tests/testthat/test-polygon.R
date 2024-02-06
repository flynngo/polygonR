test_that("Aggregate queries successfully", {
  expect_no_error(
    request_agg(
      ticker = "AAPL", multiplier = 1, timespan = "day", from = "2023-01-09",
      to = "2023-01-09", limit = 120)
    )
})

test_that("Error handling", {
  expect_error(
    request_agg(
      ticker = "AAPL", multiplier = 1, timespan = "day", from = "3023-01-09",
      to = "3023-01-09", limit = 120),
    "HTTP 403"
    )
  expect_error(
    request_agg(
      ticker = "AAPL", multiplier = 1, timespan = "day", from = "2023-01-09",
      to = "2023-01-09", limit = 120, api_key = "invalid key"),
    "HTTP 401"
    )
})
