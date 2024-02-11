test_that("Aggregate queries successfully", {
  expect_no_error(
    aggregate(
      ticker = "AAPL", multiplier = 1, timespan = "day", from = "2023-01-09",
      to = "2023-01-09", limit = 120)
    )
})

test_that("Error handling", {
  expect_error(
    aggregate(
      ticker = "AAPL", multiplier = 1, timespan = "day", from = "3023-01-09",
      to = "3023-01-09", limit = 120),
    "HTTP 403"
    )
  expect_error(
    aggregate(
      ticker = "AAPL", multiplier = 1, timespan = "day", from = "2023-01-09",
      to = "2023-01-09", limit = 120, api_key = "invalid key"),
    "HTTP 401"
    )
})
# Run the extra requests needed to hit the rate_limit, I can reduce the number
# of loops and eventually remove or skip when I've added enough tests for over 6
# requests.
test_that("Basic plan rate limit isn't hit", {
  skip_on_cran()
  expect_no_error({
    for(i in 1:3) aggregate(
      ticker = "AAPL", multiplier = 1, timespan = "day", from = "2023-01-09",
      to = "2023-01-09", limit = 120, rate_limit = 5)
    })
  }
)
