test_that("Aggregates query is successful", {
  expect_no_error(
    aggregates(
      ticker = "AAPL", multiplier = 1, timespan = "day", from = "2023-01-09",
      to = "2023-01-09", limit = 120
    )
  )
})

test_that("Error handling", {
  expect_error(
    aggregates(
      ticker = "AAPL", multiplier = 1, timespan = "day", from = "3023-01-09",
      to = "3023-01-09", limit = 120
    ),
    "HTTP 403"
  )
  expect_error(
    aggregates(
      ticker = "AAPL", multiplier = 1, timespan = "day", from = "2023-01-09",
      to = "2023-01-09", limit = 120, api_key = "invalid key"
    ),
    "HTTP 401"
  )
})

test_that("query works iteratively", {
  expect_equal(
    length(
      query(
        "https://api.polygon.io/v2/aggs/ticker/AAPL/range/1/day/2023-01-09/2023-01-15", # nolint
        params = list(
          adjusted = TRUE,
          sort = "asc",
          limit = 3
        ),
        api_key = get_api_key(),
        rate_limit = 5,
        max_reqs = 2
      )
    ),
    2
  )
  expect_equal(
    nrow(
      aggregates(
        ticker = "AAPL", multiplier = 1, timespan = "day", from = "2023-01-09",
        to = "2023-01-15", limit = 3
      )
    ),
    5
  )
  expect_warning(
    query(
      "https://api.polygon.io/v2/aggs/ticker/AAPL/range/1/day/2023-01-09/2023-01-15", # nolint
      params = list(
        adjusted = TRUE,
        sort = "asc",
        limit = 1
      ),
      api_key = get_api_key(),
      rate_limit = 5,
      max_reqs = 2
    ),
    "Incomplete results were returned for query."
  )
})

test_that("Grouped daily queries are successful", {
  expect_no_error(
    grouped_daily(date = "2023-01-09")
  )
  expect_true(any(
    grouped_daily(date = "2023-01-09", include_otc = TRUE)$ticker == "ALIZF")
  )
  expect_false(any(
    grouped_daily(date = "2023-01-09", include_otc = FALSE)$ticker == "ALIZF")
  )
})

test_that("Basic plan rate limit isn't hit", {
  skip("Is tested as a side-effect of previous tests.")
  expect_no_error({
    for (i in 1:5) {
      aggregates(
        ticker = "AAPL", multiplier = 1, timespan = "day", from = "2023-01-09",
        to = "2023-01-09", limit = 120, rate_limit = 5
      )
    }
  })
})
