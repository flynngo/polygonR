test_that("Aggregate queries successfully", {
  expect_no_error(
    aggregate(
      ticker = "AAPL", multiplier = 1, timespan = "day", from = "2023-01-09",
      to = "2023-01-09", limit = 120
    )
  )
})

test_that("Error handling", {
  expect_error(
    aggregate(
      ticker = "AAPL", multiplier = 1, timespan = "day", from = "3023-01-09",
      to = "3023-01-09", limit = 120
    ),
    "HTTP 403"
  )
  expect_error(
    aggregate(
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
        "https://api.polygon.io/v2/aggs/ticker/AAPL/range/1/day/2023-01-09/2023-01-15",
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
      aggregate(
        ticker = "AAPL", multiplier = 1, timespan = "day", from = "2023-01-09",
        to = "2023-01-15", limit = 3
      )
    ),
    5
  )
  expect_warning(
    query(
      "https://api.polygon.io/v2/aggs/ticker/AAPL/range/1/day/2023-01-09/2023-01-15",
      params = list(
        adjusted = T,
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

test_that("Basic plan rate limit isn't hit", {
  skip("Is tested as a side-effect of previous tests.")
  expect_no_error({
    for (i in 1:5) {
      aggregate(
        ticker = "AAPL", multiplier = 1, timespan = "day", from = "2023-01-09",
        to = "2023-01-09", limit = 120, rate_limit = 5
      )
    }
  })
})
