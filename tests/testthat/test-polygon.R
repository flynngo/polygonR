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

test_that("Aggregates queries are successful", {
  expect_no_error(
    aggregates(
      ticker = "AAPL", multiplier = 1, timespan = "day", from = "2023-01-09",
      to = "2023-01-09", limit = 120
    )
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

test_that("Open/close queries are successful", {
  msft <- open_close(ticker = "MSFT", date = "2024-02-12")
  alizf <- open_close(ticker = "ALIZF", date = "2024-02-01")

  msft_expected <- tibble::tibble(
    open = 420.56,
    high = 420.74,
    low = 414.75,
    close = 415.26,
    trade_volume = 21202920
    )
  alizf_expected <- tibble::tibble(
    open = 266.00,
    high = 266.00,
    low = 262.00,
    close = 262.08,
    trade_volume = 100,
    otc = TRUE
    )

  # Different sites list different trade volumes.
  tolerance <- c(open = .005, high = .005, low = .005, close = .005, trade_volume = 100) # nolint

  for (i in names(tolerance)) {
    expect_lte(abs(msft[, i] - msft_expected[i]), tolerance[i])
    expect_lte(abs(alizf[, i] - alizf_expected[i]), tolerance[i])
  }
  expect_equal(alizf[, "otc"], alizf_expected["otc"])
})

test_that("Previous close queries are successful", {
  expect_no_error(
    nflx <- prev_close("NFLX")
  )
  expect_no_error(
    danoy <- prev_close("DANOY")
  )
  expected_cols <- c(
    "ticker", "trade_volume", "volume_weighted", "open", "close", "high", "low",
    "time", "n", "otc"
    )
  # TODO: I'm planning to change column names, these tests will need to be updated accordingly
  expect_equal(colnames(nflx), expected_cols[-length(expected_cols)])
  expect_equal(colnames(danoy), expected_cols)

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
