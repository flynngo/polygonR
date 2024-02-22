test_that("Error handling", {
  params <- list(
    adjusted = TRUE,
    sort = "asc",
    limit = 1
  )
  expect_error(
    query(
      paste0(
        base_url(),
        "/v2/aggs/ticker/AAPL/range/1/day/3023-01-09/3023-01-09"
      ),
      params = params,
      api_key = get_api_key(),
      rate_limit = 5
    ),
    "HTTP 403"
  )
  expect_error(
    query(
      paste0(
        base_url(),
        "/v2/aggs/ticker/AAPL/range/1/day/2023-01-09/2023-01-09"
      ),
      params = params,
      api_key = "invalid key",
      rate_limit = 5
    ),
    "HTTP 401"
  )
})

test_that("API key helper works", {
  new_key <- "ABC123"
  withr::with_envvar(
    new = c("POLYGON_KEY" = "NULL"),
    {
      expect_message(set_api_key(new_key), "POLYGON_KEY set")
      expect_identical(Sys.getenv("POLYGON_KEY"), new_key)
    }
  )
})

test_that("query works iteratively", {
  expect_identical(
    length(
      query(
        paste0(
          base_url(),
          "/v2/aggs/ticker/AAPL/range/1/day/2023-01-09/2023-01-15"
        ),
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
    2L
  )
  expect_warning(
    query(
      paste0(
        base_url(),
        "/v2/aggs/ticker/AAPL/range/1/day/2023-01-09/2023-01-15"
      ),
      params = list(
        adjusted = TRUE,
        sort = "asc",
        limit = 1
      ),
      api_key = get_api_key(),
      rate_limit = 5,
      max_reqs = 1
    ),
    "Incomplete results were returned for query."
  )
})

test_that("Stock aggregates queries are successful", {
  expect_no_error(
    aapl <- aggregates(
      ticker = "AAPL", multiplier = 1, timespan = "day", from = "2024-01-09",
      to = "2024-02-09", limit = 120
    )
  )
  expect_identical(
    colnames(aapl),
    c("ticker", "volume", "volume_weighted", "open", "close", "high", "low", "time", "transactions") # nolint
  )
  expect_no_error(
    amzn <- aggregates(
      ticker = "AMZN", multiplier = 1, timespan = "day", from = "2024-02-08",
      to = "2024-02-14", limit = 3
    )
  )
  expect_identical(nrow(amzn), 5L)
})

test_that("Options aggregates queries are successful", {
  expect_no_error(
    actual <- aggregates(
      ticker = "O:SPY251219C00650000", multiplier = 1, timespan = "day",
      from = "2024-01-09", to = "2024-02-09", limit = 120
    )
  )
  expect_identical(
    colnames(actual),
    c("ticker", "volume", "volume_weighted", "open", "close", "high", "low", "time", "transactions") # nolint
  )
})

test_that("Indices aggregates queries are successful", {
  expect_no_error(
    ndx <- aggregates(
      ticker = "I:NDX", multiplier = 1, timespan = "day",
      from = "2024-01-09", to = "2024-02-09", limit = 120
    )
  )
  expect_identical(
    colnames(ndx),
    c("ticker", "open", "close", "high", "low", "time")
  )
})


test_that("Forex aggregates queries are successful", {
  expect_no_error(
    eur_usd <- aggregates(
      ticker = "C:EURUSD", multiplier = 1, timespan = "day",
      from = "2024-01-09", to = "2024-02-09", limit = 120
    )
  )
  expect_identical(
    colnames(eur_usd),
    c("ticker", "volume", "volume_weighted", "open", "close", "high", "low", "time", "transactions") # nolint
  )
})


test_that("Crypto aggregates queries are successful", {
  expect_no_error(
    btc_usd <- aggregates(
      ticker = "X:BTCUSD", multiplier = 1, timespan = "day",
      from = "2024-01-09", to = "2024-02-09", limit = 120
    )
  )
  expect_identical(
    colnames(btc_usd),
    c("ticker", "volume", "volume_weighted", "open", "close", "high", "low", "time", "transactions") # nolint
  )
})

test_that("Stocks grouped daily queries are successful", {
  expect_no_error(
    gd_stocks1 <- grouped_daily(date = "2023-01-09", market = "stocks")
  )
  expect_no_error(
    gd_stocks2 <- stocks_daily(date = "2023-01-09")
  )
  expect_identical(gd_stocks1, gd_stocks2)
  expect_identical(
    colnames(gd_stocks1),
    c(
      "ticker", "volume", "volume_weighted", "open", "close", "high", "low",
      "time", "transactions"
    )
  )
  expect_true(
    any(
      "ALIZF" == stocks_daily(date = "2023-01-09", include_otc = TRUE)$ticker
    )
  )
  expect_false(
    any(
      "ALIZF" == stocks_daily(date = "2023-01-09", include_otc = FALSE)$ticker
    )
  )
})

test_that("Forex grouped daily queries are successful", {
  expect_no_error(
    gd_fx1 <- grouped_daily(date = "2023-01-09", market = "fx")
  )
  expect_no_error(
    gd_fx2 <- fx_daily(date = "2023-01-09")
  )
  expect_equal(gd_fx1, gd_fx2)
  expect_identical(
    colnames(gd_fx1),
    c(
      "ticker", "volume", "volume_weighted", "open", "close", "high", "low",
      "time", "transactions"
    )
  )
})

test_that("Crypto grouped daily queries are successful", {
  expect_no_error(
    gd_crypto1 <- grouped_daily(date = "2023-01-09", market = "crypto")
  )
  expect_no_error(
    gd_crypto2 <- crypto_daily(date = "2023-01-09")
  )
  expect_equal(gd_crypto1, gd_crypto2)
  expect_identical(
    colnames(gd_crypto1),
    c(
      "ticker", "volume", "volume_weighted", "open", "close", "high", "low",
      "time", "transactions"
    )
  )
})

test_that("Stocks open/close queries are successful", {
  msft <- open_close(ticker = "MSFT", date = "2024-02-12")
  alizf <- open_close(ticker = "ALIZF", date = "2024-02-01")

  msft_expected <- tibble::tibble(
    open = 420.56,
    high = 420.74,
    low = 414.75,
    close = 415.26,
    volume = 21202920
  )
  alizf_expected <- tibble::tibble(
    open = 266.00,
    high = 266.00,
    low = 262.00,
    close = 262.08,
    volume = 100,
    otc = TRUE
  )

  # Different sites list different trade volumes.
  tol <- c(open = .005, high = .005, low = .005, close = .005, volume = 100)

  for (i in names(tol)) {
    expect_lte(abs(msft[, i] - msft_expected[i]), tol[i])
    expect_lte(abs(alizf[, i] - alizf_expected[i]), tol[i])
  }
  expect_identical(alizf[, "otc"], alizf_expected["otc"])
})

test_that("Options open/close queries are successful", {
  actual <- open_close(ticker = "O:SPY251219C00650000", date = "2024-02-12")
  expected <- tibble::tibble(
    open = 4.74,
    high = 5.0,
    low = 4.74,
    close = 5.0
  )
  tol <- .005
  for (i in names(expected)) {
    expect_lte(abs(actual[, i] - expected[, i]), .005)
  }
})

test_that("Indices open/close queries are successful", {
  actual <- open_close(ticker = "I:NDX", date = "2024-02-12")
  expected <- tibble::tibble(
    open = 17942.28,
    high = 18041.45,
    low = 17859.66,
    close = 17882.66
  )
  tol <- .005
  for (i in names(expected)) {
    expect_lte(abs(actual[, i] - expected[, i]), tol)
  }
})

test_that("Crypto open/close queries are successful", {
  actual <- open_close(ticker = "X:BTCUSD", date = "2024-02-12")
  expected <- tibble::tibble(
    open = 48321.14,
    high = 50363.42,
    low = 47642,
    close = 49941.81
  )
  tol <- .005
  for (i in names(expected)) {
    expect_lte(abs(actual[, i] - expected[, i]), tol)
  }
})

test_that("Stocks previous close queries are successful", {
  expect_no_error(
    nflx <- prev_close("NFLX")
  )
  expect_no_error(
    danoy <- prev_close("DANOY")
  )
  expect_identical(
    colnames(nflx),
    c(
      "ticker", "volume", "volume_weighted", "open", "close", "high", "low",
      "time", "transactions"
    )
  )
  expect_identical(
    colnames(danoy),
    c(
      "ticker", "volume", "volume_weighted", "open", "close", "high", "low",
      "time", "transactions", "otc"
    )
  )
})

test_that("Options previous close queries are successful", {
  expect_no_error(
    actual <- prev_close("O:SPY251219C00650000")
  )
  expect_identical(
    colnames(actual),
    c(
      "ticker", "volume", "volume_weighted", "open", "close", "high", "low",
      "time", "transactions"
    )
  )
})

test_that("Indices previous close queries are successful", {
  expect_no_error(
    actual <- prev_close("I:NDX")
  )
  expect_identical(
    colnames(actual),
    c("ticker", "open", "close", "high", "low", "time")
  )
})

test_that("Forex previous close queries are successful", {
  expect_no_error(
    actual <- prev_close("C:EURUSD")
  )
  expect_identical(
    colnames(actual),
    c(
      "ticker", "volume", "volume_weighted", "open", "close", "high", "low",
      "time", "transactions"
    )
  )
})

test_that("Crypto previous close queries are successful", {
  expect_no_error(
    actual <- prev_close("X:BTCUSD")
  )
  expect_identical(
    colnames(actual),
    c(
      "ticker", "volume", "volume_weighted", "open", "close", "high", "low",
      "time", "transactions"
    )
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

test_that("Ticker market detection", {
  ticker_markets <- tibble::tribble(
    ~ticker, ~expected,
    "X:BTCUSD", "crypto",
    "C:GBPUSD", "fx",
    "AAL", "stock",
    "I:NDX", "index",
    "O:SPY251219C00650000", "option",
  )
  expect_identical(
    purrr::map_chr(ticker_markets$ticker, ~ ticker_type(.x)),
    ticker_markets$expected
  )
  expect_error(
    ticker_type("invalid:format"),
    'ticker = "invalid:format" is not a valid ticker.'
  )
})

test_that("Grouped daily throws error when invalid market is supplied.", {
  expect_error(
    grouped_daily(date = "2023-01-09", market = "invalid"),
    'market = "invalid" is invalid.',
    fixed = TRUE
  )
})
