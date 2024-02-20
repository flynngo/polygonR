test_that("Error handling", {
  params <-  list(
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
    new = c("POLYGON_KEY" = "NULL"), {
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
    aapl <- aggregates(
      ticker = "O:AAPL251219C00650000", multiplier = 1, timespan = "day",
      from = "2024-01-09", to = "2024-02-09", limit = 120
    )
  )
  expect_identical(
    colnames(aapl),
    c("ticker", "volume", "volume_weighted", "open", "close", "high", "low", "time", "transactions") # nolint
  )
  expect_no_error(
    amzn <- aggregates(
      ticker = "AMZN251219C00650000", multiplier = 1, timespan = "day",
      from = "2024-02-08", to = "2024-02-14", limit = 3
    )
  )
  expect_identical(nrow(amzn), 5L)
})


test_that("Indices aggregates queries are successful", {
  expect_no_error(
    ndx <- aggregates(
      ticker = "I:NDX", multiplier = 1, timespan = "day",
      from = "2024-01-09", to = "2024-02-09", limit = 120
    )
  )
  # Column names are different for indices so this test will need to be update
  # to reflect that.
  expect_identical(
    colnames(ndx),
    c("ticker", "volume", "volume_weighted", "open", "close", "high", "low", "time", "transactions") # nolint
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
  # TODO: add in actual price check tests for gd_stocks1 and gd_stocks2, so I know that
  # they're returning results
  expect_no_error(
    gd_stocks1 <- grouped_daily(date = "2023-01-09", market = "stocks")
  )
  expect_no_error(
    gd_stocks2 <- stocks_daily(date = "2023-01-09")
  )

  # TODO: update below functions to stocks_daily (when the above ones work)
  expect_true(
    any(
      "ALIZF" == grouped_daily(date = "2023-01-09", market = "stocks", include_otc = TRUE)$ticker
    )
  )
  expect_false(
    any(
      "ALIZF" == grouped_daily(date = "2023-01-09", market = "stocks", include_otc = FALSE)$ticker
    )
  )
})

test_that("Forex grouped daily queries are successful", {
  # TODO: add in actual price or column check tests for gd_fx1 and gd_fx2, so I know that
  # they're returning results
  expect_no_error(
    gd_fx1 <- grouped_daily(date = "2023-01-09", market = "fx")
  )
  expect_no_error(
    gd_fx2 <- fx_daily(date = "2023-01-09")
  )
})

test_that("Crypto grouped daily queries are successful", {
  # TODO: add in actual price or column check tests for gd_crypto1 and gd_crypto2, so I
  # know that they're returning results
  expect_no_error(
    gd_crypto1 <- grouped_daily(date = "2023-01-09", market = "crypto")
  )
  expect_no_error(
    gd_crypto2 <- fx_daily(date = "2023-01-09")
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

# TODO: update options contracts to be more sensible.
test_that("Options open/close queries are successful", {
  msft <- open_close(ticker = "MSFT251219C00650000", date = "2024-02-12")
  alizf <- open_close(ticker = "ALIZF251219C00650000", date = "2024-02-01")

  # TODO: update expected prices to be correct
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

test_that("Indices open/close queries are successful", {
  actual <- open_close(ticker = "I:NDX", date = "2024-02-12")
# TODO: update expected prices to be correct
  expected <- tibble::tibble(
    open = 420.56,
    high = 420.74,
    low = 414.75,
    close = 415.26,
    volume = 21202920
  )
  # Different sites list different trade volumes.
  tol <- c(open = .005, high = .005, low = .005, close = .005, volume = 100)

  for (i in names(tol)) {
    expect_lte(abs(actual[, i] - expected[i]), tol[i])
  }
})

test_that("Crypto open/close queries are successful", {
  # TODO: polygon api takes from and to. I can probably convert to a ticker like
  # this for consistency, but I should possibly also have a from and to option.
  actual <- open_close(ticker = "X:BTCUSD", date = "2024-02-12")

  # TODO: update prices to be correct.
  expected <- tibble::tibble(
    open = 420.56,
    high = 420.74,
    low = 414.75,
    close = 415.26,
    volume = 21202920
  )
  # Different sites list different trade volumes.
  tol <- c(open = .005, high = .005, low = .005, close = .005, volume = 100)

  for (i in names(tol)) {
    expect_lte(abs(actual[, i] - expected[i]), tol[i])
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

# TODO: update options contracts to be more sensible.
test_that("Options previous close queries are successful", {
  expect_no_error(
    nflx <- prev_close("NFLX251219C00650000")
  )
  expect_no_error(
    danoy <- prev_close("DANOY251219C00650000")
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

test_that("Indices previous close queries are successful", {
  expect_no_error(
    actual <- prev_close("I:NDX")
  )
  # TODO: indices results column names are different, so this test will require
  # updating
  expect_identical(
    colnames(actual),
    c(
      "ticker", "volume", "volume_weighted", "open", "close", "high", "low",
      "time", "transactions"
    )
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
    ~ticker, ~markets, ~expected,
    "X:BTCUSD", "crypto", "crypto",
    "X:ETHAUD", "crypto", "crypto",
    "C:ZARNOK", "fx", "fx",
    "C:GBPUSD", "fx", "fx",
    "AAL", "stocks", "stocks",
    "ESGV", "stocks", "stocks",
    "HIZOF", "otc", "stocks",
    "DANOY", "otc", "stocks",
    "I:DJITLS", "indices", "indices",
    "I:NQGIHEIEUR", "indices", "indices"
    )
  expect_identical(
    map_chr(ticker_markets$ticker, ~market_type(.x)),
    ticker_markets$expected)
})
