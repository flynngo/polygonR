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

test_that("Aggregates queries are successful", {
  expect_no_error(
    aapl <- aggregates(
      ticker = "AAPL", multiplier = 1, timespan = "day", from = "2024-01-09",
      to = "2024-02-09", limit = 120
    )
  )
  expect_identical(
    colnames(aapl),
    c("ticker", "volume", "volume_weighted", "open", "close", "high", "low", "time", "transactions")
  )
  expect_no_error(
    amzn <- aggregates(
      ticker = "AMZN", multiplier = 1, timespan = "day", from = "2024-02-08",
      to = "2024-02-14", limit = 3
    )
  )
  expect_identical(nrow(amzn), 5L)
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

test_that("Previous close queries are successful", {
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
