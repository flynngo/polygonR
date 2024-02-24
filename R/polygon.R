#' Query polygon.io REST API
#'
#' Query the polygon.io REST API. Meant for internal use only.
#'
#' @param url URL to query.
#' @param params Named list of additional parameters to send with query.
#' @param api_key String containing API key for polygon.io.
#' @param rate_limit Number of API requests allowed per minute. Basic plan users
#'   are allowed 5 requests per minute, while paid plan users are allowed `Inf`
#'   requests per minute.
#' @param max_reqs The number of requests allowed for a single query. If a query
#'   exceeds the amount of data that is allowed for a single request then
#'   `query` will iteratively send requests until either the query is completed
#'   or `max_reqs` requests are sent.
#' @seealso [`httr2::req_perform_iterative()`], which handles the underlying
#'   implementation.
#' @returns A list, at most length `max_reqs`, containing responses and possibly
#'   one error object, if one of the requests errors. If present, the error
#'   object will always be the last element in the list.
#' @noRd
query <- function(url, params, api_key, rate_limit, max_reqs = 1) {
  req <- httr2::request(url) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_user_agent("polygonR (https://github.com/flynngo/polygonR)") |>
    httr2::req_headers(Authorization = glue::glue("Bearer {api_key}")) |>
    httr2::req_throttle(rate_limit / 60)

  resps <- httr2::req_perform_iterative(
    req,
    next_req = next_req,
    max_reqs = max_reqs
  )

  if (length(resps) == max_reqs &&
        !is.null(httr2::resp_body_json(resps[[max_reqs]])$next_url)) {
    cli::cli_warn(
      c(
        "!" = "Incomplete results were returned for query.",
        "x" = "{.arg max_reqs} = {max_reqs} was reached before query finished.",
        "i" = "For complete results increase {.arg max_reqs} and re-run."
      )
    )
  }
  resps
}

#' Iteration Helper Function for [`query`]
#'
#' For internal use by [`query()`]. Function returns the next request in the
#' iteration when calling [`query`].
#'
#' @param resp [`httr2::response()`] for previous iteration.
#' @param req [`httr2::request()`] for previous iteration.
#' @return Either [`httr2::request()`] for the next iteration or `NULL` if the
#'   query is complete.
#' @seealso [`httr2::req_perform_iterative()`] for general information about
#'   `next_req`.
#' @noRd
next_req <- function(resp, req) {
  next_url <- httr2::resp_body_json(resp)$next_url
  if (is.null(next_url)) {
    return(NULL)
  }
  req |>
    httr2::req_url(url = next_url)
}

#' Get aggregate bars for an asset over a time period
#'
#' Use the aggregates API to get aggregate bars for a particular asset over a
#' given date range in custom time window sizes.
#'
#' @param ticker A string containing the ticker code for an asset (stock,
#'   option, index, etc.). See [`ticker_type`] for more details on formatting
#'   ticker codes.
#' @param from The start of the aggregate time window. Either a date with the
#'   format "YYYY-MM-DD" or a millisecond timestamp.
#' @param to The end of the aggregate time window. Either a date with the format
#'   "YYYY-MM-DD" or a millisecond timestamp.
#' @param timespan The size of the time window.
#' @param multiplier The size of the timespan multiplier.
#' @param api_key String containing API key for polygon.io.
#' @param adjusted Whether or not the results are adjusted for stock splits.
#' @param limit Limits the number of base aggregates that are used to create an
#'   aggregates request. Max 50000. See `max_reqs` for further information about
#'   queries that exceed `limit`.
#' @param sort Sort the results by timestamp. "asc" will return results in
#'   ascending order (oldest at the top), "desc" will return results in
#'   descending order (newest at the top).
#' @param rate_limit Number of API requests allowed per minute. Default = 5
#'   corresponds to the 5 requests allowed per minute for Basic plan users. All
#'   paid plan users are allowed unlimited requests per minute, equivalent to
#'   rate_limit = `Inf`.
#' @param max_reqs The number of requests allowed for a single query. If a query
#'   exceeds the maximum size for a single request then `query` will iteratively
#'   send requests until either the query is completed or `max_reqs` requests
#'   are sent. See `limit` to increase the maximum request size.
#'
#' @returns A tibble containing price information of `ticker` for each of the
#'   requested periods.
#' @references \insertRef{stocksDocumentation}{polygonR}
#' @export
aggregates <- function(ticker,
                       from,
                       to,
                       timespan = "day",
                       multiplier = 1,
                       api_key = get_api_key(),
                       adjusted = TRUE,
                       limit = 50000,
                       sort = "asc",
                       rate_limit = 5,
                       max_reqs = 5) {
  params <- list(
    adjusted = adjusted,
    sort = sort,
    limit = limit
  )
  query(
    glue::glue("{base_url()}/v2/aggs/ticker/{ticker}/range/{multiplier}/{timespan}/{from}/{to}"), # nolint
    params = params,
    api_key = api_key,
    rate_limit = rate_limit,
    max_reqs = max_reqs
  ) |>
    httr2::resps_data(\(resp) tidy_aggregates(resp))
}

market_values <- c("stocks", "fx", "forex", "crypto")
#' Get the daily open, high, low, and close for a market
#'
#' Get the daily open, high, low, and close (OHLC) for an entire market on a
#' particular date.
#'
#' @param date Either a date with the format "YYYY-MM-DD" or a millisecond
#'   timestamp.
# nolint start
#' @param market The market to show, possible values are `r dQuote(market_values)`.
# nolint end
#' @param include_otc Include OTC securities (default = `FALSE`).
#' @inheritParams aggregates
#'
#' @returns A tibble containing the requested data.
#' @references \insertRef{stocksDocumentation}{polygonR}
#' @export
grouped_daily <- function(date,
                          market = "stocks",
                          include_otc = FALSE,
                          api_key = get_api_key(),
                          adjusted = TRUE,
                          rate_limit = 5) {
  if (!any(market %in% market_values)) {
    cli::cli_abort(c(
      "x" = "market = {.str {market}} is invalid.",
      "i" = "{.arg market} must be one of {.str  {market_values}}"
    ))
  }
  args <- list(date = date, api_key = api_key, adjusted = adjusted, rate_limit = rate_limit) # nolint
  if (market == "forex") {
    market <- "fx"
  } else if (market == "stocks") {
    args$include_otc <- include_otc
  }

  do.call(
    glue::glue("{market}_daily"),
    args
  )
}

#' @rdname grouped_daily
#' @export
stocks_daily <- function(date,
                         include_otc = FALSE,
                         api_key = get_api_key(),
                         adjusted = TRUE,
                         rate_limit = 5) {
  params <- list(
    adjusted = adjusted,
    include_otc = include_otc
  )
  query(
    glue::glue("{base_url()}/v2/aggs/grouped/locale/us/market/stocks/{date}"),
    params = params,
    api_key = api_key,
    rate_limit = rate_limit
  ) |>
    httr2::resps_data(\(resp) tidy_grouped_daily(resp))
}

#' @rdname grouped_daily
#' @export
crypto_daily <- function(date,
                         api_key = get_api_key(),
                         adjusted = TRUE,
                         rate_limit = 5) {
  params <- list(
    adjusted = adjusted
  )
  query(
    glue::glue("{base_url()}/v2/aggs/grouped/locale/global/market/crypto/{date}"), # nolint
    params = params,
    api_key = api_key,
    rate_limit = rate_limit
  ) |>
    httr2::resps_data(\(resp) tidy_grouped_daily(resp))
}

#' @rdname grouped_daily
#' @export
fx_daily <- function(date,
                     api_key = get_api_key(),
                     adjusted = TRUE,
                     rate_limit = 5) {
  params <- list(
    adjusted = adjusted
  )
  query(
    glue::glue("{base_url()}/v2/aggs/grouped/locale/global/market/fx/{date}"),
    params = params,
    api_key = api_key,
    rate_limit = rate_limit
  ) |>
    httr2::resps_data(\(resp) tidy_grouped_daily(resp))
}

#' Get the open and closing prices of an asset
#'
#' Get the open, close and after-hours prices of an asset on a particular date.
#'
#' @inheritParams aggregates
#' @inheritParams grouped_daily
#'
#' @returns A tibble containing the requested data.
#' @references \insertRef{stocksDocumentation}{polygonR}
#' @export
open_close <- function(ticker,
                       date,
                       adjusted = TRUE,
                       api_key = get_api_key(),
                       rate_limit = 5) {
  params <- list(
    adjusted = adjusted
  )
  query(
    glue::glue("{base_url()}/v1/open-close/{ticker}/{date}"),
    params = params,
    api_key = api_key,
    rate_limit = rate_limit
  ) |>
    httr2::resps_data(\(resp) tidy_open_close(resp))
}

# TODO: Need to check what the API call returns on a Sunday. Is it the Friday
# close value, or something else?

#' Get the open and closing prices from the previous day
#'
#' Get the open, high, low, and close (OHLC) prices of an asset from the
#' previous day.
#'
#' @inheritParams aggregates
#'
#' @returns A tibble containing the requested data.
#' @export
#' @references \insertRef{stocksDocumentation}{polygonR}
prev_close <- function(ticker,
                       adjusted = TRUE,
                       api_key = get_api_key(),
                       rate_limit = 5) {
  params <- list(
    adjusted = adjusted
  )
  query(
    glue::glue("{base_url()}/v2/aggs/ticker/{ticker}/prev"),
    params = params,
    api_key = api_key,
    rate_limit = rate_limit
  ) |>
    httr2::resps_data(\(resp) tidy_prev_close(resp))
}

#' Query all ticker symbols which are supported by Polygon.io
#'
#' Query all ticker symbols which are supported by Polygon.io. This API
#' currently includes Stocks/Equities, Indices, Forex, and Crypto.
#'
#' @note Warning: Default arguments will return all ticker symbols, which will
#'   require a large number of queries, run slowly due to the `rate_limit`, and
#'   hit the default `max_reqs` value. Basic plan users should specify some
#'   search parameters to drastically reduce the number of queries and hence the
#'   time to complete the request.
#'
#' @param type Specify the type of the tickers. Defaults to querying all types.
#' @param market Filter by market type, possible values are
#' `r dQuote(c(market_values, "otc"))`.
#' By default all markets are included.
#' @param exchange Specify the primary exchange of the asset in the ISO code
#'   format. Find more information about the ISO codes [at the ISO org
#'   website](https://www.cusip.com/identifiers.html#/CUSIP). Defaults to empty
#'   string which queries all exchanges.
#' @param cusip Specify the CUSIP code of the asset you want to search for. Find
#'   more information about CUSIP codes [at their
#'   website](https://www.cusip.com/identifiers.html#/CUSIP). Defaults to empty
#'   string which queries all CUSIPs.
#' @param cik Specify the CIK of the asset you want to search for. Find more
#'   information about CIK codes [at their
#'   website](https://www.sec.gov/edgar/searchedgar/cik). Defaults to empty
#'   string which queries all CIKs.
#' @param date Retrieve tickers available on the date "YYYY-MM-DD".
#' @param search Search for terms within the ticker and/or company name.
#' @param active Specify if the tickers returned should be actively traded on
#'   the queried date. Default is `TRUE`.
#' @inheritParams aggregates
#'
#' @return A tibble that contains all matching tickers.
#' @export
tickers <- function(ticker = NULL, type = NULL, market = NULL, exchange = NULL,
                    cusip = NULL, cik = NULL, date = NULL, search = NULL,
                    active = NULL, limit = 1000, api_key = get_api_key(),
                    rate_limit = 5, max_reqs = 5) {
  params <- as.list(rlang::call_match(defaults = TRUE))[-1]
  params <- params[!(names(params) %in% c("api_key", "rate_limit", "max_reqs"))]
  query(
    glue::glue("{base_url()}/v3/reference/tickers"),
    params = params,
    api_key = api_key,
    rate_limit = rate_limit,
    max_reqs = max_reqs
  ) |>
    httr2::resps_data(\(resp) tidy_tickers(resp))
}

#' Convert query results to tidy format
#'
#' Convert the response from query of polygon.io into tabular
#' data.
#'
#' @param resp query response (see [`httr2::response`])
#' @return A tibble containing the data in `resp`.
#'
#' @name tidy_resp
#' @noRd
NULL

#' @rdname tidy_resp
tidy_aggregates <- function(resp) {
  json <- httr2::resp_body_json(resp)
  if (is.null(json[["results"]])) {
    return(NULL)
  }
  cols_lookup <- c(
    close = "c",
    high = "h",
    low = "l",
    open = "o",
    time = "t",
    volume = "v",
    volume_weighted = "vw",
    transactions = "n"
  )
  dplyr::bind_cols(
    ticker = json[["ticker"]],
    results = dplyr::bind_rows(json[["results"]]),
  ) |>
    dplyr::rename(dplyr::any_of(cols_lookup)) |>
    dplyr::mutate(
      time = lubridate::as_datetime(.data$time / 1000)
    )
}

#' @rdname tidy_resp
tidy_grouped_daily <- function(resp) {
  json <- httr2::resp_body_json(resp)
  if (is.null(json[["results"]])) {
    return(NULL)
  }
  dplyr::bind_rows(json[["results"]]) |>
    dplyr::rename(
      ticker = "T",
      close = "c",
      high = "h",
      low = "l",
      open = "o",
      time = "t",
      volume = "v",
      volume_weighted = "vw",
      transactions = "n"
    ) |>
    dplyr::mutate(
      time = lubridate::as_datetime(.data$time / 1000)
    )
}

#' @rdname tidy_resp
tidy_open_close <- function(resp) {
  resp |>
    httr2::resp_body_json() |>
    dplyr::bind_rows() |>
    dplyr::rename(
      after_hours = "afterHours",
      date = "from",
      pre_market = "preMarket",
      ticker = "symbol"
    ) |>
    dplyr::mutate(
      date = lubridate::as_date(.data$date)
    ) |>
    dplyr::select(-c("status"))
}

#' @rdname tidy_resp
tidy_prev_close <- function(resp) {
  json <- httr2::resp_body_json(resp)
  if (is.null(json[["results"]])) {
    return(NULL)
  }
  cols_lookup <- c(
    ticker = "T",
    close = "c",
    high = "h",
    low = "l",
    open = "o",
    time = "t",
    volume = "v",
    volume_weighted = "vw",
    transactions = "n"
  )
  dplyr::bind_rows(json[["results"]]) |>
    dplyr::rename(dplyr::any_of(cols_lookup)) |>
    dplyr::mutate(
      time = lubridate::as_datetime(.data$time / 1000)
    )
}

#' @rdname tidy_resp
tidy_tickers <- function(resp) {
  json <- httr2::resp_body_json(resp)
  if (is.null(json[["results"]])) {
    return(NULL)
  }
  dplyr::bind_rows(json[["results"]])
}

#' Detect market type of a ticker
#'
#' Classify a `ticker` as either stock, option, forex, index or crypto based on
#' it's format.
#'
#' Only the ticker prefix is considered when detecting the market type. This
#' function does not check that `ticker` corresponds to a valid asset, see
#' Section: *Ticker formats* for more details on correctly formatting ticker
#' codes.
#'
#' # Ticker formats
#'
#' Tickers of each type are formatted as follows.
#'
#' * **Stock** tickers are simply formatted as standard ticker codes.
#' E.g.`"AAPL"` for Apple.
#' * **Option** tickers follow the general format `"O:AAPL211119C00085000"`,
#' where
#'    * O: indicates that this is an option ticker,
#'    * AAPL is the ticker for the underlying stock,
#'    * 211119 is the expiration date in YYMMDD format,
#'    * C or P indicates whether the option is a call or put option, and
#'    * 00085000 is the strike price to three decimal places, in this case $85.
#' * **Forex** price tickers follow the general format `"C:GBPUSD"`, where
#'    * C: indicates that this is a forex ticker,
#'    * GBP is the currency we are exchanging from, in this case GB
#' pounds, and
#'    * USD is the currency we are exchanging to, in this case US dollars.
#' * **Index** tickers follow the general format `"I:NDX"`, where:
#'    * I: indicates that this is an index ticker,
#'    * NDX is the the index ticker code, in this case the NASDAQ 100.
#' * **Crypto** price tickers follow the general format `"X:BTCUSD"`, where
#'    * X: indicates that this is crypto ticker,
#'    * BTC is the currency we are exchanging from, in this case bitcoin, and
#'    * USD is the currency we are exchanging to.
#'
#' @param ticker A string containing a ticker code.
#'
#' @return One of `"stock"`, `"option"`, `"fx"`, `"index"` and `"crypto"`.
#'
#' @seealso [*polygon.io*: How to Read A Stock Options
#'   Ticker](https://polygon.io/blog/how-to-read-a-stock-options-ticker)
#' @examples ticker_type("AAL")
#' @export
ticker_type <- function(ticker) {
  tt <- dplyr::case_when(
    grepl("^C:", ticker) ~ "fx",
    grepl("^I:", ticker) ~ "index",
    grepl("^X:", ticker) ~ "crypto",
    grepl("^O:", ticker) ~ "option",
    grepl("^[a-zA-Z]+$", ticker) ~ "stock"
  )
  if (is.na(tt)) {
    cli::cli_abort(c(
      "x" = "ticker = {.str {ticker}} is not a valid ticker.",
      "i" = "See {.run [ticker_type](?polygonR::ticker_type)} to learn more."
    ))
  }
  tt
}

base_url <- function() {
  "https://api.polygon.io"
}

# Helper functions for API key
get_api_key <- function() {
  key <- Sys.getenv("POLYGON_KEY")
  if (!identical(key, "")) {
    return(key)
  }

  if (is_testing()) {
    return(testing_key())
  } else {
    cli::cli_warn(c(
      "No API key found.",
      "i" = "Supply {.arg api_key} arg or set {.envvar POLYGON_KEY} env var.",
      "i" = "Use {.fun set_api_key} to set {.envvar POLYGON_KEY}."
    ))
  }
}

#' Set API key as environment variable
#'
#' @param key Default = `NULL` will prompt the user to enter their API key and
#'   is recommended. Alternatively users can pass their API key as a string,
#'   although this will print the key in the console, which is less secure.
#'
#' @return NULL
#' @export
set_api_key <- function(key = NULL) {
  if (is.null(key)) {
    key <- askpass::askpass("Please enter your API key")
  }
  Sys.setenv("POLYGON_KEY" = key)
  cli::cli_inform(c("v" = "POLYGON_KEY set."))
}

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

testing_key <- function() {
  httr2::secret_decrypt(
    "xbLJDxkC7VZ7-kcL4rrYV-1fQyHW6I-DURQ9a7ePNsAjwYdkRByso2JHlnSskB22",
    "POLYGONTEST_KEY"
  )
}
