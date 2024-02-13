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
#'
#' @seealso [`httr2::req_perform_iterative()`], which handles the underlying
#'   implementation.
#'
#' @returns A list, at most length `max_reqs`, containing responses and possibly
#'   one error object, if one of the requests errors. If present, the error
#'   object will always be the last element in the list.
#' @noRd
query <- function(url, params, api_key, rate_limit, max_reqs) {
  req <- httr2::request(url) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_user_agent("polygonR (https://github.com/flynngo/polygonR)") |>
    httr2::req_headers(Authorization = glue::glue("Bearer {api_key}")) |>
    httr2::req_throttle(rate_limit / 60)

  resps <- httr2::req_perform_iterative(
    req,
    next_req = next_req,
    max_reqs = max_reqs
    # TODO: add progress = FALSE
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
#'
#' @return Either [`httr2::request()`] for the next iteration or `NULL` if the
#'   query is complete.
#'
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

#' Request data from polygon aggregates api
#'
#' Requests data using the polygon.io aggregates API.
#'
#' @param ticker Specify a case-sensitive ticker symbol. For example, "AAPL"
#'   represents Apple Inc.
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
#' @references \url{https://polygon.io/docs/} for further information about
#'   arguments for polygon.io API requests.
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
    glue::glue("https://api.polygon.io/v2/aggs/ticker/{ticker}/range/{multiplier}/{timespan}/{from}/{to}"), # nolint
    params = params,
    api_key = api_key,
    rate_limit = rate_limit,
    max_reqs = max_reqs
  ) |>
    httr2::resps_data(\(resp) process_agg(httr2::resp_body_json(resp)))
}

grouped_daily <- function(date,
                          include_otc = FALSE,
                          api_key = get_api_key(),
                          adjusted = TRUE,
                          rate_limit = 5,
                          max_reqs = 5) {
  params <- list(
    adjusted = adjusted,
    include_otc = include_otc
  )
  query(
    glue::glue("https://api.polygon.io/v2/aggs/grouped/locale/us/market/stocks/{date}"), # nolint
    params = params,
    api_key = api_key,
    rate_limit = rate_limit,
    max_reqs = max_reqs
  ) |>
    httr2::resps_data(\(resp) tidy_gd(httr2::resp_body_json(resp)))
}

#' Convert polygon.io aggregates query from json to tidy format
#'
#' Convert the json obtained from an aggregates query of polygon.io into tabular
#' data.
#'
#' @param json response object from [`httr2::resp_body_json`]
#'
#' @return tibble containing information in `json`.
process_agg <- function(json) {
  results <- json[["results"]]
  if (is.null(results)) {
    return(NULL)
  }
  tibble::tibble(
    ticker = json[["ticker"]],
    adjusted = json[["adjusted"]], # TODO: remove adjusted (it's an arg in call, don't need to return)
    results = dplyr::bind_rows(results),
  ) |> # instead of bind_rows -> unnest do bind_cols -> bind_rows
    tidyr::unnest(cols = c("results")) |>
    dplyr::rename(
      close = "c",
      high = "h",
      low = "l",
      open = "o",
      time = "t",
      trade_volume = "v",
      volume_weighted = "vw"
    ) |>
    dplyr::mutate(
      time = lubridate::as_datetime(.data$time / 1000)
    )
}

# Tidy grouped_daily function

tidy_gd <- function(json) {
  results <- json[["results"]]
  if (is.null(results)) {
    return(NULL)
  }
  dplyr::bind_rows(results) |>
    dplyr::rename(
      ticker = "T",
      close = "c",
      high = "h",
      low = "l",
      open = "o",
      time = "t",
      trade_volume = "v",
      volume_weighted = "vw"
    ) |>
    dplyr::mutate(
      time = lubridate::as_datetime(.data$time / 1000)
    )
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
#'
#' @return NULL
#' @export
set_api_key <- function() {
  key <- askpass::askpass("Please enter your API key")
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
