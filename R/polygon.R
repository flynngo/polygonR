#' Request data from polygon aggregates api
#'
#' Requests data using the polygon.io Aggregates API.
#'
#' @param ticker Specify a case-sensitive ticker symbol. For example, AAPL represents Apple Inc.
#' @param from The start of the aggregate time window. Either a date with the format YYYY-MM-DD or a millisecond timestamp.
#' @param to The end of the aggregate time window. Either a date with the format YYYY-MM-DD or a millisecond timestamp.
#' @param timespan The size of the time window.
#' @param multiplier The size of the timespan multiplier.
#' @param api_key polygon.io API key
#' @param adjusted Whether or not the results are adjusted for splits.
#' @param limit Limits the number of base aggregates queried to create the aggregate results. Max 50000.
#' @param sort Sort the results by timestamp. "asc" will return results in ascending order (oldest at the top), "desc" will return results in descending order (newest at the top).

#'
#' @references https://polygon.io/docs/stocks/get_v2_aggs_ticker__stocksticker__range__multiplier___timespan___from___to
#' @export
#'
request_agg <- function(ticker, from, to, timespan = "day",  multiplier = 1, api_key = get_api_key(), adjusted = TRUE, limit = 50000, sort = "asc") {
  # Build API request
  params <- list(
    adjusted = adjusted,
    sort = sort,
    limit = limit
  )
  req <- glue::glue("https://api.polygon.io/v2/aggs/ticker/{ticker}/range/{multiplier}/{timespan}/{from}/{to}") %>%
    httr2::request() |>
    httr2::req_url_query(!!!params) |>
    httr2::req_headers(Authorization = glue::glue("Bearer {api_key}"))
  return(req)
  # Execute request
  resp <- req %>%
    httr2::req_perform()  %>%
    httr2::resp_body_json()

  # Format data
  process_agg(resp)

  # TODO: if limit < number of base aggregates needed for query then resp$next_url will be non-null and contain the next query (so I should check for this and if it's nonnull call a new query and bind the returned objects together (and go until it's not null (maybe prompt user first?))). httr2 has features to help with multiple requests.

}


#' Convert polygon.io aggregates query from json to tidy format
#'
#' Convert the json obtained from an aggregates query of polygon.io into tabular data.
#'
#' @param json response object from [`httr2::resp_body_json`]
#'
#' @return tibble containing information in `json`.
process_agg <- function(json) {
  # Extract attributes
  tibble::tibble(
    ticker = json[["ticker"]],
    adjusted = json[["adjusted"]],
    results = tidy_results(json[["results"]]),
  ) %>%
    tidyr::unnest(cols = c(.data$results))

}

#' Format results attribute as tidy data
#'
#' Convert the results attribute into tabular data.
#'
#' @param results results attribute from query (see [`process_agg()`]).
#'
#' @return results attribute as a tibble
tidy_results <- function(results) {
  results %>%
    dplyr::bind_rows() %>%
    dplyr::rename(
      close = "c",
      high = "h",
      low = "l",
      open = "o",
      time = "t",
      trade_volume = "v",
      volume_weighted = "vw"
      ) %>%
    # Convert time from unix time in milliseconds
    dplyr::mutate(
      time = lubridate::as_datetime(.data$time/1000)
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

    # TODO: Check this with fresh environment
    warning("No API key found, please supply with `api_key` argument or with POLYGON_KEY env var")
    set_api_key()
    message("Restart R")
  }
}

set_api_key <- function(key = NULL) {
  if (is.null(key)) {
    key <- askpass::askpass("Please enter your API key")
  }
  Sys.setenv("POLYGON_KEY" = key)
}

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

# TODO: need to update testing_key with my encrypted key
testing_key <- function() {
  httr2::secret_decrypt("6TUF1FcAwwK2jRsSgr0Sd3ZBOCxysCcQj4evRxWBC-WuzP3EpUzpu8bx-vQ81uz4", "POLYGONTEST_KEY")
}
