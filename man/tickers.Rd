% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/polygon.R
\name{tickers}
\alias{tickers}
\title{Query all ticker symbols which are supported by Polygon.io}
\usage{
tickers(
  ticker = NULL,
  type = NULL,
  market = NULL,
  exchange = NULL,
  cusip = NULL,
  cik = NULL,
  date = NULL,
  search = NULL,
  active = NULL,
  limit = 1000,
  api_key = get_api_key(),
  rate_limit = 5,
  max_reqs = 5
)
}
\arguments{
\item{ticker}{A string containing the ticker code for an asset (stock,
option, index, etc.). See \code{\link{ticker_type}} for more details on formatting
ticker codes.}

\item{type}{Specify the type of the tickers. Defaults to querying all types.}

\item{market}{Filter by market type, possible values are
"stocks", "fx", "forex", "crypto", "otc".
By default all markets are included.}

\item{exchange}{Specify the primary exchange of the asset in the ISO code
format. Find more information about the ISO codes \href{https://www.cusip.com/identifiers.html#/CUSIP}{at the ISO org website}. Defaults to empty
string which queries all exchanges.}

\item{cusip}{Specify the CUSIP code of the asset you want to search for. Find
more information about CUSIP codes \href{https://www.cusip.com/identifiers.html#/CUSIP}{at their website}. Defaults to empty
string which queries all CUSIPs.}

\item{cik}{Specify the CIK of the asset you want to search for. Find more
information about CIK codes \href{https://www.sec.gov/edgar/searchedgar/cik}{at their website}. Defaults to empty
string which queries all CIKs.}

\item{date}{Retrieve tickers available on the date "YYYY-MM-DD".}

\item{search}{Search for terms within the ticker and/or company name.}

\item{active}{Specify if the tickers returned should be actively traded on
the queried date. Default is \code{TRUE}.}

\item{limit}{Limits the number of base aggregates that are used to create an
aggregates request. Max 50000. See \code{max_reqs} for further information about
queries that exceed \code{limit}.}

\item{api_key}{String containing API key for polygon.io.}

\item{rate_limit}{Number of API requests allowed per minute. Default = 5
corresponds to the 5 requests allowed per minute for Basic plan users. All
paid plan users are allowed unlimited requests per minute, equivalent to
rate_limit = \code{Inf}.}

\item{max_reqs}{The number of requests allowed for a single query. If a query
exceeds the maximum size for a single request then \code{query} will iteratively
send requests until either the query is completed or \code{max_reqs} requests
are sent. See \code{limit} to increase the maximum request size.}
}
\value{
A tibble that contains all matching tickers.
}
\description{
Query all ticker symbols which are supported by Polygon.io. This API
currently includes Stocks/Equities, Indices, Forex, and Crypto.
}
\note{
Warning: Default arguments will return all ticker symbols, which will
require a large number of queries, run slowly due to the \code{rate_limit}, and
hit the default \code{max_reqs} value. Basic plan users should specify some
search parameters to drastically reduce the number of queries and hence the
time to complete the request.
}
