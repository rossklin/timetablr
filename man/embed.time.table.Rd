\name{embed.time.table}
\alias{embed.time.table}
\title{Delay embedding of time.table}
\usage{
\method{embed}{time.table}(x, dimension = 1, prefixes = NULL,
  suffixes = NULL, lag = TRUE, preserve.frequency = TRUE)
}
\arguments{
  \item{x}{time.table to embedd}

  \item{dimension}{number of lagged values to add columns
  for}

  \item{prefixes}{prefixes to add to lagged column names,
  either vector of length \code{column} or a single string,
  defaults to "lag."}

  \item{suffixes}{suffixes to add to lagged column names,
  either vector of length \code{column} or a single string,
  defaults to \code{c(".1", ".2", ..., ".dimension")}.}

  \item{forward}{whether to lag values (the alternative
  being to lead them), defaults to true}

  \item{preserve.frequency}{whether the result should
  include only those index/time combinations present in
  \code{x}}
}
\description{
Creates a time delay embedding (a la Takens' theorem) for
the measuement values of a time.table, that is to say it
adds columns for a number of lagged values for each
measuement.
}

