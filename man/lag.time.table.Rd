\name{lag.time.table}
\alias{lag.time.table}
\title{Lag time.table}
\usage{
\method{lag}{time.table}(tt, forward = TRUE, steps = 1L,
  preserve.frequency = TRUE)
}
\arguments{
  \item{tt}{time.table to lag}

  \item{forward}{Shift forward (default)}

  \item{steps}{Number of steps to shift (i.e. number of
  delta)}

  \item{preserve.frequency}{Preserve range (indeed, the
  exact values) of index/time values present in \code{tt}

  Note that for now preserve.freuqency==FALSE is not
  implemented.}
}
\description{
Lags each time series by a number of time steps, optionally
preserving the index/time values present in the data.table
(in which case some information is lost).
}

