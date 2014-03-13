\name{diff.time.table}
\alias{diff.time.table}
\title{Difference time.table}
\usage{
\method{diff}{time.table}(tt, forward = TRUE, preserve.frequency = TRUE)
}
\arguments{
  \item{tt}{time.table to difference}

  \item{forward}{compute forward differences (default).}

  \item{preserve.frequency}{preserve the range of (indeed,
  exact) values present in \code{tt}

  Note that for now only forward==TRUE and
  preserve.frequency==TRUE are implemented.}
}
\description{
Difference (diff) each time series in a time.table, taking
missing missing values into account (i.e. each difference
it taken at exactly one time step, with a value of NA if
the next observation is not present). Optionally diff
preserves the exact index/time combinations present in the
original time.table (in which case some information is
lost).
}

