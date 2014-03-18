\name{cv_split_time_table}
\alias{cv_split_time_table}
\title{Split a time.table for cross validation}
\usage{
cv_split_time_table(tt, counts = NULL, props = NULL,
  sample.points = FALSE)
}
\arguments{
  \item{tt}{time.table to split}

  \item{counts}{vector containing number of observation in
  each subset}

  \item{props}{vector of proportions of the full dataset
  for each subset}

  \item{sample.points}{whether to sample individual
  observations rather than time.series as a whole (defaults
  to time.series as a whole)

  Names of the resulting list correspond to names used in
  the \code{counts} or \code{props} vector.}
}
\description{
Splits a time.table into a number of subsets useful for
cross validation.
}

