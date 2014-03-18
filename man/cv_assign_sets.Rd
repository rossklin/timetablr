\name{cv_assign_sets}
\alias{cv_assign_sets}
\title{Genrate cross validation sets}
\usage{
cv_assign_sets(tt, counts = NULL, props = NULL, sample.points = FALSE,
  cv.set.name = "cv.set", keep.all = FALSE)
}
\arguments{
  \item{tt}{time.table to split}

  \item{counts}{vector giving number of observations in
  each set}

  \item{props}{vector giving proportion of observations in
  each set}

  \item{sample.points}{whether to sample individual
  observations rather than whole time series (defaults to
  sampling time series)}

  \item{cv.set.name}{name of column containing set labels}

  \item{keep.all}{whether to keep datapoints not assigned
  to a subset}
}
\description{
Assign random indices to observations, useful for
generating train/test/validation sets.
}

