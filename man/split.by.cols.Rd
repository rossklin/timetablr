\name{split.by.cols}
\alias{split.by.cols}
\title{Split a data.table}
\usage{
\method{split}{by.cols}(dt, cols)
}
\arguments{
  \item{dt}{data.table to split}

  \item{cols}{column names to split by

  Returns a list with attribute "values" containing the
  unique combination of \code{cols} values associated to
  each element of the list.}
}
\description{
Splits a data.table according to a number of its columns.
}

