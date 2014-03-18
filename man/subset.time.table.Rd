\name{subset.time.table}
\alias{subset.time.table}
\title{Subset a time table}
\usage{
\method{subset}{time.table}(tt, expr = NULL, vars = NULL, index = NULL,
  times = NULL, preserve.frequency = NULL)
}
\arguments{
  \item{tt}{time.table to subset}

  \item{expr}{Expression evaluated within tt to subset the
  rows of tt}

  \item{vars}{Non-index/time columns to subset}

  \item{index}{data.table/data.frame containing indices or
  indices and times to subset}

  \item{times}{times within each index to subset (to not
  specify if index already contains times to extract!)}

  \item{preserve.frequency}{whether to preserve (or
  reguess) frequency information after subsetting}
}
\description{
Subset a time table rows and columns by expression or
indices, preserving time.table structure.
}
\details{
Note that for now \code{vars} *must* be a collection of
strings.

One can specify more than one of \code{expr}, \code{vars},
and \code{index}/\code{time}, in which case the time.table
is first subset by index/time, then by expr and finally by
vars, meaning index/expr can rely on variables removed
after the subset.  Note that this can result in unexpected
behaviour if expr evaluates to a vector of row numbers.

Furthermore both \code{vars} and \code{expr} can mention
extra columns in index/time (merged in the intermediate
data.table in the \code{index} step).

If a column name exists in both \code{tt} and
\code{index}/\code{time} then the name will referr to the
corresponding column of \code{tt} *if* the column is to be
kept (i.e. it is in \code{vars}), else it will refer to the
merged value from \code{index}/\code{time}.
}

