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
  specify if index already contains times to extract!)

  Note that for now \code{vars} *must* be a collection of
  strings.}
}
\description{
Subset a time table rows and columns by expression or
indices, preserving time.table structure.
}

