\name{subset.time.table.parts}
\alias{subset.time.table.parts}
\title{Extract specific columns from a time.table}
\usage{
\method{subset}{time.table.parts}(tt, with.index = FALSE, with.time = FALSE,
  with.measurement = FALSE, with.auxiliary = FALSE, rekey = (with.index |
  with.time), manual = c())
}
\arguments{
  \item{tt}{time table to export from}

  \item{with.index}{extract index columns}

  \item{with.time}{extract time column}

  \item{time.measurement}{extract measurement columns}

  \item{with.auxiliary}{extract auxiliary columns}

  \item{rekey}{whether to keep the index/time key(s) in the
  resulting data.table}

  \item{manual}{add additional columns to include (by name)

  You probably want one of \code{\link{index}},
  \code{\link{time}}, \code{\link{measurement}}, or
  \code{\link{auxiliary}}.}
}
\description{
Extract columns from a time.table (producing a data.table)
by their function (as index, time, measurement, or
auxiliary).
}

