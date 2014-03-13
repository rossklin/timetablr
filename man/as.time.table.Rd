\name{as.time.table}
\alias{as.time.table}
\title{Create a time.table}
\usage{
as.time.table(df, id.vars, time.var, measurement.vars = NULL,
  aux.vars = NULL, frequency = NULL)
}
\arguments{
  \item{df}{data.frame or data.table containing data, one
  row for each observation.}

  \item{id.vars}{Name(s) of columns identifiying the entity
  associated to an observation. Each unique combination of
  values corresponding to one entity.}

  \item{time.var}{*Single* name identifying the column
  containing time information for each observation.}

  \item{measurement.vars}{Name(s) of columns containing
  measurement values. These are the columns most functions
  will operate on by default. Will default to all column
  names not used for indexing/time/auxiliary.}

  \item{aux.vars}{Name(s) of columns containing auxiliary
  measurements. These will be carried along by most
  functions but will not be mutated. Will defatult to all
  columns not used for anything else (with measurement.vars
  having precedence, i.e. if neither
  \code{measurement.vars} or \code{aux.vars} is specified
  the non-index/time columns will be interpreted as
  measurements).}

  \item{frequency}{list containing values \code{from},
  \code{to}, and \code{delta} specifying a (shared) range
  of the time series and the size of their time steps.
  Technically optional though some functions require it (in
  the future there will probably be a more clear separation
  between regular and irregual time series).

  time.table is a subclass of data.table (and therefore
  also of data.frame) and can thus be used like one. Be
  careful though, as most operations *will lose* any
  time.table specific attributes.}
}
\description{
Create a time.table given a data.frame containing its data
and information about how to interpret its columns.
}

