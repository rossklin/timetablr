\name{affix_names}
\alias{affix_names}
\title{Affix measurement/auxiliary names}
\usage{
affix_names(tt, prefix = NULL, suffix = NULL, measurement.prefix = NULL,
  measurement.suffix = NULL, auxiliary.prefix = NULL,
  auxiliary.suffix = NULL)
}
\arguments{
  \item{tt}{time.table to transform names of}

  \item{prefix}{prefix to add to names (defaults to empty
  string)}

  \item{suffix}{suffix to add to names (defaults to empty
  string)}

  \item{measurement.prefix}{prefix to add to measurement
  names (defaults to \code{prefix})}

  \item{measurement.suffix}{suffix to add to measurement
  names (defaults to \code{suffix})}

  \item{auxiliary.prefix}{prefix to add to auxiliary names
  (defaults to \code{prefix})}

  \item{auxiliary.suffix}{suffix to add to auxiliary names
  (defaults to \code{suffix})

  Note, changes tt by reference (i.e. mutates it).}
}
\description{
Adds prefix/suffix to the measurement/auxiliary (column)
names of a time.table. This is useful for when you want to
merge a transformed table back into the original one while
avoiding clashing column names.
}

