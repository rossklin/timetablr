\name{same_str_as}
\alias{same_str_as}
\title{Create time.table from a template}
\usage{
same_str_as(dt, tt, add.index = c(), add.measurement = c(),
  add.auxiliary = c())
}
\arguments{
  \item{dt}{data.table containing data}

  \item{tt}{time.table to use as template}

  \item{add.index}{new index columns present in dt}

  \item{add.measurement}{new measurement columns present in
  dt}

  \item{add.auxiliary}{new auxiliary columns present in dt

  Note that all columns in \code{tt} must be present in
  \code{dt} and that any columns in \code{dt} not in
  \code{tt} are lost.}
}
\description{
Turns a data.table into a time table using attributes from
another time.table.
}

