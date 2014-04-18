\name{promote}
\alias{promote}
\title{Canges role of column}
\usage{
promote(tt, changes, columns = NULL, destructive = TRUE)
}
\arguments{
  \item{tt}{time.table to swich column roles in}

  \item{changes}{list or character vector of role changes,
  values each being one of "index", "time", "measurement",
  and "auxiliary"}

  \item{columns}{list of columns of same length as
  \code{changes}, defaults to names of elements in
  \code{changes}}

  \item{destructive}{whether to mutate the time.table,
  defaults to True}
}
\description{
Changes the role of columns in a time.table, making sure
relevant internal structure is canged.
}

