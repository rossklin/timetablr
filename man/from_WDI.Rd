\name{from_WDI}
\alias{from_WDI}
\title{World Bank data}
\usage{
from_WDI(..., auxiliary.indicator = c(), country.name = NULL,
  translation.table = NULL)
}
\arguments{
  \item{...}{Arguments sent sent to \code{WDI} (primarily
  of interest are \code{country}, defaulting to "all",
  \code{start}, defaulting to 1961, \code{end}, defaulting
  to 2013, and \code{indicator}, defaulting to
  "NY.GDP.PCAP.PP.KD" (GDP per capita constant 2005 USD))}

  \item{auxiliary.indicator}{additional indicators to
  include as auxiliary values}

  \item{country.name}{column name to use for country code
  column (used as index), defaults to "iso2c" used by WDI}

  \item{translation.table}{list mapping indicator code
  names to column names to be used in final
  \code{time.table}}
}
\description{
Construct a time.table from World Bank data.
}

