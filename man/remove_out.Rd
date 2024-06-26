% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_process.R
\name{remove_out}
\alias{remove_out}
\title{Remove outliers water Values}
\usage{
remove_out(
  results_dt,
  min = NULL,
  max = NULL,
  max_vu,
  min_vu,
  replace_na_method
)
}
\arguments{
\item{results_dt}{Output from \code{watervalues} or \code{Grid_Matrix}}

\item{min}{minimal accepted water value}

\item{max}{maximal accepted water value}

\item{max_vu}{value to replace values higher than max}

\item{min_vu}{value to replace values lower than min}

\item{replace_na_method}{Method to replace extreme values, either "constant values" to replace
by max_vu and min_vu or "extreme values" to replace by the extreme values of the current week}
}
\value{
a \code{data.table}
}
\description{
Remove outliers water Values
}
