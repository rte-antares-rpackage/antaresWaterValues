% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{to_Antares_Format}
\alias{to_Antares_Format}
\title{Convert water values to Antares format}
\usage{
to_Antares_Format(data, constant = T)
}
\arguments{
\item{data}{A data.table generated by \code{Grid_Matrix}}

\item{constant}{Boolean. Generate daily constant values by week. FALSE to do interpolation.}
}
\value{
A 365*101 numeric matrix
}
\description{
This function converts water values generated by \code{Grid_Matrix}
to the format expected by Antares: a 365*101 matrix, where
the rows are the 365 days of the year and the columns are round percentage values
ranging from 0 to 100 assessing the reservoir level.
Since \code{Grid_Matrix} output weekly values for an
arbitrary number of reservoir levels, interpolation is performed on both scales
in order to fit the desired format.
}
