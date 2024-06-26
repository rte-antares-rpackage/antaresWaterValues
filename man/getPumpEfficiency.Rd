% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hydro_storage_functions.R
\name{getPumpEfficiency}
\alias{getPumpEfficiency}
\title{Get the Pumping efficiency ratio for an area reservoir}
\usage{
getPumpEfficiency(area, force = FALSE, opts = antaresRead::simOptions())
}
\arguments{
\item{area}{An 'antares' area.}

\item{force}{If "reservoir management" is disabled, return anyway the reservoir capacity?}

\item{opts}{List of simulation parameters returned by the function
\code{antaresRead::setSimulationPath}}
}
\value{
the reservoir capacity (in MWh), or \code{NULL} if none.
}
\description{
Get the Pumping efficiency ratio for an area reservoir
}
