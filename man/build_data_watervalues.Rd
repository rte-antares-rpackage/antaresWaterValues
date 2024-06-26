% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/math_functions.R
\name{build_data_watervalues}
\alias{build_data_watervalues}
\title{Calculate and plot watervalues with \code{value_node_gen}. Penalties taken into account.
Used in \code{Grid_Matrix}}
\usage{
build_data_watervalues(
  watervalues,
  statesdt,
  reservoir,
  penalty_level_high,
  penalty_level_low,
  force_final_level = F,
  penalty_final_level_low = 0,
  penalty_final_level_high = 0,
  final_level = NULL
)
}
\arguments{
\item{watervalues}{Data frame generated in \code{Grid_Matrix}}

\item{statesdt}{Data frame of possible states, generated in \code{Grid_Matrix}}

\item{reservoir}{Data frame describing rule curves, generated by \code{readReservoirLevels}}

\item{penalty_level_high}{Penalty for violating the bottom rule curve}

\item{penalty_level_low}{Penalty for violating the top rule curve}

\item{force_final_level}{Binary. Whether final level should be constrained}

\item{penalty_final_level_low}{Penalties for both bottom rule curve to constrain final level}

\item{penalty_final_level_high}{Penalties for top rule curve to constrain final level}

\item{final_level}{Double. Final level to consider}
}
\value{
Data frame with water value (vu) for each week (weeks) and each state (states).
vu_pen corresponds to water value without penalties
}
\description{
Calculate and plot watervalues with \code{value_node_gen}. Penalties taken into account.
Used in \code{Grid_Matrix}
}
