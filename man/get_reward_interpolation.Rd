% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Bellman_subfunctions.R
\name{get_reward_interpolation}
\alias{get_reward_interpolation}
\title{Create approximation of reward function for each scenario, used in \code{Bellman}}
\usage{
get_reward_interpolation(Data_week)
}
\arguments{
\item{Data_week}{Data frame generated in \code{Grid_Matrix} code containing
reward database for each scenario (we suppose there is only one week at a time)}
}
\value{
List of \code{stats::approxfun} for each scenario
}
\description{
Create approximation of reward function for each scenario, used in \code{Bellman}
}
