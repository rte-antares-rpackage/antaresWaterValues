% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plot_functions.R
\name{plot_reward_mc}
\alias{plot_reward_mc}
\title{Plot the reward and return the results in table}
\usage{
plot_reward_mc(reward_base, week_id, Mc_year)
}
\arguments{
\item{reward_base}{A data.table contains the rewards.
Obtained using the function get_Reward()}

\item{week_id}{Numeric of length 1. number of the week to plot.}

\item{Mc_year}{Numeric of length 1. number of thr MC year to plot}
}
\value{
a \code{ggplot} object
}
\description{
Plot the reward and return the results in table
}
