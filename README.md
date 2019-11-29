<img src="man/figures/antares_simulator.png" align="right" width=250 />
<br/>

# antaresWaterValues

> Calculate Water Values to use in Antares V7.

[![Travis-CI Build Status](https://travis-ci.org/rte-antares-rpackage/antaresWaterValues.svg?branch=master)](https://travis-ci.org/rte-antares-rpackage/antaresWaterValues)


## Installation

You can install the package from [GitHub](https://github.com/) with:

```r
# install.packages("devtools")
devtools::install_github("rte-antares-rpackage/antaresWaterValues", build_vignettes = TRUE)
```


## Goal

This package aims at generating water values for Antares. It allows to run specific
simulations in Antares from within R, and to use the results of these simulations
to compute Bellman values and water values for each stock level and each week of the
year. The obtained values can be exported to Antares and used in any study (since
Antares v7).


## Usage

Usage of the package is detailed in the vignette: `vignette("antaresWaterValues")`.

