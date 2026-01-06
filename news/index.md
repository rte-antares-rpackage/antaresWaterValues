# Changelog

## antaresWaterValues 2.0.0

### New features

- New function to compute Bellman values sequentially based on reward
  functions from plaia implementation
- New function getOptimalTrend
- New function get_initial_level_year_per_year with scenario builder
  data

### Improvements

- Simplify arguments of Bellman functions
- More robust changeHydroManagement

## antaresWaterValues 1.2.3

### Bugfixes

- Creation of districts for API study (temporary)

## antaresWaterValues 1.2.2

### New features

- Build on release github action

## antaresWaterValues 1.2.1

### New features

- New test on lower bound

## antaresWaterValues 1.2.0

### New features

- Method to compute Bellman values for multiple storages based on one
  simulation

### Improvements

- Better handling of errors in runWaterValuesSimulation
- Better README

### Bugfixes

- Errors in shiny_calculate

## antaresWaterValues 1.1.0

### New features

- Mode expansion compatible with AntaREST
- Possible to launch a subset of simulations with parameter
  launch_simulations

### Improvements

- Update of documentation

### Remove features

- Only one method to compute Bellman values in Grid_Matrix
- Remove reward computation in Grid_Matrix

### Bugfixes

- Update of binding constraints with old versions of Antares
- Error on non integer pumping power

## antaresWaterValues 1.0.0

### Major changes

- Compatibilty with AntaREST
- Simplification and factorization of runWaterValuesSimulation
- Removing arguments relative to district and fictive areas
- Better deal with errors in runWaterValuesSimulation with backup data

## antaresWaterValues 0.5.0

### New features

- Initialization of website
- New github actions

### Improvements

- Unify functions parameters
- Cleaner functions documentation

## antaresWaterValues 0.4.2

### New features

- Use of cvar in Bellman values computation

### Improvements

- Simplify the scnearization of controls

## antaresWaterValues 0.4.0

### Major changes

- Controls evaluated by Antares can differ by scenarios

## antaresWaterValues 0.3.3

### Minor changes

- Adding the possibility to force final level of stock at the end of the
  year thanks to penalties

## antaresWaterValues 0.3.2

### Minor changes

- Adding new data saved at the end of the simulation in order to
  simplify Shiny interface
- Separate Shiny code in modules

## antaresWaterValues v0.3.1

### Minor changes

- Choosing a study without Windows shell folder widget by using
  `shiny_water_values` function’s `opts` parameter
- Simplifying ‘reset’ and ‘restore’ functions

## antaresWaterValues v0.3.0 \[august 2023\]

### Major changes

- New method to calculate rewards based on marginal prices
- Deleting inaccessible states and adding penalties on rule curves
- Better and faster implementation of dynamic programming scheme

### Minor changes

- Simplification of package and Shiny interface
- Non constant generating and pumping powers taken into account

## antaresWaterValues v0.1.0 \[april 2023\]

- Initial version of the package
