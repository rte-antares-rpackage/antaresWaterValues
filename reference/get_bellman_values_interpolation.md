# Create approximation of Bellman function for next week for each scenario, used in `Bellman` and in `getOptimalTrend`

Create approximation of Bellman function for next week for each
scenario, used in `Bellman` and in `getOptimalTrend`

## Usage

``` r
get_bellman_values_interpolation(next_state, next_week_values, mcyears)
```

## Arguments

- next_state:

  possible states for next week

- next_week_values:

  Vector of Bellman values to interpolate

- mcyears:

  Vector of monte carlo years to take into account

## Value

List of [`stats::approxfun`](https://rdrr.io/r/stats/approxfun.html) for
each scenario
