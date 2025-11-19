# Create approximation of Bellman function for next week for each scenario, used in `Bellman` and in `getOptimalTrend`

Create approximation of Bellman function for next week for each
scenario, used in `Bellman` and in `getOptimalTrend`

## Usage

``` r
get_bellman_values_interpolation(Data_week, next_week_values, mcyears)
```

## Arguments

- Data_week:

  Data frame generated in `Grid_Matrix` code containing list of states
  and years evaluated (we suppose there is only one week at a time)

- next_week_values:

  Vector of Bellman values to interpolate

- mcyears:

  Vector of monte carlo years to take into account

## Value

List of [`stats::approxfun`](https://rdrr.io/r/stats/approxfun.html) for
each scenario
