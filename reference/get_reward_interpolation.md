# Create approximation of reward function for each scenario, used in `Bellman`

Create approximation of reward function for each scenario, used in
`Bellman`

## Usage

``` r
get_reward_interpolation(Data_week)
```

## Arguments

- Data_week:

  Data frame generated in `Grid_Matrix` code containing reward database
  for each scenario (we suppose there is only one week at a time)

## Value

List of [`stats::approxfun`](https://rdrr.io/r/stats/approxfun.html) for
each scenario
