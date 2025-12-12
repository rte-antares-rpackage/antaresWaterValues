# Compute the candidate with maximal reward

Compute the candidate with maximal reward

## Usage

``` r
max_candidate(grid_costs)
```

## Arguments

- grid_costs:

  List. The firsts columns correspond to the candidates storage and
  cluster. The last column contains the total reward under names
  "Total_cost".

## Value

A `list` containing only the row of the best candidate from
`grid_costs`.
