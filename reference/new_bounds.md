# Updates the candidates bounds at the end of an iteration.

Updates the candidates bounds at the end of an iteration.

## Usage

``` r
new_bounds(best_candidate, candidates_data, storage_points)
```

## Arguments

- best_candidate:

  \_

- candidates_data:

  List of vector of doubles of length 3. One vector of double for each
  cluster candidate. The vectors of doubles have the form (bound min,
  bound max, number of points). The number of points must be \>3 to
  update bounds at each iterations and approach solution.

- storage_points:

  Vector of integers. Storage volumes candidates at these iteration

## Value

A `list` with : storage_bounds (Vector of integers. Storage volumes
candidates at next iteration.) and candidates_data (List. It is of the
same form as the `candidates_data` argument with new clusters
candidates.)
