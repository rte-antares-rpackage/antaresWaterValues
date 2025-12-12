# Compute the list of cluster candidates to study following their bounds

Compute the list of cluster candidates to study following their bounds

## Usage

``` r
grid_other_candidates(candidates_data)
```

## Arguments

- candidates_data:

  List of vector of doubles of length 3. One vector of double for each
  candidate. The vectors of doubles have the form (bound min, bound max,
  number of points).

## Value

a `list` of the capacity of each candidate cluster
