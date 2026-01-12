# Remove candidate cluster from the study before running investment process

Remove candidate cluster from the study before running investment
process

## Usage

``` r
remove_candidate_cluster(opts, candidates_types_gen)
```

## Arguments

- opts:

  List of study parameters returned by the function
  `antaresRead::setSimulationPath(simulation="input")` in input mode.

- candidates_types_gen:

  Data_frame with column names :
  `c(index, name, type, TOTEX, Marg_price, Part_fixe, Prix_fixe, Borne_min, Borne_max, Points_nb, Zone)`.
  Each row describes a cluster candidate. The index should correspond to
  the index of the candidate in `candidates_data`. The name is a
  character, the type is either `"cluster_flexible"` or
  `"cluster_bande"`, TOTEX is in eur/MW/year, Marg_price is in eur/MWh.
  Part_fixe is the fixed part for a variable cluster between 0 and 1,
  Prix_fixe in eur/MWh is its price. Borne_min and Borne_max are in MW,
  Points_nb is an integer (number of candidates tested at each
  iteration, it should be at least 4). Zone is a character containing
  the name of the area where to propose the candidate.
