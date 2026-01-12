# Edit the study with the result from investment

Edit the study with the result from investment

## Usage

``` r
edit_study_with_results(opts, node, output_node, candidates_types)
```

## Arguments

- opts:

  List of study parameters returned by the function
  `antaresRead::setSimulationPath(simulation="input")` in input mode.

- node:

  Character. Area for which to update data.

- output_node:

  List. Output of `MultiStock_H2_Investment_reward_compute_once`.

- candidates_types:

  Data_frame with column names : c("index", "name", "type", "TOTEX",
  "Marg_price"). It is a parameter of
  `MultiStock_H2_Investment_reward_compute_once`.
