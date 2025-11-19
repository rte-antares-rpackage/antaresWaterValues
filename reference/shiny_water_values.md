# Open web interface for computing water values

Open web interface for computing water values. Select the study with
parameter `opts`.

## Usage

``` r
shiny_water_values(opts, silent = FALSE)
```

## Arguments

- opts:

  List of study parameters returned by the function
  `antaresRead::setSimulationPath(simulation="input")` in input mode.

- silent:

  Binary. `TRUE` to suppress warnings.
