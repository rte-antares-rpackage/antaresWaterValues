# Calculate water values from Bellman values, used in `Grid_Matrix` and `build_data_watervalues`

Calculate water values from Bellman values, used in `Grid_Matrix` and
`build_data_watervalues`

## Usage

``` r
value_node_gen(watervalues, statesdt, reservoir)
```

## Arguments

- watervalues:

  an intermediate result in Grid_Matrix contains the bellman values

- statesdt:

  an intermediate result in Grid_Matrix contains the states
  dicretization

- reservoir:

  an intermediate result in Grid_Matrix contains the reservoir levels
