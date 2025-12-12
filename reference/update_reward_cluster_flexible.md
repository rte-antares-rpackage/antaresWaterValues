# Modify the reward function to take into account the effects of a non must-run cluster, with a speciifc capcaity and marginal cost.

Modify the reward function to take into account the effects of a non
must-run cluster, with a speciifc capcaity and marginal cost.

## Usage

``` r
update_reward_cluster_flexible(power, marg_cost, reward_init, mcYear)
```

## Arguments

- power:

  Integer. Capacity of the cluster.

- marg_cost:

  Numeric. Marginal cost (and market bid) of the cluster in eur/MWh.

- reward_init:

  List. Rewards and decision space from the output of the function
  [`antaresWaterValues::get_Reward()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/get_Reward.md).

- mcYear:

  Vector of integers. Monte Carlo years of the rewards.

## Value

a `list` of rewards and decision space, like `reward_init`.
