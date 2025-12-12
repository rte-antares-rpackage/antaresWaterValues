# Modify the reward function to take into account the effects of a must-run cluster.

Modify the reward function to take into account the effects of a
must-run cluster.

## Usage

``` r
update_reward_cluster_bande(power, reward_init)
```

## Arguments

- power:

  Integer. Capacity of the cluster.

- reward_init:

  List. Rewards and decision space from the output of the function
  [`antaresWaterValues::get_Reward()`](https://rte-antares-rpackage.github.io/antaresWaterValues/reference/get_Reward.md).

## Value

a `list` of rewards and decision space, like `reward_init`.
