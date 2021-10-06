test_that("check largest_decsions()",
          {
            expect_equal(largest_decisions(states=1000,value_inflow=500,niveau_max=3000,E_max=750,P_max=1000)$largest_turb,750)
            expect_equal(largest_decisions(states=1000,value_inflow=500,niveau_max=3000,E_max=750,P_max=1000)$largest_pump,-1000)

            expect_equal(largest_decisions(states=0,value_inflow=500,niveau_max=3000,E_max=750,P_max=1000)$largest_turb,500)
            expect_equal(largest_decisions(states=2000,value_inflow=500,niveau_max=3000,E_max=750,P_max=1000)$largest_pump,-500)
          }        )



test_that("check check_largest_decisions()",
          {
            decision_space <- seq(-5000,5000,by=1000)
            largest_decisions <-list()
            largest_decisions$largest_turb <- 2500
            largest_decisions$largest_pump <- -2500
            alpha=1

            expect_equal(check_largest_decisions(decision_space,largest_decisions,alpha),seq(-2000,2000,by=1000))

            largest_decisions$largest_pump <- 0

            expect_equal(check_largest_decisions(decision_space,largest_decisions,alpha),seq(0,2000,by=1000))

            largest_decisions$largest_pump <- -2500
            largest_decisions$largest_turb <- 0

            expect_equal(check_largest_decisions(decision_space,largest_decisions,alpha),seq(-2000,0,by=1000))

          }
          )



test_that("check turbined_energy()",
          {
            largest_decisions <-list()
            largest_decisions$largest_turb <- 25000
            largest_decisions$largest_pump <- -25000
            states <- 30000
            next_states <- seq(0,100000,by=5000)
            value_inflow <- 900
            decisions_current <- c(-50000,50000)
            res <- c(20900, 15900, 10900, 5900, 900, -4100, -9100, -14100, -19100,-24100)
            expect_equal(turbined_energy(states,next_states,value_inflow,decisions_current,largest_decisions),res)

            largest_decisions$largest_turb <- 100000
            largest_decisions$largest_pump <- -100000
            decisions_current <- c(-100000,100000)
            value_inflow <- 10000
            res <- 40000-next_states
            expect_equal(turbined_energy(states,next_states,value_inflow,decisions_current,largest_decisions),res)

          })



test_that("check decision_cover()",
          {
            turbined_energy <- seq(-2000,3000,by=1000)
            decisions_current <-seq(-5000,5000,by=1000)
            expect_equal(decisions_cover(turbined_energy,decisions_current),turbined_energy)

            turbined_energy <- append(turbined_energy,3500)
            res <- append(turbined_energy,4000)
            expect_equal(decisions_cover(turbined_energy,decisions_current), res)

            turbined_energy <- append(turbined_energy,-2500,after=0)
            res <- append(turbined_energy,-3000,after = 0)
            res <- append(res,4000)
            expect_equal(decisions_cover(turbined_energy,decisions_current), res)

          })


test_that("check accessible_reward()",
          {
            decision_cover <- c(-35000,55000)
            decision_space <- seq(-90000,100000,by=10000)
            value_reward <- decision_space*2.5

            res_steps <- seq(-30000,50000,by=10000)
            res_rewards <-  res_steps*2.5
            expect_equal(accessible_rewards(decision_cover,decision_space,value_reward)$step,res_steps)
            expect_equal(accessible_rewards(decision_cover,decision_space,value_reward)$rewards,res_rewards)


            decision_cover <- c(-40000,60000)
            res_steps <- seq(-40000,60000,by=10000)
            res_rewards <-  res_steps*2.5

            expect_equal(accessible_rewards(decision_cover,decision_space,value_reward)$step,res_steps)
            expect_equal(accessible_rewards(decision_cover,decision_space,value_reward)$rewards,res_rewards)

          })
