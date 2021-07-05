start.time <- Sys.time()




#Test runWaterValuesSimulation()------------------------

{
library(antaresRead)
library(antaresEditObject)
library(dotenv)
library(data.table)
library(zoo)
library(dplyr)
library(tibble)
library(ggplot2)

# put the parameters.env file path------------------
load_dot_env("D:/Users/gharsallaouidhi/Documents/R coding/watervalues/parameters.env")
n=as.integer(Sys.getenv("n"))
solver=Sys.getenv("solv_path")
study=Sys.getenv("study_path")
area=Sys.getenv("area")

antaresRead::setSimulationPath(path = study, simulation = "input")

start.time <- Sys.time()


simulation_res <- runWaterValuesSimulation(
  area = area,                        # area étudiée
  path_solver=solver,
  fictive_area = "watervalues_fr",    # nom de l'area fictive qui sera créée
  nb_disc_stock = n,                  # nombre de simulations à lancer
  overwrite = TRUE,
  nb_mcyears=3
)
end.time <- Sys.time()

opts <- antaresRead::setSimulationPath(path = study, simulation = "input")
options("antares" = opts)


reward_dt <- get_Reward(simulation_names=simulation_res$simulation_names,district_name ="water values district" ,opts = opts)



results <- Grid_Matrix(      area = area,
                             simulation_names = simulation_res$simulation_names,
                             simulation_values = simulation_res$simulation_values,
                             nb_cycle = 1,
                             opts = opts,
                             week_53 = 0,
                             district_name = "water values district",
                             method="mean-grid",
                             states_step_ratio=0.1,
                             max_mcyears=NULL,
                             reservoir_capacity=NULL
)



post_process(results)

#viz

waterValuesViz(results)

#write to antares

results <- results[results$weeks!=53,]

reshaped_values <- toAntaresFormat(results)

writeWaterValues(
  area = area,
  data = reshaped_values
)


}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#----------------------------testing scripts-----------------


for (i in 1:52){
 for (j in 1:5){ # max mcyear
 temp <- watervalues[weeks==i&years==j]$value_node
 temp <- temp[2:length(temp)]
 temp[!is.finite(temp)] <- 0
 print(decr(temp))
 }
}



# test VU Monotonicity
for (i in 1:52){
  temp <- results[weeks==i]$vu
  temp <- temp[1:(length(temp)-1)]
  temp[is.na(temp)] <- Inf
  print(incr(temp))
}




# test Bellman values Monotonicity
for (i in 1:52){
    temp <- results[weeks==i]$value_node
     temp <- temp[1:(length(temp)-1)]
    temp[!is.finite(temp)] <- 0
    print(incr(temp))
}



# vu for usages values and any other thing for Bellman values
  plot_Bellman(results,17,"v")
