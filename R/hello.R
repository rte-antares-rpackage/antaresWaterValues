
library(antaresRead)
library(antaresEditObject)
#(antaresWaterValues)
library(dotenv)
library(data.table)
load_dot_env("D:/Users/gharsallaouidhi/Documents/R coding/antaresWaterValues/parameters.env")
n=as.integer(Sys.getenv("n"))
solver=Sys.getenv("solv_path")
study=Sys.getenv("study_path")
area=Sys.getenv("area")

antaresRead::setSimulationPath(path = study, simulation = "input")

simulation_res <- runWaterValuesSimulation(
  area = area,                        # area étudiée
  path_solver=solver,
  fictive_area = "watervalues_fr",    # nom de l'area fictive qui sera créée
  nb_disc_stock = n,                  # nombre de simulations à lancer
  overwrite = TRUE
)
