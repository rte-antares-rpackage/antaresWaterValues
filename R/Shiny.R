#' Open watervalues Calculator in APP Web
#'
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param simulation_res
#'   List of simulation results returned by the function
#'   \code{watervalues::runWaterValuesSimulation}
#' @param silent Boolean. TRUE to suppress warnings.
#' @param ... further arguments passed to or from other methods.
#' @importFrom bsplus `%>%`
#' @import data.table
#' @import shinyBS
#' @import shiny
#' @export

shiny_water_values <-
  function(opts = NULL,
           simulation_res = NULL,
           silent = F,
           ...) {
    for (p in c(
      "bsplus",
      "DT",
      "grDevices",
      "shinyBS",
      "shinybusy",
      "shinycustomloader",
      "shinyjs",
      "shinythemes",
      "spsComps",
      "spsUtil",
      "tools",
      "shinyWidgets",
      "shiny"
    )) {
      if (!requireNamespace(p, quietly = TRUE)) {
        stop(paste0("Package ", p, " must be installed to use this function."),
             call. = FALSE)
      }
    }

    `%>%` <- bsplus::`%>%`
    if (is.null(opts)) {
      opts <- antaresRead::setSimulationPath(simulation = "input")
    }
    options("antares" = opts)

    ui <- shiny::fluidPage(
      shinyjs::useShinyjs(),
      theme = shinythemes::shinytheme("cerulean"),
      shinybusy::add_busy_gif(
        src = "https://github.com/dhia-gharsallaoui/watervalues/blob/main/static/calculating 3.gif?raw=true",
        position = "bottom-right",
        height = 70,
        width = 70
      ),

      shiny::navbarPage(
        "Water Values !",

        shiny::tabPanel("Simulations",
                        simulationUI("simulation", opts)),


        shiny::navbarMenu(
          "Water values calculation",
          shiny::tabPanel("Calculate Water Values",
                          calculateUI("calculate", opts)),

          shiny::tabPanel(
            "Bellman plot",
            bellmanUI("bellman",opts)
          ),


          shiny::tabPanel(
            "Rewards Plot",
            rewardUI("reward",opts)
          ),

          shiny::tabPanel(
            "Post Process",
            postProcessUI("post",opts)
          ),
        ),
        #end navbarMenu

      ) #navbar

    ) #UI

    server <- function(input, output, session) {
      global <- shiny::reactiveValues(datapath = getwd())

      simulationServer("simulation", opts, silent)

      res <- calculateServer("calculate",opts,silent)

      bellmanServer("bellman",opts, res$watervalues, res$penalty_high,
                    res$penalty_low, res$force_final_level,
                    res$penalty_final_level)

      rewardServer("reward", opts, res$reward_db)

      postProcessServer("post", opts, res$watervalues, res$penalty_high,
                        res$penalty_low, res$area, res$force_final_level,
                        res$penalty_final_level)

    }

    options(shiny.launch.browser = TRUE)
    options(shiny.sanitize.errors = FALSE)
    options(shiny.fullstacktrace = FALSE)
    options(shiny.trace = F)
    options(shiny.error = NULL)
    shiny::shinyApp(ui = ui, server = server)
  }
