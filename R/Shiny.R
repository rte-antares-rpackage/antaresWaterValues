#' Open web interface for computing water values
#'
#' Open web interface for computing water values. Select the study with parameter \code{opts}.
#'
#' @inheritParams runWaterValuesSimulation
#' @param silent Binary. \code{TRUE} to suppress warnings.
#' @importFrom bsplus `%>%`
#' @import data.table
#' @import shinyBS
#' @import shiny
#' @export
shiny_water_values <-
  function(opts,
           silent = FALSE) {
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

        shiny::tabPanel("Launch Antares simulations",
                        simulationUI("simulation", opts)),

        shiny::tabPanel("Calculate Water Values",
                        calculateUI("calculate", opts)),

        shiny::tabPanel("Export results to Antares",
                        postProcessUI("post",opts)),

        shiny::navbarMenu(
          "Analyze results",

          shiny::tabPanel(
            "Bellman plot",
            bellmanUI("bellman",opts)
          ),


          shiny::tabPanel(
            "Rewards Plot",
            rewardUI("reward",opts)
          ),
        ),
        #end navbarMenu
        shiny::tabPanel(
          "Iterative calculation of Bellman values",
          iterativeUI("iterative",opts)
        ),

      ) #navbar

    ) #UI

    server <- function(input, output, session) {
      global <- shiny::reactiveValues(datapath = getwd())

      simulationServer("simulation", opts, silent)

      res <- calculateServer("calculate",opts,silent)

      bellmanServer("bellman",opts, res$watervalues)

      rewardServer("reward", opts, res$reward_db)

      postProcessServer("post", opts, res$watervalues, res$area)

      iterativeServer("iterative",opts, silent)

    }

    options(shiny.launch.browser = TRUE)
    options(shiny.sanitize.errors = FALSE)
    options(shiny.fullstacktrace = FALSE)
    options(shiny.trace = FALSE)
    options(shiny.error = NULL)
    shiny::shinyApp(ui = ui, server = server)
  }
