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


    #------User interface-----
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


          #----------Post process -----
          shiny::tabPanel(
            "Post Process",

            shiny::sidebarLayout(
              fluid = TRUE,

              shiny::sidebarPanel(
                shiny::h1("Post process"),
                shiny::h3("Remove extreme values"),
                shinyWidgets::switchInput(
                  "Run_remove_out",
                  value = F,
                  offStatus = "danger",
                  onStatus = "success"
                ) %>%
                  bsplus::shinyInput_label_embed(
                    bsplus::shiny_iconlink() %>%
                      bsplus::bs_embed_popover(title = "Activate filter to remove extreme values")
                  ),





                shiny::conditionalPanel(
                  condition = "input.Run_remove_out",
                  shiny::numericInput(
                    inputId = "min_rm",
                    label = "Minimum value to keep",
                    value = 0,
                    step = 1,
                    width = '100%'
                  ),
                  shiny::numericInput(
                    inputId = "max_rm",
                    label = "Maximum value to keep",
                    value = 200,
                    step = 1,
                    width = '100%'
                  ),

                  shiny::h3("Replace not defined values"),

                  shinyWidgets::radioGroupButtons(
                    inputId = "replace_na_method",
                    label = "Method to replace NaN values",
                    choices = c("Constant values", "Extreme values"),
                    individual = TRUE,
                    justified = TRUE
                  ),

                  shiny::conditionalPanel(
                    condition = "input.replace_na_method=='Constant values'",
                    shiny::numericInput("max_vu", "Max Water value", value =
                                          200),
                    shinyBS::bsTooltip(
                      "max_vu",
                      "The highest water value that you want to affect",
                      "bottom"
                    ),
                    shiny::numericInput("min_vu", "Min Water value", value =
                                          0),
                    shinyBS::bsTooltip(
                      "min_vu",
                      "The smallest water value that you want to affect",
                      "bottom"
                    ),
                  ),
                ),
                shinyBS::bsTooltip(
                  "min_rm",
                  "Delete all water values that are under this value",
                  "bottom"
                ),
                shinyBS::bsTooltip(
                  "max_rm",
                  "Delete all water values that are bigger then this value",
                  "bottom"
                ),





                shiny::tags$button(
                  id = "to_antares",
                  type = "button",
                  class = "btn action-button btn-large btn-primary",
                  shiny::img(
                    src = "https://antares-simulator.org/static/img/antares-256.png",
                    height = "50px",
                    shiny::HTML('<i class="icon-star"></i>To Antares')
                  )
                ),
                shinyBS::bsTooltip(
                  "to_antares",
                  "convert the water values to antares format than implemented in the desired area",
                  "bottom"
                )

              ),
              #sidebarPanel




              shiny::mainPanel(
                shinycustomloader::withLoader(
                  shiny::plotOutput("post_process"),
                  type = "html",
                  loader = "dnaspin"
                ),
                shinyWidgets::downloadBttn(
                  outputId = "download_pp_plot",
                  style = "unite",
                  color = "primary",
                  block = T
                ),

              )
            ) #sidebarLayout

          ) #end tabpanel "Post Process"

        ),
        #end navbarMenu

      ) #navbar

    ) #UI

    server <- function(input, output, session) {
      global <- shiny::reactiveValues(datapath = getwd())

      simulationServer("simulation", opts, silent)

      res <- calculateServer("calculate",opts,silent)

      bellmanServer("bellman",opts, res$watervalues, res$penalty_high,
                    res$penalty_low)

      rewardServer("reward", opts, res$reward_db)


      # #--------post process----------
      # final_result <- shiny::reactive({
      #   if (input$Run_remove_out) {
      #     remove_out(
      #       rv$results,
      #       min = input$min_rm,
      #       max = input$max_rm,
      #       replace_na_method = input$replace_na_method,
      #       max_vu = input$max_vu,
      #       min_vu = input$min_vu
      #     )
      #   } else{
      #     rv$results
      #   }
      # })
      #
      #
      #
      # output$post_process <-
      #   shiny::renderPlot(waterValuesViz(final_result()))
      #
      # output$download_pp_plot <- shiny::downloadHandler(
      #   filename = function() {
      #     paste('full water values-', Sys.Date(), '.png', sep = '')
      #   },
      #   content = function(con) {
      #     grDevices::png(con , width = 1200,
      #                    height = 766)
      #     print(waterValuesViz(final_result()))
      #     grDevices::dev.off()
      #   }
      # )
      #
      # shiny::observeEvent(input$to_antares, {
      #   results <- final_result()
      #   results <- results[results$weeks != 53,]
      #
      #   reshaped_values <- to_Antares_Format(results,
      #                                        input$penalty_low,
      #                                        input$penalty_high)
      #   antaresEditObject::writeWaterValues(area = input$Area,
      #                                       data = reshaped_values)
      #   shinyWidgets::show_alert(title = "Implement water values in Antares",
      #                            text = " Done !!",
      #                            type = "success")
      # })




    }


    #------Run-----
    options(shiny.launch.browser = TRUE)
    options(shiny.sanitize.errors = FALSE)
    options(shiny.fullstacktrace = FALSE)
    options(shiny.trace = F)
    options(shiny.error = NULL)
    shiny::shinyApp(ui = ui, server = server)
  }
