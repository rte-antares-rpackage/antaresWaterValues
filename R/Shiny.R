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
          #--------- Bellman graphs UI -----
          shiny::tabPanel(
            "Bellman plot",
            bellmanUI("bellman",opts)
          ),

          #------ Reward Plot ------------
          shiny::tabPanel(
            "Rewards Plot",

            shiny::sidebarLayout(
              shiny::sidebarPanel(
                shiny::h1("Rewards plot"),

                shiny::actionButton("import_reward", "Import reward"),

                shiny::sliderInput(
                  "week_id_rew",
                  "Week to show",
                  value = c(2, 2),
                  min = 1,
                  max = 52
                ),

                shinyBS::bsTooltip("week_id_rew", "The weeks you want to plot",
                                   "bottom"),

                shiny::selectInput(
                  "param_rew",
                  "Type",
                  c(
                    "Reward" = "r",
                    "Reward 1 MC" = "r1",
                    "Reward variation" = "rv",
                    "reward variation 1Mc" =
                      "rv1"
                  )
                ),

                shiny::conditionalPanel(
                  condition = "['rv1','r1'].includes(input.param_rew)",
                  shiny::sliderInput(
                    "Mc_year",
                    label = "Monte-Carlo year",
                    min = 1,
                    max = opts$parameters$general$nbyears,
                    value = c(1, 1)
                    ,
                    step = 1
                  )
                ),


                shinyBS::bsTooltip("Mc_year", "The scenarios that you want to plot",
                                   "bottom"),

                shinyWidgets::downloadBttn(
                  outputId = "download_reward_base",
                  style = "unite",
                  color = "primary",
                  block = T
                )
              ),

              shiny::mainPanel(
                shiny::plotOutput("rewardplot"),
                shinyWidgets::downloadBttn(
                  outputId = "download_reward_plot",
                  style = "unite",
                  color = "primary",
                  block = T
                ),
                # shiny::plotOutput("reward_second_plot"),

                DT::dataTableOutput("reward_table")
              )


            )


          ),
          #end tabpanel 3


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

    #------Server functions ------
    server <- function(input, output, session) {
      global <- shiny::reactiveValues(datapath = getwd())

      simulationServer("simulation", opts, silent)

      res <- calculateServer("calculate",opts,silent)

      bellmanServer("bellman",opts,silent, res$watervalues, res$penalty_high,
                    res$penalty_low)

      #--------plot reward page------

      # possible_controls <- shiny::reactive({
      #   possible_controls <- if (input$smart_interpolation_reward) {
      #     rbind(
      #       simulation_res()$simulation_values,
      #       constraint_generator(
      #         area = input$Area,
      #         opts = opts,
      #         pumping = input$pumping_cal,
      #         nb_disc_stock = input$controls,
      #         pumping_efficiency = input$efficiency
      #       )
      #     ) %>%
      #       dplyr::select("week", "u") %>%
      #       dplyr::distinct() %>%
      #       dplyr::arrange(week, .data$u)
      #   } else {
      #     simulation_res()$simulation_values %>% dplyr::select("week", "u")
      #   }
      #   possible_controls
      # })
      #
      # shiny::observeEvent(input$import_reward,
      #                     {
      #                       spsComps::shinyCatch({
      #                         reward_dt <-
      #                           get_Reward(
      #                             simulation_names = simulation_res()$simulation_names,
      #                             simulation_values = simulation_res()$simulation_values,
      #                             district_name = "water values district",
      #                             opts = opts,
      #                             method_old = !input$smart_interpolation_reward,
      #                             hours = if (input$smart_interpolation_reward) {
      #                               round(seq(0, 168, length.out = input$hours))
      #                             },
      #                             possible_controls = possible_controls(),
      #                             max_hydro = if (input$smart_interpolation_reward) {
      #                               get_max_hydro(input$Area, opts)
      #                             },
      #                             mcyears = input$mcyears[1]:input$mcyears[2],
      #                             area = input$Area,
      #                             district_balance = "water values district"
      #                           )
      #                         rv$reward_dt <- reward_dt
      #                         shinybusy::remove_modal_spinner()
      #                         shinyWidgets::show_alert(title = "Rewards",
      #                                                  text = "Importation Done !!",
      #                                                  type = "success")
      #                       })
      #                     })
      #
      #
      # rewardplot <- shiny::reactive({
      #   if (is.null(rv$reward))
      #     rv$reward <- reward_db()
      #
      #   week_id_rew <- input$week_id_rew[1]:input$week_id_rew[2]
      #   Mc_year <- input$Mc_year[1]:input$Mc_year[2]
      #   if (input$param_rew == "r")
      #   {
      #     plot_reward(rv$reward_dt$reward, week_id_rew)
      #   } else{
      #     if (input$param_rew == "rv")
      #     {
      #       plot_reward_variation(rv$reward_dt$reward, week_id_rew)
      #     } else{
      #       if (input$param_rew == "r1")
      #       {
      #         plot_reward_mc(rv$reward_dt$reward, week_id_rew,
      #                        Mc_year)
      #       } else{
      #         if (input$param_rew == "rv1")
      #         {
      #           plot_reward_variation_mc(rv$reward_dt$reward, week_id_rew,
      #                                    Mc_year)
      #         }
      #       }
      #     }
      #   }
      # })
      #
      # reward_var_plot <- shiny::reactive({
      #   week_id_rew <- input$week_id_rew[1]:input$week_id_rew[2]
      #   Mc_year <- input$Mc_year[1]:input$Mc_year[2]
      #
      #   if (input$param_rew == "r1")
      #   {
      #     plot_reward_variation_mc(rv$reward_dt$reward, week_id_rew,
      #                              Mc_year)
      #   } else{
      #     plot_reward_variation(rv$reward_dt$reward, week_id_rew)
      #   }
      #
      #
      # })
      #
      #
      #
      #
      # output$rewardplot <- shiny::renderPlot(rewardplot()$graph)
      # # output$reward_second_plot <- shiny::renderPlot(reward_var_plot()$graph)
      # output$reward_table <- DT::renderDataTable(rewardplot()$table)
      # output$download_reward_plot <- shiny::downloadHandler(
      #   filename = function() {
      #     paste('Reward-', Sys.Date(), '.png', sep = '')
      #   },
      #   content = function(con) {
      #     grDevices::png(con , width = 1200, height = 766)
      #     print(rewardplot()$graph)
      #     grDevices::dev.off()
      #   }
      # )
      #
      #
      # shiny::observe({
      #   if (!is.null(rv$reward_dt))
      #     shiny::isolate(reward_base <- rv$reward_dt)
      # })
      #
      # output$download_reward_base <- shiny::downloadHandler(
      #   filename <- function() {
      #     paste('Reward-Base-', Sys.Date(), '.Rdata', sep = '')
      #   },
      #
      #   content = function(file) {
      #     save(reward_base, file = file)
      #   }
      # )
      # # end reward Plot
      #
      #
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
