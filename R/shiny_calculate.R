calculateUI <- function(id, opts) {
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::h1("Calculate watervalues"),
      shiny::h2("Study parameters"),

      shiny::fileInput(NS(id,"ini_file"), label = "Rdata file containing the simulations"),
      shinyBS::bsTooltip(
        NS(id,"ini_file"),
        " Select the Rdata file that contains the simulation results.",
        "bottom"
      ),

      #area
      shiny::textOutput(
        NS(id,"Area")),
      shinyBS::bsTooltip(
        NS(id,"Area"),
        "The area for which you will calculate the water values.",
        "bottom"
      ),

      shiny::textOutput(
        NS(id,"pumping_cal")),
      shinyBS::bsTooltip(
        NS(id,"pumping_cal"),
        "Whether pumping is possible."
      ),

      #MC years:
      shiny::textOutput(
        NS(id,"mcyears")),

      shinyBS::bsTooltip(
        NS(id,"mcyears"),
        " Monte-Carlo years considered in water values calculation.",
        "bottom"
      ),

      shiny::h2("Bellman values calculation"),

      #number of states:
      shiny::sliderInput(
        NS(id,"nb_states"),
        label = "Choose the number of states",
        min = 5,
        max = 100,
        value = 40,
        step = 1
      ),
      shinyBS::bsTooltip(
        NS(id,"nb_states"),
        " Discretization ratio to generate steps levels between the reservoir capacity and zero.",
        "bottom"
      ),

      # penalty for violation of the bottom rule curve
      shiny::numericInput(
        NS(id,"penalty_low"),
        "Penalty for the violation of the bottom rule curve (euros/MWh)",
        value = 3001
      ),
      shinyBS::bsTooltip(
        NS(id,"penalty_low"),
        "Penalty will be added proportionally to the distance from the rule curve, it is directly comparable with the cost of unsupplied energy.",
        "bottom"
      ),

      # penalty for violation of the top rule curve
      shiny::numericInput(
        NS(id,"penalty_high"),
        "Penalty for the violation of the top rule curve (euros/MWh)",
        value = 0
      ),
      shinyBS::bsTooltip(
        NS(id,"penalty_high"),
        "Penalty will be added proportionally to the distance from the rule curve, it is directly comparable with the cost of spilled energy.",
        "bottom"
      ),

      shinyWidgets::materialSwitch(
        NS(id,"final_level_exact"),
        "Final level should be respected for all scenarios (if not consider expectancy)",
        value = T,
        status = "success"
      ) %>%
        bsplus::shinyInput_label_embed(
          bsplus::shiny_iconlink() %>%
            bsplus::bs_embed_popover(
              title ="If true, penalties ensure that the final level is met, if not the expectancy of final levels should be equal to the objective and there is no hard constraint.")),

      shiny::numericInput(
        NS(id,"penalty_final_level_high"),
        "Penalty for final level (top rule curve)",
        value = 3001
      ),
      shiny::numericInput(
        NS(id,"penalty_final_level_low"),
        "Penalty for final level (bottom rule curve)",
        value = 3001
      ),

      shinyBS::bsTooltip(
        NS(id,"penalty_final_level_low"),
        "Penalty will be added proportionally to the distance from the expected final level.",
        "bottom"
      ),

      shinyBS::bsTooltip(
        NS(id,"penalty_final_level_high"),
        "Penalty will be added proportionally to the distance from the expected final level.",
        "bottom"
      ),

      shiny::actionButton(
        NS(id,"Calculate"),
        "Launch calculation",
        icon = shiny::icon("check-circle"),
        align = "center"
      ),
      shinyBS::bsTooltip(
        NS(id,"Calculate"),
        "Click to start the calculation of te water values using the selected parameters",
        "bottom"
      ),
      shiny::h2("Plot"),

      shinyWidgets::materialSwitch(
        NS(id,"filter"),
        "Filter water values",
        value = F,
        status = "success"
      ) %>%
        bsplus::shinyInput_label_embed(
          bsplus::shiny_iconlink() %>%
            bsplus::bs_embed_popover(title = "Visualize only watervalues inside rule curves")
        ),


      shiny::actionButton(NS(id,"plot"), "Show"),
      shinyBS::bsTooltip(NS(id,"plot"), " Show the Graph",
                         "bottom"),
    ),

    shiny::mainPanel(
      shinycustomloader::withLoader(
        shiny::plotOutput(NS(id,"Watervalues")),
        type = "html",
        loader = "dnaspin"
      ),
      shinyWidgets::downloadBttn(
        outputId = NS(id,"download_wv_plot"),
        style = "unite",
        color = "primary",
        block = T
      ),

    )
  )
}

calculateServer <- function(id, opts, silent) {
  moduleServer(id, function(input, output, session) {
    res <- shiny::reactiveValues()

    simulation_res <- shiny::reactive({
      if (is.null(input$ini_file)) {
        NULL
      } else{
        inFile <- input$ini_file
        file <- inFile$datapath
        # load the file into new environment and get it from there
        e = new.env()
        name <- load(file, envir = e)
        simulation_res <- e[[name]]
        simulation_res
      }
    })

    output$Area <- shiny::renderText({
      if(is.null(simulation_res())){"Area :"}
      else paste0("Area : ",simulation_res()$area)})

    output$pumping_cal <- shiny::renderText({
      if(is.null(simulation_res())){"Pumping :"}
      else {
        if(simulation_res()$pumping) {
          paste0("Pumping : yes with efficiency ",simulation_res()$eff)}
        else {
          "Pumping : no"
        }
      }
    })

    output$mcyears <- shiny::renderText({
      if(is.null(simulation_res())){"Monte Carlo years :"}
      else {
        paste0("Monte Carlo years : from ",simulation_res()$mcyears[1], " to ",
               utils::tail(simulation_res()$mcyears,n=1))}
    })

    constraints <- shiny::reactive({
      constraint_generator(
          area = simulation_res()$area,
          opts = opts,
          pumping = simulation_res()$pumping,
          nb_disc_stock = 20,
          efficiency = simulation_res()$eff,
          mcyears = simulation_res()$mcyears
        )
    })

    shiny::observeEvent(input$Calculate,

                        spsUtil::quiet({
                          spsComps::shinyCatch({
                            reward_db <-
                              get_Reward(
                                simulation_names = simulation_res()$simulation_names,
                                simulation_values = simulation_res()$simulation_values,
                                opts = opts,
                                method_old = F,
                                possible_controls = constraints(),
                                max_hydro_hourly = get_max_hydro(simulation_res()$area, opts),
                                mcyears = simulation_res()$mcyears,
                                area = simulation_res()$area,
                                expansion = simulation_res()$expansion
                              )

                            results <-     Grid_Matrix(
                              area = simulation_res()$area,
                              reward_db = reward_db,
                              nb_cycle = if(input$final_level_exact){1}else{2},
                              opts = opts,
                              week_53 = 0,
                              states_step_ratio = (1 / input$nb_states),
                              mcyears = simulation_res()$mcyears,
                              shiny = T,
                              efficiency = simulation_res()$eff,
                              penalty_low = input$penalty_low,
                              penalty_high = input$penalty_high,
                              penalty_final_level_low = input$penalty_final_level_low,
                              penalty_final_level_high = input$penalty_final_level_high
                            )$aggregated_results

                            shiny::isolate(res$results <- results)
                            shiny::isolate(res$reward_db <- reward_db)
                            shinyWidgets::show_alert(title = "Water Values",
                                                     text = "Calculation Done !!",
                                                     type = "success")


                          }, blocking_level = "error", position = "top-center", shiny = TRUE, prefix = "")

                        }, print_cat = F, message = F, warning = silent))





    watervalues <- shiny::eventReactive(input$plot,
                                        {
                                          waterValuesViz(res$results, input$filter)
                                        })

    output$Watervalues <- shiny::renderPlot(watervalues())

    shinyBS::addPopover(session,
                        "Watervalues",
                        title = "water values",
                        content = "This graph describe the water values for each week starting from week 1 to week 52 in the X-axis and the level of the reservoir in perecent in the Y-axis. the water values are determined by the colors you can see them in the legend of the graph. ")

    output$download_wv_plot <- shiny::downloadHandler(
      filename = function() {
        paste('watervalues-', Sys.Date(), '.png', sep = '')
      },
      content = function(con) {
        grDevices::png(con , width = 1200,
                       height = 766)
        print(watervalues())
        grDevices::dev.off()
      }
    )

    list(
      watervalues = reactive(res$results),
      reward_db = reactive(res$reward_db),
      area = reactive(simulation_res()$area)
    )
  })
}
