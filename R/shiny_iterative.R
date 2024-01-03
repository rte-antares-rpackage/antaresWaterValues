iterativeUI <- function(id, opts) {
  shiny::tagList(shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::h2("Study parameters"),
      shinyWidgets::pickerInput(
        NS(id, "itr_sim_area"),
        "Choose the area",
        opts$areaList,
        options = list(`live-search` = TRUE)
      ) %>%
        bsplus::shinyInput_label_embed(
          bsplus::shiny_iconlink() %>%
            bsplus::bs_embed_popover(title = "The area concerned by the simulation.")
        ),


      shiny::textInput(NS(id, "itr_solver_path"), "Solver path "
                       , value = "xxxxxxx/bin/antares-8.0-solver.exe"),

      shinyBS::bsTooltip(
        NS(id, "itr_solver_path"),
        "The path of the Antares solver you found in your Antares installation directory.",
        "bottom"
      ),

      shiny::h2("Simulation parameters"),
      shinyWidgets::materialSwitch(
        NS(id, "itr_pumping"),
        "Pumping",
        value = F,
        status = "success"
      ) %>%
        bsplus::shinyInput_label_embed(
          bsplus::shiny_iconlink() %>%
            bsplus::bs_embed_popover(title = "Take pumping into account. Use it when your simulations have aggregated pumping.")
        ),

      shiny::conditionalPanel(
        ns = NS(id),
        condition = "input.itr_pumping",
        shiny::uiOutput(NS(id, "itr_eff"))
      ),

      shiny::sliderInput(
        NS(id, "itr_sim_mcyears"),
        label = "Choose the number of MC years to simulate",
        min = 1,
        max = opts$parameters$general$nbyears,
        value = c(1, opts$parameters$general$nbyears),
        step = 1
      ),

      shinyBS::bsTooltip(
        NS(id, "itr_sim_mcyears"),
        " Number of Monte Carlo years to simulate.",
        "bottom"
      ),

      shiny::numericInput(
        NS(id, "itr_max"),
        "Maximum number of simulations",
        min = 1,
        value = 3
      ),

      shiny::h2("Bellman values calculation parameters"),

      shiny::sliderInput(
        NS(id, "itr_hours"),
        "Number of hours to use to calculate rewards",
        max = 168,
        min = 1,
        value = 10
      ),
      shiny::numericInput(
        NS(id, "itr_controls"),
        "Number of controls to calculate",
        min = 0,
        value = 3
      ),

      #number of states:
      shiny::sliderInput(
        NS(id, "itr_nb_states"),
        label = "Choose the number of states",
        min = 5,
        max = 100,
        value = 40,
        step = 1
      ),
      shinyBS::bsTooltip(
        NS(id, "itr_nb_states"),
        " Discretization ratio to generate steps levels between the reservoir capacity and zero.",
        "bottom"
      ),

      shinyWidgets::radioGroupButtons(
        inputId = NS(id, "method_dp"),
        label = "Select algorithm to use",
        choices = c("grid-mean", "mean-grid", "quantile"),
        individual = TRUE,
        justified = TRUE,
        checkIcon = list(yes = shiny::icon("ok",
                                           lib = "glyphicon"))
      ),
      shinyBS::bsTooltip(
        NS(id, "method_dp"),
        " Select the algorithm to use in calculation for more information check documentation.",
        "bottom"
      ),

      shiny::conditionalPanel(
        ns = NS(id),
        condition = "input.method_dp=='quantile'",
        shiny::sliderInput(
          NS(id, "q_ratio_dp"),
          label = NULL,
          min = 0,
          max = 100,
          value = 50,
          post  = " %"
        ),
      ),
      shinyBS::bsTooltip(
        NS(id, "q_ratio_dp"),
        "The bellman values selected in each week  give q_ratio of all bellman values are equal or less to it.",
        "bottom"
      ),

      # penalty for violation of the bottom rule curve
      shiny::numericInput(
        NS(id, "itr_penalty_low"),
        "Penalty for the violation of the bottom rule curve",
        value = 3001
      ),
      shinyBS::bsTooltip(
        NS(id, "itr_penalty_low"),
        "Penalty will be added proportionally to the distance from the rule curve, it is directly comparable with the cost of unsupplied energy.",
        "bottom"
      ),

      # penalty for violation of the top rule curve
      shiny::numericInput(
        NS(id, "itr_penalty_high"),
        "Penalty for the violation of the top rule curve",
        value = 0
      ),
      shinyBS::bsTooltip(
        NS(id, "itr_penalty_high"),
        "Penalty will be added proportionally to the distance from the rule curve, it is directly comparable with the cost of spilled energy.",
        "bottom"
      ),

      shiny::actionButton(
        NS(id, "itr_calculate"),
        "Launch calculation",
        icon = shiny::icon("check-circle"),
        align = "center"
      ),
      shinyBS::bsTooltip(
        NS(id, "itr_calculate"),
        "Click to start the calculation of te water values using the selected parameters",
        "bottom"
      ),

      shiny::h2("Results"),

      shinyWidgets::materialSwitch(
        NS(id, "itr_filter"),
        "Filter water values",
        value = F,
        status = "success"
      ) %>%
        bsplus::shinyInput_label_embed(
          bsplus::shiny_iconlink() %>%
            bsplus::bs_embed_popover(title = "Visualize only watervalues inside rule curves")
        ),


      shiny::actionButton(NS(id, "itr_plot"), "Show"),
      shinyBS::bsTooltip(NS(id, "itr_plot"), " Show the Graph",
                         "bottom"),

      shiny::actionButton(NS(id, "itr_to_antares"), "Write water values to Antares"),


    ),
    #end sidebarPanel

    shiny::mainPanel(
      shinycustomloader::withLoader(shiny::plotOutput(NS(
        id, "itr_watervalues"
      )), type = "html", loader = "dnaspin")
    )

  ))
}

iterativeServer <- function(id, opts, silent) {
  moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues()

    shiny::observeEvent(input$itr_calculate,
                        spsUtil::quiet({
                          spsComps::shinyCatch({
                            results <-  calculateBellmanWithIterativeSimulations(
                              area = input$itr_sim_area,
                              pumping = input$itr_pumping,
                              pump_eff = input$itr_efficiency,
                              opts = opts,
                              nb_control = input$itr_controls,
                              nb_itr = input$itr_max,
                              mcyears = input$itr_sim_mcyears[1]:input$itr_sim_mcyears[2],
                              penalty_low = input$itr_penalty_low,
                              penalty_high = input$itr_penalty_high,
                              path_solver = input$itr_solver_path,
                              study_path = opts$studyPath,
                              hours = round(seq(0, 168, length.out = input$itr_hours)),
                              states_step_ratio = 1 / input$itr_nb_states,
                              method_dp = input$method_dp,
                              q_ratio = input$q_ratio_dp / 100
                            )$results

                            shiny::isolate(rv$itr_results <- results)
                            shinyWidgets::show_alert(title = "Water Values",
                                                     text = "Calculation Done !!",
                                                     type = "success")

                          },
                          blocking_level = "error", position = "top-center", shiny = TRUE, prefix = "")

                        },
                        print_cat = F, message = F, warning = silent))

    itr_watervalues <- shiny::eventReactive(input$itr_plot,
                                            {
                                              waterValuesViz(rv$itr_results, input$itr_filter)
                                            })

    output$itr_watervalues <- shiny::renderPlot(itr_watervalues())

    output$itr_eff <- shiny::renderUI({ shiny::numericInput(NS(id,"itr_efficiency"),"Efficiency pumping ratio",min=0,max=1,
                                                            value=getPumpEfficiency(area=input$itr_sim_area,opts = opts))
    })

    shiny::observeEvent(input$itr_to_antares,
                        {
                          reshaped_values <-
                            rv$itr_results[rv$itr_results$weeks != 53, ] %>% to_Antares_Format(input$itr_penalty_low, input$itr_penalty_high)
                          antaresEditObject::writeWaterValues(area = input$itr_sim_area,
                                                              data = reshaped_values)
                          shinyWidgets::show_alert(title = "To antares",
                                                   text = "Done !",
                                                   type = "success")
                        })
  })
}
