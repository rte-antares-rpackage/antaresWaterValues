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

      shiny::h2("Reward calculation"),
      # Reward calculation
      shinyWidgets::materialSwitch(
        NS(id,"smart_interpolation_reward"),
        "Use marginal prices to interpolate rewards",
        value = F,
        status = "success"
      ) %>%
        bsplus::shinyInput_label_embed(
          bsplus::shiny_iconlink() %>%
            bsplus::bs_embed_popover(title = "If marginal prices are used, one can use less Antares simulation to retrieve reward function")
        ),

      shiny::conditionalPanel(
        ns = NS(id),
        condition = "input.smart_interpolation_reward",
        shiny::sliderInput(
          NS(id,"hours"),
          "Number of hours to use to calculate rewards",
          max = 168,
          min = 1,
          value = 10
        ),
        shiny::numericInput(
          NS(id,"controls"),
          "Number of controls to calculate",
          min = 0,
          value = 3
        )
      ),

      shiny::conditionalPanel(
        ns = NS(id),
        condition = "!input.smart_interpolation_reward",
        # correct monotony option for gains
        shinyWidgets::materialSwitch(
          NS(id,"correct_monotony_gain"),
          "Correct monotony of gains",
          value = F,
          status = "success"
        ) %>%
          bsplus::shinyInput_label_embed(
            bsplus::shiny_iconlink() %>%
              bsplus::bs_embed_popover(title = "Correct monotony of gain, ie the more water is turbined the less the cost of the electric system is high")
          )
      ),

      shiny::h2("Bellman values calculation"),
      # Algorithm
      shinyWidgets::radioGroupButtons(
        inputId = NS(id,"method"),
        label = "Select algorithm to use",
        choices = c("grid-mean", "mean-grid", "quantile"),
        individual = TRUE,
        justified = TRUE,
        checkIcon = list(yes = shiny::icon("ok",
                                           lib = "glyphicon"))
      ),
      shinyBS::bsTooltip(
        NS(id,"method"),
        " Select the algorithm to use in calculation for more information check documentation.",
        "bottom"
      ),


      shiny::conditionalPanel(
        ns = NS(id),
        condition = "input.method=='quantile'",
        shiny::sliderInput(
          NS(id,"q_ratio"),
          label = NULL,
          min = 0,
          max = 100,
          value = 50,
          post  = " %"
        ),
      ),
      shinyBS::bsTooltip(
        NS(id,"q_ratio"),
        "The bellman values selected in each week  give q_ratio of all bellman values are equal or less to it.",
        "bottom"
      ),

      shinyWidgets::materialSwitch(
        NS(id,"until_convergence"),
        "Repeat until convergence",
        value = F,
        status = "success"
      ) %>%
        bsplus::shinyInput_label_embed(
          bsplus::shiny_iconlink() %>%
            bsplus::bs_embed_popover(title = "Repeat the calculation using in each time the last calculation as initial condition until the convergence.")
        ),

      #number of cycles
      shiny::conditionalPanel(
        ns = NS(id),
        condition = "!input.until_convergence",
        shiny::numericInput(
          NS(id,"nb_cycle"),
          "Number of cycle to calculate",
          value = 2,
          min = 1
        )
      ),
      shinyBS::bsTooltip(
        NS(id,"nb_cycle"),
        " Number of times to run the algorithm to reduce the initial values effects.",
        "bottom"
      ),


      shiny::conditionalPanel(
        ns = NS(id),
        condition = "input.until_convergence",
        shiny::sliderInput(
          NS(id,"convergence_rate"),
          "Convergence goal",
          max = 100,
          min = 0,
          value = 90,
          post  = " %"
        ),
        shiny::numericInput(NS(id,"convergence_criteria"), "convergence landmark", value =
                              1),
        shiny::numericInput(NS(id,"cycle_limit"), "Number of Cycles limit ", value =
                              10)
      ),

      shinyBS::bsTooltip(
        NS(id,"convergence_rate"),
        "The convergence level from which we suppose that no need to continue another cycle.",
        "bottom"
      ),
      shinyBS::bsTooltip(
        NS(id,"convergence_criteria"),
        "the value define convergence. if the difference between two water values is less then this value those values are converged.",
        "bottom"
      ),
      shinyBS::bsTooltip(
        NS(id,"cycle_limit"),
        "The maximum number of cycles to calculate before convergence.",
        "bottom"
      ),

      #week 53 value
      shiny::numericInput(NS(id,"week_53"), "Water value initial condition", value =
                            0),
      shinyBS::bsTooltip(
        NS(id,"week_53"),
        " Water values for week 53, will be mutiplied by the half capacity of the reservoir to genrate an approximitive bellman values as initial condition",
        "bottom"
      ),

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
        NS(id,"force_final_level"),
        "Force final level",
        value = F,
        status = "success"
      ) %>%
        bsplus::shinyInput_label_embed(
          bsplus::shiny_iconlink() %>%
            bsplus::bs_embed_popover(title = "This option modifies rule curves in the calculation to force the final level to be egal to the initial level. There is no hard constraint in simulation, only penalties as defined below.")
        ),

      shiny::conditionalPanel(
        ns = NS(id),
        condition = "input.force_final_level",
        shinyWidgets::materialSwitch(
          NS(id,"final_level_egal_initial"),
          "Final level should be equal to initial level",
          value = T,
          status = "success"
        ) %>%
          bsplus::shinyInput_label_embed(
            bsplus::shiny_iconlink() %>%
              bsplus::bs_embed_popover(title ="If the final level should be equal to the initial level. There could be a deviation of the final level to the closest integer due to the implementation of penalties through water values.")),
        shiny::numericInput(
          NS(id,"penalty_final_level"),
          "Penalty for final level",
          value = 3001
        ),
      ),

      shinyBS::bsTooltip(
        NS(id,"penalty_final_level"),
        "Penalty will be added proportionally to the distance from the expected final level.",
        "bottom"
      ),

      shiny::conditionalPanel(
        ns = NS(id),
        condition = "!input.final_level_egal_initial",
        shiny::numericInput(
          NS(id,"final_level"),
          "Final level (%)",
          value = 0
        ),
      ),

      shinyBS::bsTooltip(
        NS(id,"final_level"),
        "There could be a deviation of the final level to the closest integer.",
        "bottom"
      ),


      # correct concavity option for Bellman values
      shinyWidgets::materialSwitch(
        NS(id,"correct_concavity"),
        "Correct concavity of Bellman values",
        value = F,
        status = "success"
      ) %>%
        bsplus::shinyInput_label_embed(
          bsplus::shiny_iconlink() %>%
            bsplus::bs_embed_popover(title = "Correct concavity of Bellman values to have monotone water values.")
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

      DT::dataTableOutput(NS(id,"calculated_controls"))

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
        paste0("Monte Carlo years : from ",simulation_res()$mc_years[1], " to ",
               utils::tail(simulation_res()$mc_years,n=1))}
    })

    constraints <- shiny::reactive({
      constraint_generator(
        area = simulation_res()$area,
        opts = opts,
        pumping = simulation_res()$pumping,
        nb_disc_stock = input$controls,
        pumping_efficiency = simulation_res()$eff
      )
    })

    calculated_controls <- shiny::reactive({
      rbind(
        dplyr::mutate(simulation_res()$simulation_values, From = "Simulation"),
        dplyr::mutate(constraints(),
          From = "Calculated controls"
        )
      ) %>%
        dplyr::select(-c("sim")) %>%
        dplyr::group_by(.data$week, .data$From) %>%
        dplyr::summarise(Controls = list(.data$u), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = .data$From,
                           values_from = .data$Controls) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(Union = list(union(
          .data$`Calculated controls`, .data$Simulation
        ))) %>%
        tidyr::pivot_longer(cols = 2:4,
                            names_to = "From",
                            values_to = "Controls") %>%
        dplyr::mutate(Total = lengths(.data$Controls)) %>%
        dplyr::mutate(Controls = as.character(.data$Controls)) %>%
        as.data.table()
    })

    output$calculated_controls <- DT::renderDataTable({
      if (is.null(simulation_res())){data.frame()}
      else {
        if (input$smart_interpolation_reward)  calculated_controls()
        else {
          dplyr::filter(calculated_controls(),.data$From=="Simulation")
        }
      }
    })

    possible_controls <- shiny::reactive({
      possible_controls <- if (input$smart_interpolation_reward) {
        rbind(
          simulation_res()$simulation_values,
          constraints()
        ) %>%
          dplyr::select("week", "u") %>%
          dplyr::distinct() %>%
          dplyr::arrange(week, .data$u)
      } else {
        simulation_res()$simulation_values %>% dplyr::select("week", "u")
      }
      possible_controls
    })

    shiny::observeEvent(input$Calculate,

                        spsUtil::quiet({
                          spsComps::shinyCatch({
                            reward_db <-
                              get_Reward(
                                simulation_names = simulation_res()$simulation_names,
                                simulation_values = simulation_res()$simulation_values,
                                district_name = "water values district",
                                opts = opts,
                                method_old = !input$smart_interpolation_reward,
                                hours = if (input$smart_interpolation_reward) {
                                  round(seq(0, 168, length.out = input$hours))
                                },
                                possible_controls = possible_controls(),
                                max_hydro = if (input$smart_interpolation_reward) {
                                  get_max_hydro(simulation_res()$area, opts)
                                },
                                mcyears = simulation_res()$mc_years,
                                area = simulation_res()$area,
                                district_balance = "water values district"
                              )

                            results <-     Grid_Matrix(
                              area = simulation_res()$area,
                              reward_db = reward_db,
                              simulation_names = simulation_res()$simulation_names,
                              simulation_values = simulation_res()$simulation_values,
                              nb_cycle = input$nb_cycle,
                              opts = opts,
                              week_53 = input$week_53,
                              district_name = "water values district" ,
                              method = input$method,
                              states_step_ratio = (1 / input$nb_states),
                              mcyears = simulation_res()$mc_years,
                              q_ratio = input$q_ratio / 100,
                              shiny = T,
                              until_convergence = input$until_convergence,
                              convergence_rate = input$convergence_rate / 100,
                              convergence_criteria = input$convergence_criteria,
                              cycle_limit = input$cycle_limit,
                              pumping = simulation_res()$pumping,
                              efficiency = simulation_res()$eff,
                              correct_concavity = input$correct_concavity,
                              correct_monotony_gain = input$correct_monotony_gain,
                              penalty_low = input$penalty_low,
                              penalty_high = input$penalty_high,
                              method_old_gain = !input$smart_interpolation_reward,
                              hours_reward_calculation = if (input$smart_interpolation_reward) {
                                round(seq(0, 168, length.out = input$hours))
                              },
                              controls_reward_calculation = if (input$smart_interpolation_reward) {
                                constraints()
                              },
                              force_final_level = input$force_final_level,
                              final_level_egal_initial = input$final_level_egal_initial,
                              final_level = input$final_level,
                              penalty_final_level = input$penalty_final_level
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
      penalty_high = reactive(input$penalty_high),
      penalty_low = reactive(input$penalty_low),
      reward_db = reactive(res$reward_db),
      area = reactive(simulation_res()$area),
      force_final_level = reactive(input$force_final_level),
      penalty_final_level = reactive(input$penalty_final_level)
    )
  })
}
