simulationUI <- function(id,opts) {
  shiny::tagList(shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::h2("Study parameters"),
      shinyWidgets::pickerInput(
        NS(id,"sim_area"),
        "Choose the area",
        opts$areaList,
        options = list(`live-search` = TRUE)
      ) %>%
        bsplus::shinyInput_label_embed(
          bsplus::shiny_iconlink() %>%
            bsplus::bs_embed_popover(title = "The area concerned by the simulation.")
        ),

      shiny::textInput(NS(id,"solver_path"), "Solver path "
                       , value = "xxxxxxx/bin/antares-8.0-solver.exe"),

      shinyBS::bsTooltip(
        NS(id,"solver_path"),
        "The path of the Antares solver you found in your Antares installation directory.",
        "bottom"
      ),

      shiny::h2("Simulation parameters"),

      shinyWidgets::materialSwitch(
        NS(id,"pumping"),
        "Activate Pumping",
        value = F,
        status = "success"
      ) %>%
        bsplus::shinyInput_label_embed(
          bsplus::shiny_iconlink() %>%
            bsplus::bs_embed_popover(title = "Take into account the pumping in the area.")
        ),

      shiny::textOutput(
        NS(id,"pumping_eff")),

      shiny::numericInput(
        NS(id,"sim_nb_disc_stock"),
        "Number of reservoir discretization",
        value = 2,
        min = 1
      ),
      shinyBS::bsTooltip(
        NS(id,"sim_nb_disc_stock"),
        " Number of simulation to launch, a vector of energy constraint will be created from 0 to the hydro storage maximum and of length this parameter.",
        "bottom"
      ),

      shiny::uiOutput(NS(id,"choose_simulation")),

      shiny::sliderInput(
        NS(id,"sim_mcyears"),
        label = "choose the number of MC years to simulate",
        min = 1,
        max = opts$parameters$general$nbyears,
        value = c(1, opts$parameters$general$nbyears),
        step = 1
      ),
      shinyBS::bsTooltip(
        NS(id,"sim_mcyears"),
        " Number of Monte Carlo years to simulate.",
        "bottom"
      ),


      shiny::h2("Saving parameters"),

      shiny::uiOutput(NS(id,"dir")),
      shinyBS::bsTooltip(
        NS(id,"dir"),
        " the path where the simulation results Rdata file will be saved. ",
        "bottom"
      ),


      shiny::textInput(NS(id,"file_name"), "File name", value =
                         "simulation results"),

      shinyBS::bsTooltip(
        NS(id,"file_name"),
        " Name of Rdata file containing simulation results",
        "bottom"
      ),


      shiny::actionButton(NS(id,"simulate"), "Launch simulations"),
      shinyBS::bsTooltip(
        "simulate",
        " launch simulations with the selected parameters. You can close the web browser after launching but keep the R server.",
        "top"
      ),

      shiny::actionButton(NS(id,"reset_sim"), "Reset"),
      shinyBS::bsTooltip(
        NS(id,"reset_sim"),
        "Reset a Antares study in case something went wrong, please check your study before running an other simulation",
        "top"
      )


    ),
    #end sidebarPanel

    shiny::mainPanel(
      shiny::h2(
        "Controls (u) in MWh per week evaluated for each week and for each simulation (sim)"
      ),
      DT::dataTableOutput(NS(id,"simulation_constraint"))
    )
  ))# end sidebar layout

}

simulationServer <- function(id,opts,silent) {
  moduleServer(id, function(input, output, session) {

    eff <- shiny::reactive({
      if (input$pumping){
        if (!is.null(input$sim_area))getPumpEfficiency(area = input$sim_area, opts = opts)
        else 1
      } else {1}

    })

    output$pumping_eff <- shiny::renderText({
      if(input$pumping) {
        paste0("Efficiency : ", eff())}
    })

    output$dir <- shiny::renderUI({
      shiny::textInput(NS(id,"sim_output_dir"),
                       "Saving directory",
                       value = paste0(opts$studyPath, "/user"))

    })

    simulation_constraint <- shiny::reactive({
      constraint_generator(
        area = input$sim_area,
        opts = opts,
        pumping = input$pumping,
        nb_disc_stock = input$sim_nb_disc_stock,
        pumping_efficiency = eff(),
        mcyears = seq(
          from = input$sim_mcyears[1],
          to = input$sim_mcyears[2]
        )
      )
    })

    output$simulation_constraint <- DT::renderDataTable({
      dplyr::filter(simulation_constraint(),
                    .data$sim %in% input$subset_simulation)
    })

    output$choose_simulation <- shiny::renderUI({
      shiny::checkboxGroupInput(
        NS(id,"subset_simulation"),
        "Choose which simulations you want to run",
        unique(simulation_constraint()$sim),
        selected = unique(simulation_constraint()$sim)
      )
    })

    shiny::observeEvent(input$simulate,

                        spsUtil::quiet({
                          spsComps::shinyCatch({
                            simulation_res <-    runWaterValuesSimulation(
                              area = input$sim_area,
                              simulation_name = paste0(input$sim_area, "_wv_sim_%s"),
                              nb_disc_stock = input$sim_nb_disc_stock,
                              nb_mcyears = seq(
                                from = input$sim_mcyears[1],
                                to = input$sim_mcyears[2]
                              ),
                              path_solver = input$solver_path,
                              binding_constraint = "WeeklyWaterAmount",
                              fictive_area = "fictive_watervalues",
                              thermal_cluster = "WaterValueCluster",
                              overwrite = T,
                              link_from = input$sim_area,
                              opts = opts,
                              shiny = T,
                              otp_dest = input$sim_output_dir,
                              file_name = input$file_name,
                              pumping = input$pumping,
                              efficiency = eff(),
                              constraint_values = dplyr::filter(
                                simulation_constraint(),
                                .data$sim %in% input$subset_simulation
                              )
                            )
                          },
                          prefix = "")
                          shinyWidgets::show_alert(title = "Run simulations",
                                                   text = "Done !!",
                                                   type = "success")
                        }, print_cat = F,
                        message = F, warning = silent))

    shiny::observeEvent(input$reset_sim,

                        spsUtil::quiet({
                          opts_temp <- antaresRead::setSimulationPath(opts$studyPath, "input")
                          resetStudy(
                            opts = opts_temp,
                            area = input$sim_area,
                            pumping = input$pumping,
                            fictive_area = "fictive_watervalues",
                            binding_constraint = "WeeklyWaterAmount"
                          )
                        }, print_cat = F,
                        message = F, warning = silent))
  })
}

