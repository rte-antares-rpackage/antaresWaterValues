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

      shiny::textOutput(
        NS(id,"pumping_eff")),

      shiny::numericInput(
        NS(id,"sim_nb_disc_stock"),
        "Number of Antares simulation",
        value = 2,
        min = 1
      ),
      shinyBS::bsTooltip(
        NS(id,"sim_nb_disc_stock"),
        " Number of simulation to launch",
        "bottom"
      ),

      shiny::sliderInput(
        NS(id,"sim_mcyears"),
        label = "Choose the number of MC years to simulate",
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
        "The path where the simulation results Rdata file will be saved. ",
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
    )
  ))# end sidebar layout

}

simulationServer <- function(id,opts,silent) {
  moduleServer(id, function(input, output, session) {

    pumping <- shiny::reactive({
      max_hydro <- get_max_hydro(area = input$sim_area,opts=opts,timeStep = "hourly")
      if (min(max_hydro$pump)>0){
        T
      } else {
        F
      }
    })

    eff <- shiny::reactive({
      if (pumping()){
        getPumpEfficiency(area = input$sim_area, opts = opts)
      } else {1}
    })

    output$dir <- shiny::renderUI({
      shiny::textInput(NS(id,"sim_output_dir"),
                       "Saving directory",
                       value = paste0(opts$studyPath, "/user"))

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
                              pumping = pumping(),
                              efficiency = eff()
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
                            pumping = pumping(),
                            fictive_area = "fictive_watervalues",
                            binding_constraint = "WeeklyWaterAmount"
                          )
                        }, print_cat = F,
                        message = F, warning = silent))
  })
}

