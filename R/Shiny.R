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

shiny_water_values <- function(opts=NULL,simulation_res=NULL,silent=F,...){


  for (p in c("bsplus","DT","grDevices","shinyBS","shinybusy","shinycustomloader",
              "shinyjs","shinythemes","spsComps","spsUtil","tools","shinyWidgets",
              "shiny")){
    if (!requireNamespace(p, quietly = TRUE)) {
      stop(
        paste0("Package ", p, " must be installed to use this function."),
        call. = FALSE
      )
    }
  }




  `%>%` <- bsplus::`%>%`
  if(is.null(opts)){ opts <- antaresRead::setSimulationPath(simulation = "input") }
  options("antares" = opts)


  #------User interface-----
  linebreaks <- function(n){shiny::HTML(strrep(shiny::br(), n))}

  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    theme = shinythemes::shinytheme("cerulean"),
    shinybusy::add_busy_gif(src = "https://github.com/dhia-gharsallaoui/watervalues/blob/main/static/calculating 3.gif?raw=true",
                            position="bottom-right",height = 70, width = 70)
    ,


    shiny::navbarPage("Water Values !",


                      #---- simulation page -----
                      shiny::tabPanel("Simulations",

                                      shiny::sidebarLayout(


                                        shiny::sidebarPanel(
                                          shiny::h2("Study parameters"),
                                          shinyWidgets::pickerInput("sim_area",
                                                                    "Choose the area",
                                                                    opts$areaList,
                                                                    options = list(
                                                                      `live-search` = TRUE)) %>%
                                            bsplus::shinyInput_label_embed(
                                              bsplus::shiny_iconlink() %>%
                                                bsplus::bs_embed_popover(title = "The area concerned by the simulation.")),

                                          shiny::textInput("solver_path","Solver path "
                                                           ,value="xxxxxxx/bin/antares-8.0-solver.exe"),

                                          shinyBS::bsTooltip("solver_path", "The path of the Antares solver you found in your Antares installation directory.",
                                                             "bottom"),

                                          shiny::h2("Simulation parameters"),

                                          shinyWidgets::materialSwitch("pumping","Activate Pumping",value=F,status = "success")%>%
                                            bsplus::shinyInput_label_embed(
                                              bsplus::shiny_iconlink() %>%
                                                bsplus::bs_embed_popover(title ="Take into account the pumping in the area.")),


                                          shiny::numericInput("sim_nb_disc_stock","Number of reservoir discretization",value=2,
                                                              min=1),
                                          shinyBS::bsTooltip("sim_nb_disc_stock", " Number of simulation to launch, a vector of energy constraint will be created from 0 to the hydro storage maximum and of length this parameter.",
                                                             "bottom"),

                                          shiny::uiOutput("choose_simulation"),

                                          shiny::sliderInput("sim_mcyears",label="choose the number of MC years to simulate",min=1,
                                                             max=opts$parameters$general$nbyears,
                                                             value=c(1,opts$parameters$general$nbyears),step=1),
                                          shinyBS::bsTooltip("sim_mcyears", " Number of Monte Carlo years to simulate.",
                                                             "bottom"),


                                          shiny::h2("Saving parameters"),

                                          shiny::uiOutput("dir"),
                                          shinyBS::bsTooltip("dir", " the path where the simulation results Rdata file will be saved. ",
                                                             "bottom"),


                                          shiny::textInput("file_name","File name",value="simulation results"),

                                          shinyBS::bsTooltip("file_name", " Name of Rdata file containing simulation results",
                                                             "bottom"),


                                          shiny::actionButton("simulate","Launch simulations"),
                                          shinyBS::bsTooltip("simulate", " launch simulations with the selected parameters. You can close the web browser after launching but keep the R server.",
                                                             "top"),

                                          shiny::actionButton("reset_sim","Reset"),
                                          shinyBS::bsTooltip("reset_sim", "Reset a Antares study in case something went wrong, please check your study before running an other simulation",
                                                             "top")


                                        ), #end sidebarPanel

                                        shiny::mainPanel(
                                          shiny::h2("Controls (u) in MWh per week evaluated for each week and for each simulation (sim)"),
                                          DT::dataTableOutput("simulation_constraint")
                                        )

                                      )# end sidebar layout

                      ), #end page



                      shiny::navbarMenu("Water values calculation",


                                        #-------calculate water values page ------
                                        shiny::tabPanel("Calculate Water Values",

                                                        shiny::sidebarLayout(

                                                          shiny::sidebarPanel(
                                                            shiny::h1("Calculate watervalues"),
                                                            shiny::h2("Study parameters"),

                                                            shiny::fileInput("ini_file", label = "Rdata file containing the simulations"),
                                                            shinyBS::bsTooltip("ini_file", " Select the Rdata file that contains the simulation results.",
                                                                               "bottom"),

                                                            #area
                                                            shinyWidgets::pickerInput("Area",
                                                                                      "Choose the area",
                                                                                      opts$areaList,
                                                                                      options = list(
                                                                                        `live-search` = TRUE))%>%
                                                              bsplus::shinyInput_label_embed(
                                                                bsplus::shiny_iconlink() %>%
                                                                  bsplus::bs_embed_popover(title = "the area which you will calculate the water values in it.")),


                                                            shinyWidgets::materialSwitch("pumping_cal","Pumping",
                                                                                         value=F,status = "success")%>%
                                                              bsplus::shinyInput_label_embed(
                                                                bsplus::shiny_iconlink() %>%
                                                                  bsplus::bs_embed_popover(title ="Take pumping into account. Use it when your simulations have aggregated pumping.")),

                                                            shiny::conditionalPanel(condition = "input.pumping_cal",
                                                                                    shiny::uiOutput("eff") ),


                                                            shinyBS::bsTooltip("eff", " The efficiency ratio of pumping you want to take in account in simulations.",
                                                                               "bottom"),
                                                            shiny::h2("Reward calculation"),
                                                            # Reward calculation
                                                            shinyWidgets::materialSwitch("smart_interpolation_reward","Use marginal prices to interpolate rewards",
                                                                                         value=F,status = "success")%>%
                                                              bsplus::shinyInput_label_embed(
                                                                bsplus::shiny_iconlink() %>%
                                                                  bsplus::bs_embed_popover(title ="If marginal prices are used, one can use less Antares simulation to retrieve reward function")),

                                                            shiny::conditionalPanel(
                                                              condition="input.smart_interpolation_reward",
                                                              shiny::sliderInput("hours","Number of hours to use to calculate rewards",max=168,min=1,value=10),
                                                              shiny::numericInput("controls","Number of controls to calculate",min=0, value=3)
                                                            ),

                                                            shiny::conditionalPanel(
                                                              condition="!input.smart_interpolation_reward",
                                                              # correct monotony option for gains
                                                              shinyWidgets::materialSwitch("correct_monotony_gain","Correct monotony of gains",
                                                                                           value=F,status = "success")%>%
                                                                bsplus::shinyInput_label_embed(
                                                                  bsplus::shiny_iconlink() %>%
                                                                    bsplus::bs_embed_popover(title ="Correct monotony of gain, ie the more water is turbined the less the cost of the electric system is high"))
                                                            ),

                                                            shiny::h2("Bellman values calculation"),
                                                            # Algorithm
                                                            shinyWidgets::radioGroupButtons(
                                                              inputId = "method",
                                                              label = "Select algorithm to use",
                                                              choices = c("grid-mean","mean-grid","quantile"),
                                                              individual = TRUE,
                                                              justified = TRUE,
                                                              checkIcon = list(
                                                                yes = shiny::icon("ok",
                                                                                  lib = "glyphicon"))
                                                            ),
                                                            shinyBS::bsTooltip("method", " Select the algorithm to use in calculation for more information check documentation.",
                                                                               "bottom"),


                                                            shiny::conditionalPanel(
                                                              condition = "input.method=='quantile'",
                                                              shiny::sliderInput("q_ratio",label=NULL,min=0,max=100,value=50,post  = " %"),
                                                            ),
                                                            shinyBS::bsTooltip("q_ratio", "The bellman values selected in each week  give q_ratio of all bellman values are equal or less to it.",
                                                                               "bottom"),

                                                            shinyWidgets::materialSwitch("until_convergence","Repeat until convergence",
                                                                                         value=F,status = "success")%>%
                                                              bsplus::shinyInput_label_embed(
                                                                bsplus::shiny_iconlink() %>%
                                                                  bsplus::bs_embed_popover(title ="Repeat the calculation using in each time the last calculation as initial condition until the convergence.")),

                                                            #number of cycles
                                                            shiny::conditionalPanel(
                                                              condition="!input.until_convergence",
                                                              shiny::numericInput("nb_cycle","Number of cycle to calculate",value=2,
                                                                                  min=1)),
                                                            shinyBS::bsTooltip("nb_cycle", " Number of times to run the algorithm to reduce the initial values effects.",
                                                                               "bottom"),


                                                            shiny::conditionalPanel(
                                                              condition="input.until_convergence",
                                                              shiny::sliderInput("convergence_rate","Convergence goal",max=100,min=0,value=90,post  = " %"),
                                                              shiny::numericInput("convergence_criteria","convergence landmark",value=1),
                                                              shiny::numericInput("cycle_limit","Number of Cycles limit ",value=10)
                                                            ),

                                                            shinyBS::bsTooltip("convergence_rate", "The convergence level from which we suppose that no need to continue another cycle.",
                                                                               "bottom"),
                                                            shinyBS::bsTooltip("convergence_criteria", "the value define convergence. if the difference between two water values is less then this value those values are converged.",
                                                                               "bottom"),
                                                            shinyBS::bsTooltip("cycle_limit", "The maximum number of cycles to calculate before convergence.",
                                                                               "bottom"),

                                                            #week 53 value
                                                            shiny::numericInput("week_53","Water value initial condition",value=0),
                                                            shinyBS::bsTooltip("week_53", " Water values for week 53, will be mutiplied by the half capacity of the reservoir to genrate an approximitive bellman values as initial condition",
                                                                               "bottom"),

                                                            #number of states:
                                                            shiny::sliderInput("nb_states",label="Choose the number of states",min=5,
                                                                               max=100,value=40,step=1),
                                                            shinyBS::bsTooltip("nb_states", " Discretization ratio to generate steps levels between the reservoir capacity and zero.",
                                                                               "bottom"),

                                                            # penalty for violation of the bottom rule curve
                                                            shiny::numericInput("penalty_low","Penalty for the violation of the bottom rule curve (euros/MWh)",value=3001),
                                                            shinyBS::bsTooltip("penalty_low", "Penalty will be added proportionally to the distance from the rule curve, it is directly comparable with the cost of unsupplied energy.",
                                                                               "bottom"),

                                                            # penalty for violation of the top rule curve
                                                            shiny::numericInput("penalty_high","Penalty for the violation of the top rule curve (euros/MWh)",value=0),
                                                            shinyBS::bsTooltip("penalty_high", "Penalty will be added proportionally to the distance from the rule curve, it is directly comparable with the cost of spilled energy.",
                                                                               "bottom"),

                                                            #MC years:
                                                            shiny::sliderInput("mcyears",label="Choose the number of MC years to use",min=1,
                                                                               max=opts$parameters$general$nbyears,
                                                                               value=c(1,opts$parameters$general$nbyears),step=1),

                                                            shinyBS::bsTooltip("mcyears", " Monte-Carlo years to consider in water values calculation.",
                                                                               "bottom"),


                                                            # correct concavity option for Bellman values
                                                            shinyWidgets::materialSwitch("correct_concavity","Correct concavity of Bellman values",
                                                                                         value=F,status = "success")%>%
                                                              bsplus::shinyInput_label_embed(
                                                                bsplus::shiny_iconlink() %>%
                                                                  bsplus::bs_embed_popover(title ="Correct concavity of Bellman values to have monotone water values.")),


                                                            shiny::actionButton("Calculate","Launch calculation", icon = shiny::icon("check-circle"),
                                                                                align = "center"),
                                                            shinyBS::bsTooltip("Calculate", "Click to start the calculation of te water values using the selected parameters",
                                                                               "bottom"),
                                                            #
                                                            #           shinyWidgets::materialSwitch("show_negative","Show negative Water values",
                                                            #                         value=T,status = "danger")%>%
                                                            #             bsplus::shinyInput_label_embed(
                                                            #               bsplus::shiny_iconlink() %>%
                                                            #                 bs_embed_popover(title ="Applicate a filter to remove the negative values in the graph.
                                                            # NB: this is only a display filter the values are unchanged.")),

                                                            shiny::h2("Plot"),

                                                            shinyWidgets::materialSwitch("filter","Filter water values",value=F,status = "success")%>%
                                                              bsplus::shinyInput_label_embed(
                                                                bsplus::shiny_iconlink() %>%
                                                                  bsplus::bs_embed_popover(title ="Visualize only watervalues inside rule curves")),


                                                            shiny::actionButton("plot","Show"),
                                                            shinyBS::bsTooltip("plot", " Show the Graph",
                                                                               "bottom"),
                                                          ),

                                                          shiny::mainPanel(
                                                            shinycustomloader::withLoader( shiny::plotOutput("Watervalues"), type="html", loader="dnaspin"),
                                                            shinyWidgets::downloadBttn(
                                                              outputId = "download_wv_plot",
                                                              style = "unite",
                                                              color = "primary",
                                                              block = T
                                                            ),
                                                            shiny::conditionalPanel(
                                                              condition="input.smart_interpolation_reward",
                                                              DT::dataTableOutput("calculated_controls")
                                                            )

                                                          )
                                                        )
                                        ), #tabpanel 1

                                        #--------- Bellman graphs UI -----
                                        shiny::tabPanel("Bellman plot",
                                                        shiny::sidebarLayout(
                                                          shiny::sidebarPanel(
                                                            shiny::h1("Bellman plot"),
                                                            shiny::sliderInput("week_id","Week to show",value=c(2,2),
                                                                               min=1,max = 52),

                                                            shinyBS::bsTooltip("week_id", "The number of the week you want to plot (Bellman values are plotted for the end of the week and watervalues for the beginning of the week)",
                                                                               "bottom"),


                                                          ), #siderbarpanel
                                                          shiny::mainPanel(

                                                            shiny::plotOutput("plot_Bellman"),
                                                            shinyWidgets::downloadBttn(
                                                              outputId = "download_Bellman_plot",
                                                              style = "unite",
                                                              color = "primary",
                                                              block = T
                                                            ),

                                                          )
                                                        ), # Siderbar
                                        ),#tabpanel 2

                                        #------ Reward Plot ------------
                                        shiny::tabPanel("Rewards Plot",

                                                        shiny::sidebarLayout(


                                                          shiny::sidebarPanel(
                                                            shiny::h1("Rewards plot"),

                                                            shiny::actionButton("import_reward","Import reward"),

                                                            shiny::sliderInput("week_id_rew","Week to show",value=c(2,2),
                                                                               min=1,max = 52),

                                                            shinyBS::bsTooltip("week_id_rew", "The weeks you want to plot",
                                                                               "bottom"),

                                                            shiny::selectInput("param_rew","Type",c("Reward"="r",
                                                                                                    "Reward 1 MC"="r1",
                                                                                                    "Reward variation"="rv",
                                                                                                    "reward variation 1Mc"="rv1")),

                                                            shiny::conditionalPanel(
                                                              condition = "['rv1','r1'].includes(input.param_rew)",
                                                              shiny::sliderInput("Mc_year",label="Monte-Carlo year",min=1,
                                                                                 max=opts$parameters$general$nbyears,value=c(1,1)
                                                                                 ,step = 1)
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


                                        ),#end tabpanel 3


                                        #----------Post process -----
                                        shiny::tabPanel("Post Process",

                                                        shiny::sidebarLayout(fluid = TRUE,

                                                                             shiny::sidebarPanel(
                                                                               shiny::h1("Post process"),
                                                                               shiny::h3("Remove extreme values"),
                                                                               shinyWidgets::switchInput("Run_remove_out",
                                                                                                         value=F, offStatus = "danger",
                                                                                                         onStatus = "success")%>%
                                                                                 bsplus::shinyInput_label_embed(
                                                                                   bsplus::shiny_iconlink() %>%
                                                                                     bsplus::bs_embed_popover(title ="Activate filter to remove extreme values")),





                                                                               shiny::conditionalPanel(

                                                                                 condition = "input.Run_remove_out",
                                                                                 shiny::numericInput(inputId = "min_rm",
                                                                                                     label = "Minimum value to keep",
                                                                                                     value = 0,
                                                                                                     step = 1,
                                                                                                     width = '100%'),
                                                                                 shiny::numericInput(inputId = "max_rm",
                                                                                                     label = "Maximum value to keep",
                                                                                                     value = 200,
                                                                                                     step = 1,
                                                                                                     width = '100%'),

                                                                                 shiny::h3("Replace not defined values"),

                                                                                 shinyWidgets::radioGroupButtons(
                                                                                   inputId = "replace_na_method",
                                                                                   label = "Method to replace NaN values",
                                                                                   choices = c("Constant values","Extreme values"),
                                                                                   individual = TRUE,
                                                                                   justified = TRUE
                                                                                 ),

                                                                                 shiny::conditionalPanel(
                                                                                   condition = "input.replace_na_method=='Constant values'",
                                                                                   shiny::numericInput("max_vu","Max Water value",value=200),
                                                                                   shinyBS::bsTooltip("max_vu", "The highest water value that you want to affect",
                                                                                                      "bottom"),
                                                                                   shiny::numericInput("min_vu","Min Water value",value=0),
                                                                                   shinyBS::bsTooltip("min_vu", "The smallest water value that you want to affect",
                                                                                                      "bottom"),
                                                                                 ),
                                                                               ),
                                                                               shinyBS::bsTooltip("min_rm", "Delete all water values that are under this value",
                                                                                                  "bottom"),
                                                                               shinyBS::bsTooltip("max_rm", "Delete all water values that are bigger then this value",
                                                                                                  "bottom"),





                                                                               shiny::tags$button(
                                                                                 id = "to_antares",
                                                                                 type="button",
                                                                                 class = "btn action-button btn-large btn-primary",
                                                                                 shiny::img(src = "https://antares-simulator.org/static/img/antares-256.png",
                                                                                            height = "50px",
                                                                                            shiny::HTML('<i class="icon-star"></i>To Antares'))
                                                                               ),
                                                                               shinyBS::bsTooltip("to_antares", "convert the water values to antares format than implemented in the desired area",
                                                                                                  "bottom")

                                                                             ),#sidebarPanel




                                                                             shiny::mainPanel(
                                                                               shinycustomloader::withLoader(shiny::plotOutput("post_process"), type="html", loader="dnaspin"),
                                                                               shinyWidgets::downloadBttn(
                                                                                 outputId = "download_pp_plot",
                                                                                 style = "unite",
                                                                                 color = "primary",
                                                                                 block = T
                                                                               ),

                                                                             )
                                                        ) #sidebarLayout

                                        ) #end tabpanel "Post Process"

                      ), #end navbarMenu

                      # iterative ---------------------------------------------------------------
                      shiny::tabPanel(
                        "Iterative calculation of Bellman values",
                        iterativeUI("iterative",opts)
                      ),

    ) #navbar

  ) #UI

  #------Server functions ------
  server <- function(input, output, session) {

    global <- shiny::reactiveValues(datapath = getwd())
    rv <- shiny::reactiveValues()

    output$dir <- shiny::renderUI({
      shiny::textInput("sim_output_dir","Saving directory",value=paste0(opts$studyPath,"/user"))

    })

    simulation_constraint <- shiny::reactive({
      constraint_generator(area=input$sim_area,
                           opts=opts,
                           pumping=input$pumping,
                           nb_disc_stock = input$sim_nb_disc_stock)
    })

    output$simulation_constraint <- DT::renderDataTable({
      dplyr::filter(simulation_constraint(),.data$sim %in% input$subset_simulation)
    })

    output$choose_simulation <- shiny::renderUI({
      shiny::checkboxGroupInput("subset_simulation","Choose which simulations you want to run",
                                unique(simulation_constraint()$sim),
                                selected = unique(simulation_constraint()$sim))
    })

    output$eff <- shiny::renderUI({ shiny::numericInput("efficiency","Efficiency pumping ratio",min=0,max=1,
                                                        value=getPumpEfficiency(area=input$Area,opts = opts))
    })



    shiny::observeEvent(input$simulate,

                        spsUtil::quiet({

                          spsComps::shinyCatch({
                            simulation_res <-    runWaterValuesSimulation(
                              area=input$sim_area,
                              simulation_name = paste0(input$sim_area, "_wv_sim_%s"),
                              nb_disc_stock = input$sim_nb_disc_stock,
                              nb_mcyears = seq(from = input$sim_mcyears[1], to = input$sim_mcyears[2]),
                              path_solver =input$solver_path,
                              binding_constraint = "WeeklyWaterAmount_",
                              fictive_area = "fictive_watervalues",
                              thermal_cluster = "WaterValueCluster",
                              overwrite = T,
                              link_from=input$sim_area,
                              opts = opts,
                              shiny=T,
                              otp_dest=input$sim_output_dir,
                              file_name=input$file_name,
                              pumping = input$pumping,
                              constraint_values = dplyr::filter(simulation_constraint(),
                                                                .data$sim %in% input$subset_simulation))},
                            prefix = "")
                          shinyWidgets::show_alert(
                            title = "Run simulations",
                            text = "Done !!",
                            type = "success"
                          )},print_cat = F,
                          message = F, warning = silent))

    shiny::observeEvent(input$reset_sim,

                        spsUtil::quiet({
                          opts_temp <- antaresRead::setSimulationPath(opts$studyPath, "input")
                          resetStudy(opts=opts_temp,
                                     area=input$sim_area,
                                     pumping = input$pumping,
                                     fictive_area="fictive_watervalues")
                        },print_cat = F,
                        message = F, warning = silent))





    simulation_res <- shiny::reactive({


      if ( is.null(input$ini_file)) {
        simulation_res
      }else{
        inFile <- input$ini_file
        file <- inFile$datapath
        # load the file into new environment and get it from there
        e = new.env()
        name <- load(file, envir = e)
        simulation_res <- e[[name]]
        simulation_res
      }
    })


    reward_db <- shiny::reactive({


      if ( is.null(input$reward_file)) {
        reward_db <- NULL
        print("Failed to import Reward")
        reward_db
      }else{
        inFile <- input$reward_file
        file <- inFile$datapath
        # load the file into new environment and get it from there
        e = new.env()
        name <- load(file, envir = e)
        reward_db <- e[[name]]
        print("Reward Imported")
        rv$reward_dt <- reward_db
        reward_db
      }
    })

    inflow <- shiny::reactive({


      if ( is.null(input$inflow_file)) {
        inflow <- NULL
        inflow
      }else{
        inFile <- input$inflow_file
        file <- inFile$datapath
        # load the file into new environment and get it from there
        e = new.env()
        name <- load(file, envir = e)
        inflow <- e[[name]]
        inflow
      }
    })

    calculated_controls <- shiny::reactive({
      rbind(dplyr::mutate(simulation_res()$simulation_values,From="Simulation"),
            dplyr::mutate(constraint_generator(area=input$Area,
                                               opts=opts,
                                               pumping=input$pumping_cal,
                                               nb_disc_stock = input$controls,
                                               pumping_efficiency = input$efficiency),
                          From="Calculated controls")) %>%
        dplyr::select(-c("sim")) %>%
        dplyr::group_by(.data$week,.data$From) %>%
        dplyr::summarise(Controls=list(.data$u),.groups="drop") %>%
        tidyr::pivot_wider(names_from = .data$From,values_from = .data$Controls) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(Union=list(union(.data$`Calculated controls`,.data$Simulation))) %>%
        tidyr::pivot_longer(cols=2:4,names_to = "From",values_to = "Controls") %>%
        dplyr::mutate(Total=lengths(.data$Controls)) %>%
        dplyr::mutate(Controls=as.character(.data$Controls)) %>%
        as.data.table()
    })

    output$calculated_controls <- DT::renderDataTable({
      calculated_controls()
    })

    shiny::observeEvent( input$Calculate,

                         spsUtil::quiet({

                           spsComps::shinyCatch({
                             results <-     Grid_Matrix(
                               area = input$Area,
                               simulation_names = simulation_res()$simulation_names,
                               simulation_values = simulation_res()$simulation_values,
                               reward_db = reward_db(),
                               inflow = inflow(),
                               nb_cycle = input$nb_cycle,
                               opts = opts,
                               week_53 = input$week_53,
                               district_name ="water values district" ,
                               method=input$method,
                               states_step_ratio=(1/input$nb_states),
                               mcyears=input$mcyears[1]:input$mcyears[2],
                               reservoir_capacity=NULL,
                               q_ratio=input$q_ratio/100,
                               shiny=T,
                               until_convergence = input$until_convergence,
                               convergence_rate = input$convergence_rate/100,
                               convergence_criteria = input$convergence_criteria,
                               cycle_limit = input$cycle_limit,
                               pumping = input$pumping_cal,
                               efficiency = input$efficiency,
                               correct_concavity = input$correct_concavity,
                               correct_monotony_gain = input$correct_monotony_gain,
                               penalty_low = input$penalty_low,
                               penalty_high = input$penalty_high,
                               method_old_gain = !input$smart_interpolation_reward,
                               hours_reward_calculation = if(input$smart_interpolation_reward){round(seq(0,168,length.out=input$hours))},
                               controls_reward_calculation = if(input$smart_interpolation_reward){constraint_generator(area=input$Area,
                                                                                                                       opts=opts,
                                                                                                                       pumping=input$pumping_cal,
                                                                                                                       nb_disc_stock = input$controls,
                                                                                                                       pumping_efficiency = input$efficiency)}
                             )$aggregated_results

                             shiny::isolate(rv$results <- results)
                             shinyWidgets::show_alert(
                               title = "Water Values",
                               text = "Calculation Done !!",
                               type = "success"
                             )

                           },blocking_level="error",position = "top-center", shiny = TRUE,prefix = "" )

                         },print_cat = F, message = F, warning = silent)


    )





    watervalues <- shiny::eventReactive(input$plot,
                                        {waterValuesViz(rv$results,input$filter)})

    output$Watervalues <- shiny::renderPlot(watervalues())

    shinyBS::addPopover(session,"Watervalues",title = "water values",content = "This graph describe the water values for each week starting from week 1 to week 52 in the X-axis and the level of the reservoir in perecent in the Y-axis. the water values are determined by the colors you can see them in the legend of the graph. ")

    output$download_wv_plot <- shiny::downloadHandler(
      filename = function() {
        paste('watervalues-', Sys.Date(), '.png', sep='')
      },
      content = function(con) {
        grDevices::png(con ,width = 1200,
                       height = 766)
        print(watervalues())
        grDevices::dev.off()
      }
    )

    #--------Plot Bellman page----

    output$plot_Bellman <- shiny::renderPlot(plot_Bellman(rv$results,input$week_id[1]:input$week_id[2],
                                                          input$penalty_low,input$penalty_high))

    output$download_Bellman_plot <- shiny::downloadHandler(
      filename = function() {
        paste('Bellman-', Sys.Date(), '.png', sep='')
      },
      content = function(con) {

        grDevices::png(con ,width = 1200,
                       height = 766)
        print(plot_Bellman(rv$results,input$week_id[1]:input$week_id[2],
                           input$penalty_low,input$penalty_high))
        grDevices::dev.off()
      }
    )

    #--------plot reward page------

    possible_controls <- shiny::reactive({
      possible_controls <- if(input$smart_interpolation_reward){
        rbind(simulation_res()$simulation_values,constraint_generator(area=input$Area,
                                                                      opts=opts,
                                                                      pumping=input$pumping_cal,
                                                                      nb_disc_stock = input$controls,
                                                                      pumping_efficiency = input$efficiency)) %>%
          dplyr::select("week","u") %>%
          dplyr::distinct() %>%
          dplyr::arrange(week,.data$u)} else {
            simulation_res()$simulation_values %>% dplyr::select("week","u")}
      possible_controls
    })

    shiny::observeEvent( input$import_reward,
                         {
                           spsComps::shinyCatch({
                             reward_dt <- get_Reward(simulation_names = simulation_res()$simulation_names,
                                                     simulation_values = simulation_res()$simulation_values,
                                                     district_name ="water values district",
                                                     opts=opts,
                                                     method_old = !input$smart_interpolation_reward,
                                                     hours = if(input$smart_interpolation_reward){round(seq(0,168,length.out=input$hours))},
                                                     possible_controls = possible_controls(),
                                                     max_hydro=if(input$smart_interpolation_reward){get_max_hydro(input$Area,opts)},
                                                     mcyears=input$mcyears[1]:input$mcyears[2],
                                                     area=input$Area,
                                                     district_balance="water values district")
                             rv$reward_dt <- reward_dt
                             shinybusy::remove_modal_spinner()
                             shinyWidgets::show_alert(
                               title = "Rewards",
                               text = "Importation Done !!",
                               type = "success"
                             )
                           })
                         }
    )


    rewardplot <- shiny::reactive(

      {
        if(is.null(rv$reward))rv$reward <- reward_db()

        week_id_rew <- input$week_id_rew[1]:input$week_id_rew[2]
        Mc_year <- input$Mc_year[1]:input$Mc_year[2]
        if(input$param_rew=="r")
        {plot_reward(rv$reward_dt$reward,week_id_rew)
        }else{
          if(input$param_rew=="rv")
          {plot_reward_variation(rv$reward_dt$reward,week_id_rew)
          }else{

            if(input$param_rew=="r1")
            {plot_reward_mc(rv$reward_dt$reward,week_id_rew,
                            Mc_year)
            }else{

              if(input$param_rew=="rv1")
              {plot_reward_variation_mc(rv$reward_dt$reward,week_id_rew,
                                        Mc_year)}
            }
          }
        }
      }
    )

    reward_var_plot <- shiny::reactive({

      week_id_rew <- input$week_id_rew[1]:input$week_id_rew[2]
      Mc_year <- input$Mc_year[1]:input$Mc_year[2]

      if(input$param_rew=="r1")
      {plot_reward_variation_mc(rv$reward_dt$reward,week_id_rew,
                                Mc_year)
      }else{

        plot_reward_variation(rv$reward_dt$reward,week_id_rew)
      }


    })




    output$rewardplot <- shiny::renderPlot(rewardplot()$graph)
    # output$reward_second_plot <- shiny::renderPlot(reward_var_plot()$graph)
    output$reward_table <- DT::renderDataTable(rewardplot()$table)
    output$download_reward_plot <- shiny::downloadHandler(
      filename = function() {
        paste('Reward-', Sys.Date(), '.png', sep='')
      },
      content = function(con) {
        grDevices::png(con ,width = 1200,height = 766)
        print(rewardplot()$graph)
        grDevices::dev.off()
      }
    )


    shiny::observe({
      if(!is.null(rv$reward_dt))
        shiny::isolate(
          reward_base <- rv$reward_dt
        )
    })

    output$download_reward_base <- shiny::downloadHandler(
      filename <- function(){
        paste('Reward-Base-', Sys.Date(), '.Rdata', sep='') },

      content = function(file) {
        save(reward_base, file = file)
      }
    )
    # end reward Plot


    #--------post process----------
    final_result <- shiny::reactive({

      if(input$Run_remove_out){
        remove_out(rv$results,min = input$min_rm,max=input$max_rm,
                   replace_na_method=input$replace_na_method,max_vu=input$max_vu,
                   min_vu=input$min_vu)
      }else{
        rv$results
      }
    })



    output$post_process <- shiny::renderPlot(
      waterValuesViz(final_result())
    )

    output$download_pp_plot <- shiny::downloadHandler(
      filename = function() {
        paste('full water values-', Sys.Date(), '.png', sep='')
      },
      content = function(con) {
        grDevices::png(con ,width = 1200,
                       height = 766)
        print(waterValuesViz(final_result()))
        grDevices::dev.off()
      }
    )

    shiny::observeEvent(input$to_antares,{

      results <- final_result()
      results <- results[results$weeks!=53,]

      reshaped_values <- to_Antares_Format(results,
                                           input$penalty_low,
                                           input$penalty_high)
      antaresEditObject::writeWaterValues(
        area = input$Area,
        data = reshaped_values
      )
      shinyWidgets::show_alert(
        title = "Implement water values in Antares",
        text = " Done !!",
        type = "success"
      )
    }

    )

    # iterative calculation ---------------------------------------------------
    iterativeServer("iterative",opts, silent)


  }


  #------Run-----
  options(shiny.launch.browser=TRUE)
  options(shiny.sanitize.errors = FALSE)
  options(shiny.fullstacktrace = FALSE)
  options(shiny.trace = F)
  options(shiny.error = NULL)
  shiny::shinyApp(ui = ui, server = server)
}

