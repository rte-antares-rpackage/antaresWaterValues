#' Open watervalues Calculator in APP Web
#'
#' @param simulation_res
#'   List of simulation results returned by the function
#'   \code{watervalues::runWaterValuesSimulation}
#' @param study_path the path of the Antares study
#' @param silent Boolean. TRUE to suppress warnings.
#' @param ... further arguments passed to or from other methods.
#' @rawNamespace import(shiny, except =c(dataTableOutput,renderDataTable))
#' @import shinyWidgets
#' @importFrom  shinythemes shinytheme
#' @importFrom shinycustomloader withLoader
#' @importFrom shinybusy add_busy_gif remove_modal_spinner
#' @importFrom  spsComps shinyCatch
#' @importFrom  shinyjs useShinyjs
#' @importFrom antaresEditObject writeWaterValues
#' @importFrom data.table copy
#' @importFrom grDevices dev.off png rgb
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom antaresRead setSimulationPath
#' @importFrom shinyBS bsTooltip addPopover
#' @importFrom bsplus bs_embed_popover shiny_iconlink shinyInput_label_embed `%>%`
#' @importFrom spsUtil quiet
#' @export

shiny_water_values <- function(simulation_res=NULL,study_path,silent=F,...)

{


`%>%` <- bsplus::`%>%`
opts <- antaresRead::setSimulationPath(simulation = "input")
options("antares" = opts)

otp_variables <- c("Real OV. COST","stockDiff","hydro_price",
                   "hydro_stockDiff_cost","hydro_cost","total_hydro_cost"
                   ,"OV. COST", "OP. COST","MRG. PRICE", "CO2 EMIS.", "BALANCE",
    "ROW BAL.", "PSP", "MISC. NDG", "LOAD", "H. ROR","WIND", "SOLAR", "NUCLEAR",
    "LIGNITE","COAL",  "GAS", "OIL","MIX. FUEL","MISC. DTG","H. STOR",
    "H. PUMP","H. LEV", "H. INFL", "H. OVFL","H. VAL", "H. COST","UNSP. ENRG",
    "SPIL. ENRG", "LOLD","LOLP", "AVL DTG", "DTG MRG","MAX MRG", "NP COST",
    "NODU")


#------User interface-----
linebreaks <- function(n){HTML(strrep(br(), n))}

ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = shinythemes::shinytheme("cerulean"),
  shinybusy::add_busy_gif(src = "https://github.com/dhia-gharsallaoui/watervalues/blob/main/static/calculating 3.gif?raw=true",
                          position="bottom-right",height = 70, width = 70)
  ,


  navbarPage("Water Values !",


  #---- simulation page -----
    tabPanel("Simulations",

          sidebarLayout(


           sidebarPanel(
             pickerInput("sim_area",
                         "Choose the area",
                         opts$areaList,
                         options = list(
                           `live-search` = TRUE)) %>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(title = "The area concerned by the simulation.")),

             pickerInput("remove_areas",
                         "choose the areas to eliminate from result calculation",
                         opts$areaList,
                         options = list(
                           `live-search` = TRUE),
                         multiple = TRUE)%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(title = "area(s) to remove from the created district.")),

             textInput("solver_path","Solver path "
                       ,value="xxxxxxx/bin/antares-8.0-solver.exe"),

             shinyBS::bsTooltip("solver_path", "The path of the Antares solver you found in your Antares installation directory.",
                                "bottom"),


             textInput("sim_simulation_name","Simulation name "
                       ,value="weekly_water_amount_%s"),

             shinyBS::bsTooltip("sim_simulation_name", " The name of the simulation, add %s in the end to add constraints values to the names.",
                                "bottom"),

             materialSwitch("pumping","Activate Pumping",value=F,status = "success")%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(title ="Take into account the pumping in the area.")),


             numericInput("sim_nb_disc_stock","Number of reservoir discretization",value=2,
                          min=1),
             shinyBS::bsTooltip("sim_nb_disc_stock", " Number of simulation to launch, a vector of energy constraint will be created from 0 to the hydro storage maximum and of length this parameter.",
                                "bottom"),

             sliderInput("sim_mcyears",label="choose the number of MC years to simulate",min=1,
                         max=opts$parameters$general$nbyears,
                         value=c(1,opts$parameters$general$nbyears),step=1),
             shinyBS::bsTooltip("sim_mcyears", " Number of Monte Carlo years to simulate.",
                                "bottom"),

             pickerInput("link_from",
                         "Choose the area to link with the fictive area",
                         opts$areaList,
                         options = list(
                           `live-search` = TRUE)) %>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(title = "The area that will be linked with the created fictive area.")),


             textInput("sim_binding_constraint","Name of the binding constraint "
                       ,value="WeeklyWaterAmount"),
             shinyBS::bsTooltip("sim_binding_constraint", " Name of the binding constraint of energy on the link between the area and the fictive area.",
                                "bottom"),

             textInput("sim_fictive_area","Name of the fictive area to create "
                       ,value="fictive_watervalues"),
             shinyBS::bsTooltip("sim_fictive_area", " Name of the fictive area to create.",
                                "bottom"),

             textInput("sim_thermal_cluster","Name of the thermal cluster to create.",value = "WaterValueCluster"),

             shinyBS::bsTooltip("sim_thermal_cluster", " Name of thermal cluster to create which will generate the free power in the fictive area.",
                                "bottom"),


             uiOutput("dir"),
             shinyBS::bsTooltip("dir", " the path where the simulation results Rdata file will be saved. ",
                                "bottom"),


             textInput("file_name","File name",value="simulation results"),

             shinyBS::bsTooltip("file_name", " Name of Rdata file containing simulation results",
                                "bottom"),


             actionButton("simulate","Launch simulations"),
             shinyBS::bsTooltip("simulate", " launch simulations with the selected parameters. You can close the web browser after launching but keep the R server.",
                                "bottom")


             ), #end sidebarPanel

           mainPanel()

           )# end sidebar layout

          ), #end page



   navbarMenu("Water values calculation",


    #-------calculate water values page ------
    tabPanel("Calculate Water Values",

      sidebarLayout(

        sidebarPanel(
          materialSwitch("wv_input","Auto calculated",
                         value=F,status = "success")%>%
            shinyInput_label_embed(
              shiny_iconlink() %>%
                bs_embed_popover(title ="If you have your simulations output use the auto mode to recalculate the inflows and rewards. If you have those data in R files use custom mode.")),


          fileInput("ini_file", label = "Rdata file containing the simulations"),
          shinyBS::bsTooltip("ini_file", " Select the Rdata file that contains the simulation results.",
                             "bottom"),
          conditionalPanel(condition="input.wv_input",
          fileInput("reward_file", label = "Rdata file containing the Reward Matrix"),
          shinyBS::bsTooltip("reward_file", " Select the Rdata file that contains the reward matrix.",
                             "bottom"),
          fileInput("inflow_file", label = "Rdata file containing the weekly inflow "),
          shinyBS::bsTooltip("inflow_file", " Select the Rdata file that contains the weekly inflow generated by ReadAntares",
                             "bottom"),
          ),
          #area
          pickerInput("Area",
            "choose the area",
            opts$areaList,
            options = list(
              `live-search` = TRUE))%>%
            shinyInput_label_embed(
              shiny_iconlink() %>%
                bs_embed_popover(title = "the area which you will calculate the water values in it.")),


          #District List
          pickerInput("district_name",
                      "choose the District",
                      opts$districtList,
                      options = list(
                        `live-search` = TRUE))%>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_embed_popover(title = "the district that will be used in calculation of the rewards of transitions.")),



          # Algorithm
          radioGroupButtons(
            inputId = "method",
            label = "Select algorithm to use",
            choices = c("grid-mean","mean-grid","quantile"),
            individual = TRUE,
            justified = TRUE,
            checkIcon = list(
              yes = icon("ok",
                         lib = "glyphicon"))
          ),
          shinyBS::bsTooltip("method", " Select the algorithm to use in calculation for more information check documentation.",
                             "bottom"),


          conditionalPanel(
            condition = "input.method=='quantile'",
            sliderInput("q_ratio",label=NULL,min=0,max=100,value=50,post  = " %"),
          ),
          shinyBS::bsTooltip("q_ratio", " the bellman values selected in each week  give q_ratio of all bellman values are equal or less to it.",
                             "bottom"),

          materialSwitch("until_convergence","Repeat until convergence",
                         value=F,status = "success")%>%
            shinyInput_label_embed(
              shiny_iconlink() %>%
                bs_embed_popover(title ="Repeat the calculation using in each time the last calculation as initial condition until the convergence.")),

          #number of cycles
          conditionalPanel(
          condition="!input.until_convergence",
          numericInput("nb_cycle","number of cycle to calculate",value=2,
                       min=1)),
          shinyBS::bsTooltip("nb_cycle", " Number of times to run the algorithm to reduce the initial values effects.",
                             "bottom"),


          conditionalPanel(
            condition="input.until_convergence",
            sliderInput("convergence_rate","Convergence goal",max=100,min=0,value=90,post  = " %"),
            numericInput("convergence_criteria","convergence landmark",value=1),
            numericInput("cycle_limit","Number of Cycles limit ",value=10)
            ),

          shinyBS::bsTooltip("convergence_rate", "The convergence level from which we suppose that no need to continue another cycle.",
                             "bottom"),
          shinyBS::bsTooltip("convergence_criteria", "the value define convergence. if the difference between two water values is less then this value those values are converged.",
                             "bottom"),
          shinyBS::bsTooltip("cycle_limit", "The maximum number of cycles to calculate before convergence.",
                             "bottom"),

          #week 53 value
          numericInput("week_53","water value initial condition",value=0),
          shinyBS::bsTooltip("week_53", " Water values for week 53, will be mutiplied by the half capacity of the reservoir to genrate an approximitive bellman values as initial condition",
                             "bottom"),

          #number of states:
          sliderInput("nb_states",label="choose the number of states",min=5,
                      max=100,value=40,step=1),
          shinyBS::bsTooltip("nb_states", " Discretization ratio to generate steps levels between the reservoir capacity and zero.",
                             "bottom"),


          #MC years:
          sliderInput("mcyears",label="choose the number of MC years to use",min=1,
                      max=opts$parameters$general$nbyears,
                      value=c(1,opts$parameters$general$nbyears),step=1),

          shinyBS::bsTooltip("mcyears", " Monte-Carlo years to consider in water values calculation.",
                             "bottom"),
          materialSwitch("pumping_cal","Pumping",
                         value=F,status = "success")%>%
            shinyInput_label_embed(
              shiny_iconlink() %>%
                bs_embed_popover(title ="Take pumping into account. Use it when your simulations have aggregated pumping.")),

          conditionalPanel(condition = "input.pumping_cal",
                           uiOutput("eff") ),


          shinyBS::bsTooltip("efficiency", " The the efficiency ratio of pumpin you want to take in account in simulations.",
                             "bottom"),

          sliderInput("inaccessible_states","Eliminate inaccessible states",
                         value=100,min = 0,max = 100,post  = " %",),

          shinyBS::bsTooltip("inaccessible_states","Tolerance of inaccessible states. For example if equal to 90% we delete the state if this states is inaccessible by 90% of the scenarios.","bottom"),

          # correct outliers option
          materialSwitch("correct_outliers","Use correct outlier to remove noise",
                        value=F,status = "success")%>%
            shinyInput_label_embed(
              shiny_iconlink() %>%
                bs_embed_popover(title ="Outliers in Bellman values are replaced by spline interpolations.")),


          actionButton("Calculate","launch caulculs", icon = icon("check-circle"),
                       align = "center"),
          shinyBS::bsTooltip("Calculate", "Click to start the calculation of te water values using the selected parameters",
                             "bottom"),
#
#           materialSwitch("show_negative","Show negative Water values",
#                         value=T,status = "danger")%>%
#             shinyInput_label_embed(
#               shiny_iconlink() %>%
#                 bs_embed_popover(title ="Applicate a filter to remove the negative values in the graph.
# NB: this is only a display filter the values are unchanged.")),



          materialSwitch("filter","Filter water values",value=F,status = "success")%>%
            shinyInput_label_embed(
              shiny_iconlink() %>%
                bs_embed_popover(title ="Applicate a filter to remove the outliers values in the graph.
NB:  - Negative and positive values are affected by this filter.
       - This is only a display filter the values are unchanged.
                                 ")),

          conditionalPanel(
            condition = "input.filter",
            sliderInput("filtre_ratio",label="Filter extreme water values ratio",min=0,
                        max=100,value=100,post  = " %"),
          ),
          shinyBS::bsTooltip("filtre_ratio", "The filter ratio define the percent to keep from water values eliminating the rest (extreme negatives and positives)",
                             "bottom"),


          actionButton("plot","Show"),
          shinyBS::bsTooltip("plot", " Show the Graph",
                             "bottom"),
          ),

        mainPanel(
          shinycustomloader::withLoader( plotOutput("Watervalues"), type="html", loader="dnaspin"),
          downloadBttn(
            outputId = "download_wv_plot",
            style = "unite",
            color = "primary",
            block = T
          )

      )
      )
    ), #tabpanel 1

  #--------- Bellman graphs UI -----
    tabPanel("Bellman plot",
       sidebarLayout(
         sidebarPanel(
           numericInput("week_id","Week to show",value=2,
                        min=1,max = 52),

           shinyBS::bsTooltip("week_id", "The number of the week you want to plot",
                              "bottom"),

           selectInput("param","Variables",c("Water Values"="vu",
                                             "Bellman"="bell",
                                             "water values and Bellman"="both",
                                             "add Gradient bellman"="all")),
           sliderInput("states_step_ratio",
                       label="choose the reservoir states ratio",min=5,
                       max=100,value=40,step=1),

           shinyBS::bsTooltip("states_step_ratio", " Discretization ratio to generate steps levels between the reservoir capacity and zero.",
                              "bottom"),


         ), #siderbarpanel
         mainPanel(

            plotOutput("plot_Bellman"),
            downloadBttn(
              outputId = "download_Bellman_plot",
              style = "unite",
              color = "primary",
              block = T
            ),

         )
       ), # Siderbar
  ),#tabpanel 2


  #------ Reward Plot ------------
    tabPanel("Rewards Plot",

          sidebarLayout(

            sidebarPanel(

              pickerInput("district_name_rew",
                          "choose the District",
                          opts$districtList,
                          options = list(
                            `live-search` = TRUE)),
              shinyBS::bsTooltip("district_name_rew", " the district that will be used in calculation of the rewards of transitions.",
                                 "bottom"),
              actionButton("import_reward","Import reward"),

              textInput("simulation_name_pattern","simulation name patern"
                        ,value="weekly_water_amount_"),
              shinyBS::bsTooltip("simulation_name_pattern", " the simulation name pattern must be adequate with the simulaion names (just remove constraints values from the names).",
                                 "bottom"),

              sliderInput("week_id_rew","Week to show",value=c(2,2),
                           min=1,max = 52),

              shinyBS::bsTooltip("week_id_rew", "The weeks you want to plot",
                                 "bottom"),

              selectInput("param_rew","Type",c("Reward"="r",
                                                "Reward 1 MC"="r1",
                                                "Reward variation"="rv",
                                                "reward variation 1Mc"="rv1")),

              conditionalPanel(
                condition = "['rv1','r1'].includes(input.param_rew)",
                sliderInput("Mc_year",label="Monte-Carlo year",min=1,
                            max=opts$parameters$general$nbyears,value=c(1,1)
                            ,step = 1)
              ),


              shinyBS::bsTooltip("Mc_year", "The scenarios that you want to plot",
                                 "bottom"),

              downloadBttn(
                outputId = "download_reward_base",
                style = "unite",
                color = "primary",
                block = T
              )
              ),

              mainPanel(
                plotOutput("rewardplot"),
                downloadBttn(
                  outputId = "download_reward_plot",
                  style = "unite",
                  color = "primary",
                  block = T
                ),
                # plotOutput("reward_second_plot"),

                DT::dataTableOutput("reward_table")
              )


             )


             ),#end tabpanel 3


  #----------Post process -----
    tabPanel("Post Process",

             sidebarLayout(fluid = TRUE,

               sidebarPanel(
                 h3(strong("Remove outlier water values")),
                 switchInput("Run_remove_out",
                               value=F, offStatus = "danger",
                             onStatus = "success")%>%
                 shinyInput_label_embed(
                   shiny_iconlink() %>%
                     bs_embed_popover(title ="Activate filter to remove outlier values.
      NB: this filter affect yourwater values.")),





                 conditionalPanel(

                   condition = "input.Run_remove_out",
                   column(4, numericInput(inputId = "min_rm",
                                          label = "Min",
                                          value = 0,
                                          step = 1,
                                          width = '100%')),
                   column(4, numericInput(inputId = "max_rm",
                                          label = "Max",
                                          value = 200,
                                          step = 1,
                                          width = '100%')),
                   column(4, materialSwitch("rm_NaN",
                                          label = "NaN",
                                          value = T,
                                          status = "info",
                                          width = '100%'))),
                 shinyBS::bsTooltip("min_rm", "Delete all water values that are under this value",
                                    "bottom"),
                 shinyBS::bsTooltip("max_rm", "Delete all water values that are bigger then this value",
                                    "bottom"),

                 linebreaks(3),

                 h3(strong("Fill The rest of water values")),
                 column(12,conditionalPanel(
                   condition = "input.Run_remove_out",
                   materialSwitch("use_filtred",label = "Use Filtred",
                                  value=F, status="info")%>%
                     shinyInput_label_embed(
                       shiny_iconlink() %>%
                         bs_embed_popover(title ="Use the water values after the application of the filter above or use the original one for the next steps."))
                   ),


                 radioGroupButtons(
                   inputId = "method_post_process",
                   label = "Select method",
                   choices = c("Imputation","Constant values","None"),
                   individual = TRUE,
                   justified = TRUE,
                   selected ="None",
                   checkIcon = list(
                     yes = icon("ok",
                                lib = "glyphicon"))  ),

                 shinyBS::bsTooltip("method_post_process", "the method to use to complete the water values of the inaccessible states and the deleted values. ",
                                    "bottom"),


                conditionalPanel(

                  condition="input.method_post_process=='Imputation'",



                numericInput("max_cost","Max Water value price",value=3000),

                shinyBS::bsTooltip("max_cost", "the water value that you want to affect when you have an empty reservoir",
                                   "bottom"),
                numericInput("min_cost","Min Water value price",value=-150),

                shinyBS::bsTooltip("min_cost", "the water value that you want to affect when you have a full reservoir",
                                   "bottom"),
                materialSwitch("full_imputation","Impute NaN values",
                          value=T,status = "success"),

                pickerInput("impute_method","Impute method",
                    c("pmm", "midastouch", "sample", "cart","rf","mean","norm",
                      "norm.nob","norm.boot","norm.predict","quadratic","ri",
                      "2l.norm","2l.lmer","2l.pan","2lonly.mean","2lonly.norm"),
                    selected="norm.predict",
                    options = list(
                      `live-search` = TRUE))%>%
                  shinyInput_label_embed(
                    shiny_iconlink() %>%
                      bs_embed_popover(title ="the method used to impute NaN values you can read on each method details using the command help(mice)."))  ),


                conditionalPanel(

                  condition="input.method_post_process=='Constant values'",

                  numericInput("max_vu","Max Water value price",value=NULL),
                  shinyBS::bsTooltip("max_vu", "the water value that you want to affect when you have an empty reservoir",
                                     "bottom"),
                  numericInput("min_vu","Min Water value price",value=NULL),
                  shinyBS::bsTooltip("min_vu", "the water value that you want to affect when you have a full reservoir",
                                     "bottom"),
                  ),
                ),


                numericInput("adjust","Adjust value",value=0),
                shinyBS::bsTooltip("adjust", "The value will be added to all the water values. for substraction put negative value",
                                   "bottom"),

                h3(strong("Force Monotonic")),

                h4("Using permutation"),
                switchInput("force_monotonic",
                            value=F, offStatus = "danger",
                            onStatus = "success")%>%
                  shinyInput_label_embed(
                    shiny_iconlink() %>%
                      bs_embed_popover(title ="this filter do a permutation of water values to assure that the water values become decreasing with the reservoir level.")),
                h4("Using replacement"),
                switchInput("force_monotonic_JM",
                            value=F, offStatus = "danger",
                            onStatus = "success")%>%
                  shinyInput_label_embed(
                    shiny_iconlink() %>%
                      bs_embed_popover(title ="this filter replace a water value by the previous state value if he is bigger water values to assure that the water values become decreasing with the reservoir level.")),
                h4("Using 2 direction algorithm"),
                switchInput("force_monotonic_JM2",
                            value=F, offStatus = "danger",
                            onStatus = "success")%>%
                  shinyInput_label_embed(
                    shiny_iconlink() %>%
                      bs_embed_popover(title ="this filter replace a water value by the exploring the other levels in two direction simultaneously in the case he find a solution in each way he do the mean.")),



                tags$button(
                  id = "to_antares",
                  type="button",
                  class = "btn action-button btn-large btn-primary",
                  img(src = "https://antares-simulator.org/static/img/antares-256.png",
                  height = "50px",
                  HTML('<i class="icon-star"></i>To Antares'))
                ),
                shinyBS::bsTooltip("to_antares", "convert the water values to antares format than implemented in the desired area",
                                   "bottom")

                ),#sidebarPanel




               mainPanel(
                 shinycustomloader::withLoader(plotOutput("post_process"), type="html", loader="dnaspin"),
                 downloadBttn(
                   outputId = "download_pp_plot",
                   style = "unite",
                   color = "primary",
                   block = T
                 ),

               )
             ) #sidebarLayout

             ) #end tabpanel "Post Process"

   ), #end navbarMenu

  #----------Results UI-------
  navbarMenu("Results panels",

  tabPanel("Results",


            sidebarLayout(
              sidebarPanel(


                pickerInput(
                  inputId = "res_sim_name",
                  label = "Choose your simulation",
                  choices = getSimulationNames("",opts = opts),
                  options = list(
                    `live-search` = TRUE)
                ),

                pickerInput(
                  inputId = "res_area",
                  label = "Choose your Area",
                  choices = opts$areaList,
                  options = list(
                    `live-search` = TRUE)
                ),

                radioGroupButtons(
                  inputId = "res_timeStep",
                  label = "Select Time Step",
                  choices = c("daily","weekly","monthly"),
                  individual = TRUE,
                  justified = TRUE,
                  checkIcon = list(
                    yes = icon("ok",
                               lib = "glyphicon"))
                ),


                radioGroupButtons(
                  inputId = "res_MC",
                  label = "Select Monte Carlo Year",
                  choices = c("Synthesis Year"="NULL","All"='all',"Custom"),
                  individual = TRUE,
                  justified = TRUE,
                  checkIcon = list(
                    yes = icon("ok",
                               lib = "glyphicon"))
                ),

                conditionalPanel(
                  condition="input.res_MC=='Custom'",
                  sliderInput("res_mc_year",label="choose the MC years to use",min=1,
                              max=opts$parameters$general$nbyears,
                              value=c(1,opts$parameters$general$nbyears),step=1)
                ),


                h3(strong("Check Pmin and Pmax")),
                switchInput("Run_P_check",
                            value=F, offStatus = "danger",
                            onStatus = "success"),
                conditionalPanel(

                  condition = "input.Run_P_check",
                  fileInput("Pmin_file","select your Pmin txt file",accept=".txt"),
                  fileInput("Pmax_file","select your Pmax txt file",accept=".txt"),
                  radioGroupButtons(
                    inputId = "P_timeStep",
                    label = "Select Time Step",
                    choices = c("hourly","daily"),
                    individual = TRUE,
                    justified = TRUE,
                    checkIcon = list(
                      yes = icon("ok",
                                 lib = "glyphicon"))  ) )



              ),   # end sidebarpanel

              mainPanel(

                shinycustomloader::withLoader(plotOutput("reservoir"), type="html", loader="dnaspin"),
                downloadBttn(
                  outputId = "download_reservoir_plot",
                  style = "unite",
                  color = "primary",
                  block = T
                ),

                shinycustomloader::withLoader(plotOutput("flow"), type="html", loader="dnaspin"),
                downloadBttn(
                  outputId = "download_flow",
                  style = "unite",
                  color = "primary",
                  block = T
                ),

                shinycustomloader::withLoader(plotOutput("pmin_pmax"), type="html", loader="dnaspin"),
                downloadBttn(
                  outputId = "download_pmin_pmax_plot",
                  style = "unite",
                  color = "primary",
                  block = T
                )


              )

              ) #end sidebar Panel

           ),  #end tabpanel "Results"

  #---- Reporting ------

  tabPanel("Reporting",

           sidebarLayout(

             sidebarPanel(


               radioGroupButtons(
                 inputId = "report_type",
                 label = "Select ",
                 choices = c("area","district"),
                 individual = TRUE,
                 justified = TRUE,
                 checkIcon = list(
                   yes = icon("ok",
                              lib = "glyphicon"))),

              conditionalPanel(
                condition = "input.report_type=='area'",
                pickerInput("report_area",
                            "choose the area",
                            append(opts$areaList,"all",after=0),
                            options = list(
                              `live-search` = TRUE),
                            multiple = TRUE)
              ),

              conditionalPanel(
                condition = "input.report_type=='district'",
                pickerInput("report_district",
                            "choose the district",
                            append(opts$districtList,"all",after=0),
                            options = list(
                              `live-search` = TRUE),
                            multiple = TRUE)
              ),

              pickerInput(inputId = "report_sim1",
                          label = "Select simulations Set 1",
                          choices = getSimulationNames("",opts = opts),
                          options = list(
                            `actions-box` = TRUE,
                            `live-search` = TRUE),
                          multiple = TRUE),



                 pickerInput(inputId = "watervalues_areas1",
                             label = "Select areas using watervalues in Set 1",
                             choices = opts$areaList,
                             options = list(
                               `actions-box` = TRUE,
                               `live-search` = TRUE),
                             multiple = TRUE),

              pickerInput(inputId = "report_sim2",
                          label = "Select simulations Set 2",
                          choices = getSimulationNames("",opts = opts),
                          options = list(
                            `actions-box` = TRUE,
                            `live-search` = TRUE),
                          multiple = TRUE),



              pickerInput(inputId = "watervalues_areas2",
                          label = "Select areas using watervalues in Set 2",
                          choices = opts$areaList,
                          options = list(
                            `actions-box` = TRUE,
                            `live-search` = TRUE),
                          multiple = TRUE),

              pickerInput(

                inputId = "report_vars",
                label = "Select variables",
                choices = otp_variables,
                options = list(
                  `actions-box` = TRUE,
                  `live-search` = TRUE),
                multiple = TRUE),


               radioGroupButtons(
                 inputId = "report_mcyear_mode",
                 label = "Select Time Step",
                 choices = c("Synthesis","Custom"),
                 individual = TRUE,
                 justified = TRUE,
                 checkIcon = list(
                   yes = icon("ok",
                              lib = "glyphicon"))),

               conditionalPanel(
                 condition = "input.report_mcyear_mode=='Custom'",

                 sliderInput("report_mcyear",
                             label="choose the number of MC years to use",
                             min=1,max=opts$parameters$general$nbyears,
                             value=1, step=1)
                ),

               pickerInput(

                 inputId = "table_vars",
                 label = "Select table variables",
                 choices = append(otp_variables,c("area"),after=0),
                 options = list(
                   `actions-box` = TRUE,
                   `live-search` = TRUE),
                 multiple = TRUE)
               ),


             mainPanel(

               shinycustomloader::withLoader(plotOutput("report"), type="html", loader="dnaspin"),
               downloadBttn(
                 outputId = "download_report_plot",
                 style = "unite",
                 color = "primary",
                 block = T
               ) ,

               DT::dataTableOutput("report_table")
             ) #end mainPanel
           )


  )#end tabpanel Reporing


  ) #end navbarMenu


) #navbar

) #UI

#------Server functions ------
server <- function(input, output, session) {

  global <- reactiveValues(datapath = getwd())
  rv <- reactiveValues()

  output$dir <- renderUI({
    textInput("sim_output_dir","Saving directory",value=global$datapath)

  })

  output$eff <- renderUI({ numericInput("efficiency","Efficiency pumping ratio",min=0,max=1,
                             value=getPumpEfficiency(area=input$Area,opts = opts))
  })

  observeEvent(input$simulate,

               spsUtil::quiet({

                  spsComps::shinyCatch({
                     simulation_res <-    runWaterValuesSimulation(
                     area=input$sim_area,
                     simulation_name = input$sim_simulation_name,
                     nb_disc_stock = input$sim_nb_disc_stock,
                     nb_mcyears = input$sim_mcyears,
                     path_solver =input$solver_path,
                     binding_constraint = input$sim_binding_constraint,
                     fictive_area = input$sim_fictive_area,
                     thermal_cluster = input$sim_thermal_cluster,
                     remove_areas=input$remove_areas,
                     overwrite = T,
                     link_from=input$link_from,
                     opts = opts,
                     shiny=T,
                     otp_dest=input$sim_output_dir,
                     file_name=input$file_name,
                     pumping = input$pumping)},prefix = "")},print_cat = F,
                  message = F, warning = silent))





    simulation_res <- reactive({


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


    reward_db <- reactive({


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

    inflow <- reactive({


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

    observeEvent( input$Calculate,

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
        district_name =input$district_name ,
        method=input$method,
        states_step_ratio=(1/input$nb_states),
        mcyears=input$mcyears[1]:input$mcyears[2],
        reservoir_capacity=NULL,
        correct_outliers =input$correct_outliers,
        q_ratio=input$q_ratio/100,
        shiny=T,
        inaccessible_states = input$inaccessible_states/100,
        until_convergence = input$until_convergence,
        convergence_rate = input$convergence_rate/100,
        convergence_criteria = input$convergence_criteria,
        cycle_limit = input$cycle_limit,
        pumping = input$pumping_cal,
        efficiency = input$efficiency
        )$aggregated_results

      isolate(rv$results <- results)
      show_alert(
        title = "Water Values",
        text = "Calculation Done !!",
        type = "success"
      )

      pp_results <- data.table::copy(results)
      rv$pp_results <- pp_results
      },blocking_level="error",position = "top-center", shiny = TRUE,prefix = "" )

        },print_cat = F, message = F, warning = silent)


      )





    watervalues <- eventReactive(input$plot,
                                 {waterValuesViz(rv$results,
                                    filtre_ratio =input$filtre_ratio/100)})

    output$Watervalues <- renderPlot(watervalues())

    shinyBS::addPopover(session,"Watervalues",title = "water values",content = "This graph describe the water values for each week starting from week 1 to week 52 in the X-axis and the level of the reservoir in perecent in the Y-axis. the water values are determined by the colors you can see them in the legend of the graph. The blank zones in the graph mean that those states are inaccessible. ")

    output$download_wv_plot <- downloadHandler(
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

    output$plot_Bellman <- renderPlot(plot_Bellman(rv$results,input$week_id,
                                                   input$param,states_step_ratio =
                                                   1/input$states_step_ratio))

    output$download_Bellman_plot <- downloadHandler(
      filename = function() {
        paste('Bellman-', Sys.Date(), '.png', sep='')
      },
      content = function(con) {

        grDevices::png(con ,width = 1200,
            height = 766)
        print(plot_Bellman(rv$results,input$week_id,
                           input$param,states_step_ratio =
                             1/input$states_step_ratio))
        grDevices::dev.off()
      }
    )

#--------plot reward page------

    observeEvent( input$import_reward,
                  {
                    spsComps::shinyCatch({
                    reward_dt <- get_Reward(simulation_res(),
                                            district_name =input$district_name_rew,
                                            opts=opts)
                    rv$reward_dt <- reward_dt
                    shinybusy::remove_modal_spinner()
                    show_alert(
                      title = "Rewards",
                      text = "Importation Done !!",
                      type = "success"
                    )
                    })
                  }
                )


    rewardplot <- reactive(

      {

      if(is.null(rv$reward))rv$reward <- reward_db()

      week_id_rew <- input$week_id_rew[1]:input$week_id_rew[2]
      Mc_year <- input$Mc_year[1]:input$Mc_year[2]
      if(input$param_rew=="r")
      {plot_reward(rv$reward_dt$reward,week_id_rew,input$simulation_name_pattern,constraints_values=simulation_res()$simulation_values)
      }else{
        if(input$param_rew=="rv")
          {plot_reward_variation(rv$reward_dt$reward,week_id_rew,constraints_values=simulation_res()$simulation_values)
        }else{

        if(input$param_rew=="r1")
          {plot_reward_mc(rv$reward_dt$reward,week_id_rew,
                          Mc_year,input$simulation_name_pattern,constraints_values=simulation_res()$simulation_values)
        }else{

          if(input$param_rew=="rv1")
          {plot_reward_variation_mc(rv$reward_dt$reward,week_id_rew,
                              Mc_year,constraints_values=simulation_res()$simulation_values)}
        }
        }
        }
}
       )

    reward_var_plot <- reactive({

      week_id_rew <- input$week_id_rew[1]:input$week_id_rew[2]
      Mc_year <- input$Mc_year[1]:input$Mc_year[2]

      if(input$param_rew=="r1")
      {plot_reward_variation_mc(rv$reward_dt$reward,week_id_rew,
                                Mc_year)
      }else{

        plot_reward_variation(rv$reward_dt$reward,week_id_rew,constraints_values=simulation_res()$simulation_values)
      }


    })




    output$rewardplot <- renderPlot(rewardplot()$graph)
    # output$reward_second_plot <- renderPlot(reward_var_plot()$graph)
    output$reward_table <- DT::renderDataTable(rewardplot()$table)
    output$download_reward_plot <- downloadHandler(
      filename = function() {
        paste('Reward-', Sys.Date(), '.png', sep='')
      },
      content = function(con) {
       grDevices::png(con ,width = 1200,height = 766)
        print(rewardplot()$graph)
        grDevices::dev.off()
      }
    )


    observe({
      if(!is.null(rv$reward_dt))
        isolate(
          reward_base <<- rv$reward_dt
        )
    })

    output$download_reward_base <- downloadHandler(
      filename <- function(){
        paste('Reward-Base-', Sys.Date(), '.Rdata', sep='') },

      content = function(file) {
        save(reward_base, file = file)
      }
    )
    # end reward Plot


#--------post process----------
    results_temp <- reactive({

      if(input$Run_remove_out){
      remove_out(rv$results,min = input$min_rm,max=input$max_rm,NAN=input$rm_NaN)
      }else{
        rv$results
      }
      })

    post_result <- reactive({
      fix_v <- FALSE
      if(input$method_post_process=="Constant values"){
        fix_v <- TRUE
      }



      if(input$use_filtred){
        withProgress( post_process(results = results_temp(),max_cost=input$max_cost,
                     min_cost =input$min_cost,
                     full_imputation=input$full_imputation,
                     impute_method=input$impute_method,fix = fix_v,
                     max_vu =input$max_vu,min_vu = input$min_vu ))
      }else{

        withProgress(post_process(results = rv$results,max_cost=input$max_cost,
                     min_cost =input$min_cost,
                     full_imputation=input$full_imputation,
                     impute_method=input$impute_method,fix = fix_v,
                     max_vu =input$max_vu,min_vu = input$min_vu ))
      }

    })


    pre_final_result <- reactive({


      if(input$method_post_process=="None"){
        if(input$force_monotonic){
          monotonic_VU(results_temp())
        }else{
          if(input$force_monotonic_JM){
            monotonic_JM(results_temp())
          }else{
            if(input$force_monotonic_JM2){
              monotonic_JM2(results_temp())
            }else{
          results_temp()}
        }}
      }else{
        if(input$force_monotonic){
          monotonic_VU(post_result())
        }else{
          if(input$force_monotonic_JM){
            monotonic_JM(post_result())
          }else{
            if(input$force_monotonic_JM2){
              monotonic_JM2(post_result())
              }else{
          post_result()
            }

            }
        }
     }

    })

    final_result <- reactive(
      {
        adjust_wv(pre_final_result(),value=input$adjust)
      }
    )

    observeEvent(input$reset,
                 {
                   rv$result <-rv$pp_results
                   show_alert(
                     title = "Post process",
                     text = "Reset Done !!",
                     type = "success"
                   )
                 }

                 )



    output$post_process <- renderPlot(
      waterValuesViz(final_result())
      )

    output$download_pp_plot <- downloadHandler(
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

    observeEvent(input$to_antares,{

                 results <- final_result()
                 results <- results[results$weeks!=53,]

                 reshaped_values <- to_Antares_Format(results)
                 antaresEditObject::writeWaterValues(
                   area = input$Area,
                   data = reshaped_values
                 )
                 show_alert(
                   title = "Implement water values in Antares",
                   text = " Done !!",
                   type = "success"
                 )
                 }

    )

#------Results page--------


    reservoir <- reactive({

      if(input$res_MC=="Custom"){
        mc_year <- input$res_mc_year[1]:input$res_mc_year[2]
      }else{
        if(input$res_MC=="all"){mc_year <- "all"
        }else{mc_year <- NULL}
      }

      plot_reservoir(input$res_area,input$res_timeStep,
                     simulation_name = input$res_sim_name,
                     mcyear=mc_year,opts = opts, shiny = TRUE)
    })

    output$reservoir <- renderPlot(reservoir())

    output$download_reservoir_plot <- downloadHandler(
      filename = function() {
        paste('Reservoir-', Sys.Date(), '.png', sep='')
      },
      content = function(con) {
        grDevices::png(con ,width = 1200,
            height = 766)
        print(reservoir())
        grDevices::dev.off()
      }
    )


    flow <- reactive({

      if(input$res_MC=="Custom"){
        mc_year <- input$res_mc_year[1]:input$res_mc_year[2]
      }else{
        if(input$res_MC=="all"){mc_year <- "all"
        }else{mc_year <- NULL}
      }

      plot_flow(input$res_area,input$res_timeStep,
                     simulation_name = input$res_sim_name,
                     mcyear=mc_year,opts = opts, shiny = TRUE)
    })



    output$flow <- renderPlot(flow())

    output$download_flow <- downloadHandler(
      filename = function() {
        paste('Flow-', Sys.Date(), '.png', sep='')
      },
      content = function(con) {
        grDevices::png(con ,width = 1200,
                       height = 766)
        print(flow())
        grDevices::dev.off()
      }
    )


    pmin_pmax <- reactive({

      ext_Pmin <- tools::file_ext(input$Pmin_file$datapath)
      ext_Pmax <- tools::file_ext(input$Pmax_file$datapath)

      req(input$Pmin_file)
      validate(need(ext_Pmin == "txt", "Please upload a txt file"))
      req(input$Pmax_file)
      validate(need(ext_Pmax == "txt", "Please upload a txt file"))

      if(input$res_MC=="Custom"){
        mc_year <- input$res_mc_year
      }else{
        if(input$res_MC=="all"){mc_year <- "all"
        }else{mc_year <- NULL}
      }

      plot_generation(input$res_area,timestep = input$P_timeStep,Mcyear = mc_year,
                      simulation_name=input$res_sim_name,min_path = input$Pmin_file$datapath,
                      max_path = input$Pmax_file$datapath,opts = opts)

    })

    output$pmin_pmax <- renderPlot(pmin_pmax())

    output$download_pmin_pmax_plot <- downloadHandler(
      filename = function() {
        paste('pmin_pmax-', Sys.Date(), '.png', sep='')
      },
      content = function(con) {
        grDevices::png(con ,width = 1200,
               height = 766)
        print(pmin_pmax())
        grDevices::dev.off()
      }
    )



#------ Reporting------

    report <- reactive({
      if(input$report_mcyear_mode=="Custom"){
        mc_year <- input$report_mcyear
      }else{
        mc_year <- NULL
      }

      tab1 <- report_data(simulations=input$report_sim1,type=input$report_type,
                           area_list = input$report_area,district_list=input$report_district,
                   mcyears=mc_year,opts=opts,plot_var=input$report_vars,
                   watervalues_areas=input$watervalues_areas1,return_table = T)
      tab2 <- report_data(simulations=input$report_sim2,type=input$report_type,
                           area_list = input$report_area,district_list=input$report_district,
                           mcyears=mc_year,opts=opts,plot_var=input$report_vars,
                           watervalues_areas=input$watervalues_areas2,return_table = T)
      if(is.null(tab2))  data <- tab1
      if(is.null(tab1))  data <- tab2
      if(!(is.null(tab1)|is.null(tab2))) data <- rbind(tab1,tab2)

      data

    })

    output$report <- renderPlot(just_plot_report(report(),input$report_vars,
                                    plot_type=(input$report_type=="district")))

    output$download_report_plot <- downloadHandler(
      filename = function() {
        paste('report-', Sys.Date(), '.png', sep='')
      },
      content = function(con) {
        grDevices::png(con ,width = 1200,
            height = 766)
        print(just_plot_report(report(),input$report_vars,
                               plot_type=(input$report_type=="district")))
        grDevices::dev.off()
      }
    )

    report_table <- reactive({
         table <- select(report(),dput(append(input$table_vars,c("sim_name"),after = 0)))

    })

    output$report_table <- DT::renderDataTable(report_table())


}









#------Run-----
options(shiny.launch.browser=TRUE)
options(shiny.sanitize.errors = FALSE)
shinyApp(ui = ui, server = server)
}

