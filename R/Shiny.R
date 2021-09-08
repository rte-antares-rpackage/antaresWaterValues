#' Open watervalues Calculator in APP Web
#'
#' @param simulation_res
#'   List of simulation results returned by the function
#'   \code{watervalues::runWaterValuesSimulation}
#' @param study_path the path of the Antares study
#'
#'
#' @import  shiny
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
#' @export

shiny_water_values <- function(simulation_res=NULL,study_path,...)

{

opts <- antaresRead::setSimulationPath(path = study_path, simulation = "input")
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
    tabPanel("SImulations",

          sidebarLayout(


           sidebarPanel(
             pickerInput("sim_area",
                         "Choose the area",
                         opts$areaList,
                         options = list(
                           `live-search` = TRUE)),

             textInput("sim_simulation_name","Simulation name "
                       ,value="weekly_water_amount_%s"),

             numericInput("sim_nb_disc_stock","Number of reservoir discretization",value=2,
                          min=1),
             sliderInput("sim_mcyears",label="choose the number of MC years to simulate",min=1,
                         max=opts$parameters$general$nbyears,
                         value=opts$parameters$general$nbyears,step=1),

             textInput("sim_binding_constraint","Name of the binding constraint "
                       ,value="WeeklyWaterAmount"),

             textInput("sim_fictive_area","Name of the fictive area to create "
                       ,value="fictive_watervalues"),

             textInput("sim_thermal_cluster","Name of the thermal cluster to create"),

             # fileInput("sim_solver", label = "Select your solver"),

             uiOutput("dir"),

             textInput("file_name","File name",value="simulation results"),



             actionButton("simulate","Launch simulations"),


             ), #end sidebarPanel

           mainPanel()

           )# end sidebar layout

          ), #end page



    #-------calculate water values page ------
    tabPanel("Calculate Water Values",

      sidebarLayout(


        sidebarPanel(
          fileInput("ini_file", label = "Rdata file containing the simulations"),

          #area
          pickerInput("Area",
            "choose the area",
            opts$areaList,
            options = list(
              `live-search` = TRUE)),

          #District List
          pickerInput("district_name",
                      "choose the District",
                      opts$districtList,
                      options = list(
                        `live-search` = TRUE)),


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

          conditionalPanel(
            condition = "input.method=='quantile'",
            sliderInput("q_ratio",label=NULL,min=0,max=1,value=0.5),
          ),


          #number of cycles
          numericInput("nb_cycle","number of cycle to calculate",value=2,
                       min=1),

          #week 53 value
          numericInput("week_53","water value initial condition",value=0),


          #number of states:
          sliderInput("nb_states",label="choose the number of states",min=5,
                      max=100,value=40,step=1),

          #MC years:
          sliderInput("mcyears",label="choose the number of MC years to use",min=1,
                      max=opts$parameters$general$nbyears,
                      value=c(1,opts$parameters$general$nbyears),step=1),

          materialSwitch("inaccessible_states","Eliminate all inaccessible states",
                         value=F,status = "success"),

          # correct outliers option
          materialSwitch("correct_outliers","Use correct outlier to remove noise",
                        value=F,status = "success"),

          materialSwitch("parallel","Use parallel computing",
                         value=F,status = "success"),

          actionButton("Calculate","launch caulculs", icon = icon("check-circle"),
                       align = "center"),

          materialSwitch("show_negative","Show negative Water values",
                        value=T,status = "danger"),


          materialSwitch("filter","Filter water values",value=F,status = "success"),
          conditionalPanel(
            condition = "input.filter",
            sliderInput("filtre_ratio",label="Filter extreme water values ratio",min=0,
                        max=1,value=1),
          ),


          actionButton("plot","Show"),
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

           selectInput("param","Variables",c("Water Values"="vu",
                                             "Bellman"="bell",
                                             "water values and Bellman"="both",
                                             "add Gradient bellman"="all")),
           sliderInput("states_step_ratio",
                       label="choose the reservoir states ratio",min=5,
                       max=100,value=40,step=1),




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
              actionButton("import_reward","Import reward"),

              textInput("simulation_name_pattern","simulation name patern"
                        ,value="weekly_water_amount_"),

              sliderInput("week_id_rew","Week to show",value=c(2,2),
                           min=1,max = 52),

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

              ),

              mainPanel(
                plotOutput("rewardplot"),
                downloadBttn(
                  outputId = "download_reward_plot",
                  style = "unite",
                  color = "primary",
                  block = T
                ),

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
                             onStatus = "success"),

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
                 linebreaks(3),

                 h3(strong("Fill The rest of water values")),
                 column(12,conditionalPanel(
                   condition = "input.Run_remove_out",
                   materialSwitch("use_filtred",label = "Use Filtred",
                                  value=F, status="info")),
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


                conditionalPanel(

                  condition="input.method_post_process=='Imputation'",



                numericInput("max_cost","Max Water value price",value=3000),

                numericInput("min_cost","Min Water value price",value=-150),

                materialSwitch("full_imputation","Impute NaN values",
                          value=T,status = "success"),

                pickerInput("impute_method","Impute method",
                    c("pmm", "midastouch", "sample", "cart","rf","mean","norm",
                      "norm.nob","norm.boot","norm.predict","quadratic","ri",
                      "2l.norm","2l.lmer","2l.pan","2lonly.mean","2lonly.norm"),
                    selected="norm.predict",
                    options = list(
                      `live-search` = TRUE))),


                conditionalPanel(

                  condition="input.method_post_process=='Constant values'",

                  numericInput("max_vu","Max Water value price",value=NULL),

                  numericInput("min_vu","Min Water value price",value=NULL),

                  ),
                ),


                numericInput("adjust","Adjust value",value=0),

                h3(strong("Force Monotonic")),

                switchInput("force_monotonic",
                            value=F, offStatus = "danger",
                            onStatus = "success"),

                actionBttn(
                  inputId = "reset",
                  label = "Reset",
                  style = "pill",
                  color = "danger"
                ),

                tags$button(
                  id = "to_antares",
                  type="button",
                  class = "btn action-button btn-large btn-primary",
                  img(src = "https://antares-simulator.org/static/img/antares-256.png",
                  height = "50px",
                  HTML('<i class="icon-star"></i>To Antares'))
                )

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

             ), #end tabpanel "Post Process"

  #----------Results UI-------
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




) #navbar
) #UI

#------Server functions ------
server <- function(input, output) {

  global <- reactiveValues(datapath = getwd())

  output$dir <- renderUI({
    textInput("sim_output_dir","Saving directory",value=global$datapath)

  })
  observeEvent(input$simulate,

                {

                  spsComps::shinyCatch({
                     simulation_res <-    runWaterValuesSimulation(
                     area=input$sim_area,
                     simulation_name = input$sim_simulation_name,
                     nb_disc_stock = input$sim_nb_disc_stock,
                     nb_mcyears = input$sim_nb_mcyears,
                     binding_constraint = input$sim_binding_constraint,
                     fictive_area = input$sim_fictive_area,
                     thermal_cluster = input$sim_thermal_cluster,
                     # path_solver=input$sim_path_solver$datapath,
                     overwrite = T,
                     opts = opts,
                     shiny=T,
                     otp_dest=input$sim_output_dir,
                     file_name=input$file_name)})})





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


    rv <- reactiveValues()
    observeEvent( input$Calculate,

    {

    spsComps::shinyCatch({
      results <-     Grid_Matrix(
        area = input$Area,
        simulation_names = simulation_res()$simulation_names,
        simulation_values = simulation_res()$simulation_values,
        nb_cycle = input$nb_cycle,
        opts = opts,
        week_53 = input$week_53,
        district_name =input$district_name ,
        method=input$method,
        states_step_ratio=(1/input$nb_states),
        mcyears=input$mcyears[1]:input$mcyears[2],
        reservoir_capacity=NULL,
        correct_outliers =input$correct_outliers,
        q_ratio=input$q_ratio,
        parallel = input$parallel,
        shiny=T,
        inaccessible_states = input$inaccessible_states)

      isolate(rv$results <- results)
      show_alert(
        title = "Water Values",
        text = "Calculation Done !!",
        type = "success"
      )

      pp_results <- data.table::copy(results)
      rv$pp_results <- pp_results
      },blocking_level="error",position = "top-center", shiny = TRUE )

        }


      )





    watervalues <- eventReactive(input$plot,
                                 {waterValuesViz(rv$results,
                                    filtre_ratio =input$filtre_ratio,
                                    show_negative=input$show_negative)})

    output$Watervalues <- renderPlot(watervalues())

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
                    reward_dt <- get_Reward(simulation_res()$simulation_names,
                                            district_name =input$district_name_rew,
                                            opts)
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

      {week_id_rew <- input$week_id_rew[1]:input$week_id_rew[2]
       Mc_year <- input$Mc_year[1]:input$Mc_year[2]
      if(input$param_rew=="r")
      {plot_reward(rv$reward_dt,week_id_rew,input$simulation_name_pattern)
      }else{
        if(input$param_rew=="rv")
          {plot_reward_variation(rv$reward_dt,week_id_rew,
                                   input$simulation_name_pattern)
        }else{

        if(input$param_rew=="r1")
          {plot_reward_mc(rv$reward_dt,week_id_rew,
                          Mc_year,input$simulation_name_pattern)
        }else{

          if(input$param_rew=="rv1")
          {plot_reward_variation_mc(rv$reward_dt,week_id_rew,
                              Mc_year,input$simulation_name_pattern)}
        }
        }
        }
}
       )

    output$rewardplot <- renderPlot(rewardplot())

    output$download_reward_plot <- downloadHandler(
      filename = function() {
        paste('Reward-', Sys.Date(), '.png', sep='')
      },
      content = function(con) {
       grDevices::png(con ,width = 1200,
            height = 766)
        print(rewardplot())
        grDevices::dev.off()
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
          results_temp()
        }
      }else{
        if(input$force_monotonic){
          monotonic_VU(post_result())
        }else{
          post_result()
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

    output$download_reward_plot <- downloadHandler(
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

