#' Open watervalues Calculator in APP Web
#'
#' @param simulation_res
#'   List of simulation results returned by the function
#'   \code{watervalues::runWaterValuesSimulation}
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @import  shiny
#' @import shinythemes
#' @import shinyWidgets
#' @import shinycustomloader
#' @importFrom shinybusy add_busy_gif
#' @import spsComps
#' @import periscope
#' @export

shiny_Grid_matrix <- function(simulation_res,opts=antaresRead::simOptions())

{

otp_variables <- c("OV. COST", "OP. COST", "OP. COST_std", "OP. COST_min", "OP. COST_max",
    "MRG. PRICE", "MRG. PRICE_std", "MRG. PRICE_min", "MRG. PRICE_max",
    "CO2 EMIS.", "BALANCE", "BALANCE_std", "BALANCE_min", "BALANCE_max",
    "ROW BAL.", "PSP", "MISC. NDG", "LOAD", "LOAD_std", "LOAD_min",
    "LOAD_max", "H. ROR", "H. ROR_std", "H. ROR_min", "H. ROR_max",
    "WIND", "WIND_std", "WIND_min", "WIND_max", "SOLAR", "SOLAR_std",
    "SOLAR_min", "SOLAR_max", "NUCLEAR", "NUCLEAR_std", "NUCLEAR_min",
    "NUCLEAR_max", "LIGNITE", "LIGNITE_std", "LIGNITE_min", "LIGNITE_max",
    "COAL", "COAL_std", "COAL_min", "COAL_max", "GAS", "GAS_std",
    "GAS_min", "GAS_max", "OIL", "OIL_std", "OIL_min", "OIL_max",
    "MIX. FUEL", "MIX. FUEL_std", "MIX. FUEL_min", "MIX. FUEL_max",
    "MISC. DTG", "MISC. DTG_std", "MISC. DTG_min", "MISC. DTG_max",
    "H. STOR", "H. STOR_std", "H. STOR_min", "H. STOR_max", "H. PUMP",
    "H. PUMP_std", "H. PUMP_min", "H. PUMP_max", "H. LEV", "H. LEV_std",
    "H. LEV_min", "H. LEV_max", "H. INFL", "H. INFL_std", "H. INFL_min",
    "H. INFL_max", "H. OVFL", "H. OVFL_std", "H. OVFL_min", "H. OVFL_max",
    "H. VAL", "H. VAL_std", "H. VAL_min", "H. VAL_max", "H. COST",
    "H. COST_std", "H. COST_min", "H. COST_max", "UNSP. ENRG", "UNSP. ENRG_std",
    "UNSP. ENRG_min", "UNSP. ENRG_max", "SPIL. ENRG", "SPIL. ENRG_std",
    "SPIL. ENRG_min", "SPIL. ENRG_max", "LOLD", "LOLD_std", "LOLD_min",
    "LOLD_max", "LOLP", "AVL DTG", "AVL DTG_std", "AVL DTG_min",
    "AVL DTG_max", "DTG MRG", "DTG MRG_std", "DTG MRG_min", "DTG MRG_max",
    "MAX MRG", "MAX MRG_std", "MAX MRG_min", "MAX MRG_max", "NP COST",
    "NP COST_std", "NP COST_min", "NP COST_max", "NODU", "NODU_std",
    "NODU_min", "NODU_max")

#------User interface-----
linebreaks <- function(n){HTML(strrep(br(), n))}

ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = shinytheme("cerulean"),
  shinybusy::add_busy_gif(src = "https://github.com/dhia-gharsallaoui/watervalues/blob/main/static/calculating 3.gif?raw=true",
                          position="bottom-right",height = 70, width = 70)
  ,


  navbarPage("Water Values !",

    #Grid matrix page
    tabPanel("Calculate Water Values",

      sidebarLayout(

        sidebarPanel(
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


          #number of states:
          sliderInput("nb_states",label="choose the number of states",min=5,
                      max=100,value=40,step=1),

          #MC years:
          sliderInput("max_mcyears",label="choose the number of MC years to use",min=1,
                      max=opts$parameters$general$nbyears,
                      value=c(1,opts$parameters$general$nbyears),step=1),

          # correct outliers option
          materialSwitch("correct_outliers","Use correct outlier to remove noise",
                        value=T,status = "success"),


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
          withLoader( plotOutput("Watervalues"), type="html", loader="dnaspin"),
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

                  numericInput("max_vu","Max Water value price",value=3000),

                  numericInput("min_vu","Min Water value price",value=-150),

                  ),
                ),




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
                 withLoader(plotOutput("post_process"), type="html", loader="dnaspin"),
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
                                 lib = "glyphicon"))  ) ),

                  h3(strong("Reporting")),
                  pickerInput(inputId = "report_sim",
                              label = "Select simulations",
                              choices = getSimulationNames("",opts = opts),
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
                              multiple = TRUE)













              ),   # end sidebarpanel

              mainPanel(

                plotOutput("reservoir"),
                downloadBttn(
                  outputId = "download_reservoir_plot",
                  style = "unite",
                  color = "primary",
                  block = T
                ),
                plotOutput("pmin_pmax"),
                downloadBttn(
                  outputId = "download_pmin_pmax_plot",
                  style = "unite",
                  color = "primary",
                  block = T
                )


              )

              ) #end sidebar Panel

           )  #end tabpanel "Post Process"

) #navbar
) #UI

#------Server functions ------
server <- function(input, output) {


    rv <- reactiveValues()
    observeEvent( input$Calculate,

    {

    shinyCatch({
      results <-     Grid_Matrix(
        area = input$Area,
        simulation_names = simulation_res$simulation_names,
        simulation_values = simulation_res$simulation_values,
        nb_cycle = input$nb_cycle,
        opts = opts,
        week_53 = 0,
        district_name =input$district_name ,
        method=input$method,
        states_step_ratio=(1/input$nb_states),
        max_mcyears=input$max_mcyears[1]:input$max_mcyears[2],
        reservoir_capacity=NULL,
        correct_outliers =input$correct_outliers,
        q_ratio=input$q_ratio,
        shiny=T)

      isolate(rv$results <- results)
      show_alert(
        title = "Water Values",
        text = "Calculation Done !!",
        type = "success"
      )

      pp_results <- copy(results)
      rv$pp_results <- pp_results
      },blocking_level="error" )

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
        png(con ,width = 1200,
            height = 766)
        print(watervalues())
        dev.off()
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

        png(con ,width = 1200,
            height = 766)
        print(plot_Bellman(rv$results,input$week_id,
                           input$param,states_step_ratio =
                             1/input$states_step_ratio))
        dev.off()
      }
    )

#--------plot reward page------

    observeEvent( input$import_reward,
                  {

                    shinyCatch({
                    reward_dt <- get_Reward(simulation_res$simulation_names,
                                            district_name =input$district_name_rew,
                                            opts)
                    rv$reward_dt <- reward_dt
                    remove_modal_spinner()
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
       png(con ,width = 1200,
            height = 766)
        print(rewardplot())
        dev.off()
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


    final_result <- reactive({


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
        png(con ,width = 1200,
            height = 766)
        print(waterValuesViz(final_result()))
        dev.off()
      }
    )

    observeEvent(input$to_antares,{

                 results <- final_result()
                 results <- results[results$weeks!=53,]

                 reshaped_values <- to_Antares_Format(results)
                 writeWaterValues(
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
        png(con ,width = 1200,
            height = 766)
        print(reservoir())
        dev.off()
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
        png(con ,width = 1200,
               height = 766)
        print(pmin_pmax())
        dev.off()
      }
    )


}









#------Run-----
options(shiny.launch.browser=TRUE)
shinyApp(ui = ui, server = server)
}

