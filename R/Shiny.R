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
#' @import watervalues
#' @export

shiny_Grid_matrix <- function(simulation_res,opts=antaresRead::simOptions())

{



#------User interface-----

ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = shinytheme("cerulean"),

  navbarPage("Water Values !",

    #Grid matrix page
    tabPanel("Calculate Water Values",

      sidebarLayout(

        sidebarPanel(
          #area
          selectInput("Area",
            "choose the area",
            opts$areaList),

          #District List
          selectInput("district_name",
                      "choose the District",
                      opts$districtList),


          # Algorithm

          selectInput("method","Select algorithm to use",
                      c("grid-mean","mean-grid","quantile")),
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
                      value=opts$parameters$general$nbyears,step=1),

          # correct outliers option
          checkboxInput("correct_outliers","Use correct outlier to remove noise",
                        value=T),


          actionButton("Calculate","launch caulculs"),

          checkboxInput("show_negative","Show negative Water values",
                        value=T),


          checkboxInput("filter","Filter water values",value=F),
          conditionalPanel(
            condition = "input.filter",
            sliderInput("filtre_ratio",label="Filter extreme water values ratio",min=0,
                        max=1,value=1),
          ),


          actionButton("plot","Show"),
          ),

        mainPanel(
          plotOutput("Watervalues")

      )
      )
    ), #tabpanel 1

  # Bellman graphs
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

         )
       ), # Siderbar
  ),#tabpanel 2

    tabPanel("Rewards Plot",

          sidebarLayout(

            sidebarPanel(

              selectInput("district_name_rew",
                          "choose the District",
                          opts$districtList),
              actionButton("import_reward","Import reward"),

              textInput("simulation_name_pattern","simulation name patern"
                        ,value="weekly_water_amount_"),

              numericInput("week_id_rew","Week to show",value=2,
                           min=1,max = 52),

              selectInput("param_rew","Type",c("Reward"="r",
                                                "Reward 1 MC"="r1",
                                                "Reward variation"="rv",
                                                "reward variation 1Mc"="rv1")),

              conditionalPanel(
                condition = "['rv1','r1'].includes(input.param_rew)",
                sliderInput("Mc_year",label="Monte-Carlo year",min=1,
                            max=opts$parameters$general$nbyears,value=1,step = 1)
              ),

              ),

              mainPanel(
                plotOutput("rewardplot")

              )


             )


             )


) #navbar
) #UI

#------Server functions ------
server <- function(input, output) {


    rv <- reactiveValues()
    observeEvent( input$Calculate,
    { showModal(modalDialog("Calculating....", footer=NULL))
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
        max_mcyears=input$max_mcyears,
        reservoir_capacity=NULL,
        correct_outliers =input$correct_outliers,
        q_ratio=input$q_ratio)
      rv$results <- results
      removeModal()

      }
    )

    watervalues <- eventReactive(input$plot,
                                 {waterValuesViz(rv$results,
                                    filtre_ratio =input$filtre_ratio,
                                    show_negative=input$show_negative)})

    output$Watervalues <- renderPlot(watervalues())


    #Plot Bellman page

    output$plot_Bellman <- renderPlot(plot_Bellman(rv$results,input$week_id,
                                                   input$param,states_step_ratio =
                                                   1/input$states_step_ratio))

    #plot reward page

    observeEvent( input$import_reward,
                  { showModal(modalDialog("Importing....", footer=NULL))
                    reward_dt <- get_Reward(simulation_res$simulation_names,
                                            district_name =input$district_name_rew,
                                            opts)
                    rv$reward_dt <- reward_dt
                    removeModal()

                  }
                )


    output$rewardplot <- renderPlot(

      if(input$param_rew=="r")
      {plot_reward(rv$reward_dt,input$week_id_rew,input$simulation_name_pattern)
      }else{
        if(input$param_rew=="rv")
          {plot_reward_variation(rv$reward_dt,input$week_id_rew,
                                   input$simulation_name_pattern)
        }else{

        if(input$param_rew=="r1")
          {plot_reward_mc(rv$reward_dt,input$week_id_rew,
                          input$Mc_year,input$simulation_name_pattern)
        }else{

          if(input$param_rew=="rv1")
          {plot_reward_variation_mc(rv$reward_dt,input$week_id_rew,
                              input$Mc_year,input$simulation_name_pattern)}
        }
        }
        }

       ) # end rewardplot





}









#------Run-----
shinyApp(ui = ui, server = server)
}
