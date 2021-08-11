

# library(shiny)
library(shinythemes)

#Shiny_Grid_matrix <- function(simulation_res,opts)

#
# simulation_res <- simulation_res_se
# opts







#------User interface-----

ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = shinytheme("cerulean"),
  titlePanel("Calculate Water Values"),

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

  actionButton("plot","Show"),

  checkboxInput("show_negative","Show negative Water values",
                value=T),


  mainPanel(
    plotOutput("Watervalues")

),

)

#------Server functions ------
server <- function(input, output) {


    rv <- reactiveValues()
    observeEvent( input$Calculate,
    { showModal(modalDialog("Calculating....", footer=NULL))
      results <-     Grid_Matrix(
        area = input$Area,
        simulation_names = simulation_res_se$simulation_names,
        simulation_values = simulation_res_se$simulation_values,
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

    watervalues <- eventReactive(input$plot,{waterValuesViz(rv$results,
                                    show_negative=input$show_negative)})

    output$Watervalues <- renderPlot(watervalues())









}









#------Run-----
shinyApp(ui = ui, server = server)