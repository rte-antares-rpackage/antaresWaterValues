## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  results = "hide"
)

## ----setup--------------------------------------------------------------------
library(fresh)

## -----------------------------------------------------------------------------
adminlte_color(
  light_blue = "#086A87"
)

## -----------------------------------------------------------------------------
adminlte_sidebar(
  width = "400px",
  dark_bg = "#D8DEE9",
  dark_hover_bg = "#81A1C1",
  dark_color = "#2E3440"
)

## -----------------------------------------------------------------------------
adminlte_global(
  content_bg = "#FFFFFF",
  box_bg = "#D8DEE9", 
  info_box_bg = "#D8DEE9"
)

## ---- eval=FALSE--------------------------------------------------------------
#  library(fresh)
#  # Create the theme
#  mytheme <- create_theme(
#    adminlte_color(
#      light_blue = "#434C5E"
#    ),
#    adminlte_sidebar(
#      width = "400px",
#      dark_bg = "#D8DEE9",
#      dark_hover_bg = "#81A1C1",
#      dark_color = "#2E3440"
#    ),
#    adminlte_global(
#      content_bg = "#FFF",
#      box_bg = "#D8DEE9",
#      info_box_bg = "#D8DEE9"
#    )
#  )
#  
#  
#  library(shiny)
#  library(shinydashboard)
#  
#  ui <- dashboardPage(
#    header = dashboardHeader(title = "My dashboard"),
#    sidebar = dashboardSidebar(
#      sidebarMenu(
#        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
#        menuItem("Settings", tabName = "settings", icon = icon("sliders"))
#      )
#    ),
#    body = dashboardBody(
#  
#      use_theme(mytheme), # <-- use the theme
#  
#      tabItems(
#        tabItem(
#          "dashboard",
#  
#          # infoBoxes
#          fluidRow(
#            infoBox(
#              "Orders", "123", "Subtitle", icon = icon("credit-card"),
#              color = "light-blue"
#            ),
#            valueBox(
#              5846, "New Orders", icon = icon("credit-card"),
#              color = "light-blue"
#            ),
#            box(
#              title = "A box", solidHeader = TRUE, width = 4,
#              status = "primary",
#              "Content of the box"
#            )
#          )
#        )
#      )
#    )
#  )
#  
#  server <- function(input, output, session) {
#  
#  }
#  
#  shinyApp(ui, server)

