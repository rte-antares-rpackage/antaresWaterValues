## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(fresh)
library(shiny)
library(shinyWidgets)
library(htmltools)

## ---- echo=FALSE--------------------------------------------------------------
# tags$link(rel = "stylesheet", type = "text/css", href = "assets/bootstrap-default.min.css")
# tags$link(rel = "stylesheet", type = "text/css", href = "assets/bootstrap-custom.min.css")

## ----buttons------------------------------------------------------------------
buttons <- tagList(
  actionButton(
    "default", "Default"
  ),
  actionButton(
    "primary", "Primary", 
    class = "btn-primary"
  )
)

## ---- eval=FALSE--------------------------------------------------------------
#  bs_vars_button(
#    font_weight = 500,
#    border_radius_base = 0,
#    default_color = "#112446",
#    default_border = "#112446",
#    primary_color = "#FFF",
#    primary_bg = "#112446",
#    primary_border = "#112446"
#  )

## ---- echo=FALSE--------------------------------------------------------------
tags$div(
  class = "default-theme",
  buttons
)

## ---- echo=FALSE--------------------------------------------------------------
tags$div(
  class = "custom-theme",
  buttons
)

## ----colors-------------------------------------------------------------------
btn <- function(x)
  actionButton(
    x, x, 
    class = paste0("btn-", x)
  )

colors <- tagList(
  btn("primary"),
  btn("success"),
  btn("info"),
  btn("warning"),
  btn("danger"),
  tags$p(""),
  shinyWidgets::progressBar(
    "pb1", value = 80,
    status = "primary", 
    display_pct = TRUE
  )
)

## ---- eval=FALSE--------------------------------------------------------------
#  bs_vars_color(
#    brand_primary = "#112446",
#    brand_success = "#7bc043",
#    brand_info = "#0392cf",
#    brand_warning = "#f37736",
#    brand_danger = "#ee4035"
#  )

## ---- echo=FALSE--------------------------------------------------------------
htmlDependencies(colors[[length(colors)]]) <- NULL
tags$div(
  class = "default-theme",
  colors
)

## ---- echo=FALSE--------------------------------------------------------------
tags$div(
  class = "custom-theme",
  colors
)

## ----navbar-------------------------------------------------------------------
navbar <- navbarPage("App Title",
  tabPanel("Plot"),
  tabPanel("Summary"),
  tabPanel("Table")
)

## ---- eval=FALSE--------------------------------------------------------------
#  bs_vars_navbar(
#    padding_horizontal = "15px",
#    default_bg = "#112446",
#    default_color = "#FFFFFF",
#    default_link_color = "#FFFFFF",
#    default_link_active_color = "#FFFFFF",
#    default_link_hover_color = "#A4A4A4"
#  )

## ---- echo=FALSE--------------------------------------------------------------
tags$div(
  class = "default-theme",
  navbar
)

## ---- echo=FALSE--------------------------------------------------------------
tags$div(
  class = "custom-theme",
  navbar
)

## ----well---------------------------------------------------------------------
well <- wellPanel(
  "This is a wellPanel (or sidebarPanel)"
)

## ---- eval=FALSE--------------------------------------------------------------
#  bs_vars_wells(
#    bg = "#FFF",
#    border = "#3f2d54"
#  )

## ---- echo=FALSE--------------------------------------------------------------
tags$div(
  class = "default-theme",
  well
)

## ---- echo=FALSE--------------------------------------------------------------
tags$div(
  class = "custom-theme",
  well
)

## ----grid---------------------------------------------------------------------
grid <- tagList(
  fluidRow(
    tags$div(
      class = "col-sm-15", 
      wellPanel("width=15 - 2 columns")
    ),
    tags$div(
      class = "col-sm-15",
      wellPanel("width=15 - 2 columns")
    )
  ),
  fluidRow(
    tags$div(
      class = "col-sm-10", 
      wellPanel("width=10 - 3 columns")
    ),
    tags$div(
      class = "col-sm-10", 
      wellPanel("width=10 - 3 columns")
    ),
    tags$div(
      class = "col-sm-10", 
      wellPanel("width=10 - 3 columns")
    )
  ),
  fluidRow(
    column(width = 6, wellPanel("width=6 - 5 columns")),
    column(width = 6, wellPanel("width=6 - 5 columns")),
    column(width = 6, wellPanel("width=6 - 5 columns")),
    column(width = 6, wellPanel("width=6 - 5 columns")),
    column(width = 6, wellPanel("width=6 - 5 columns"))
  )
)

## ---- eval=FALSE--------------------------------------------------------------
#  bs_vars_global(
#    grid_columns = 30,
#    grid_gutter_width = "15px"
#  )

## ---- echo=FALSE--------------------------------------------------------------
tags$div(
  class = "default-theme",
  tagList(
    fluidRow(
      column(width = 6, wellPanel("width=6 - 2 columns")),
      column(width = 6, wellPanel("width=6 - 2 columns"))
    ),
    fluidRow(
      column(width = 4, wellPanel("width=4 - 3 columns")),
      column(width = 4, wellPanel("width=4 - 3 columns")),
      column(width = 4, wellPanel("width=4 - 3 columns"))
    )
  )
)

## ---- echo=FALSE--------------------------------------------------------------
tags$div(
  class = "custom-theme",
  grid
)

## ----tabs---------------------------------------------------------------------
tabs <- tabsetPanel(
  tabPanel("Plot"),
  tabPanel("Summary"),
  tabPanel("Table")
)

## ---- eval=FALSE--------------------------------------------------------------
#  bs_vars_tabs(
#    border_color = "#112446",
#    active_link_hover_bg = "#FFF",
#    active_link_hover_color = "#112446",
#    active_link_hover_border_color = "#112446",
#    link_hover_border_color = "#112446"
#  )

## ---- echo=FALSE--------------------------------------------------------------
tags$div(
  class = "default-theme",
  tabs
)

## ---- echo=FALSE--------------------------------------------------------------
tags$div(
  class = "custom-theme",
  tabs
)

## ----font---------------------------------------------------------------------
font <- tagList(
  tags$p("Normal text"),
  tags$b("Bold text"),
  tags$i("Italic text"),
  tags$h1("First level title"),
  tags$h2("Second level title"),
  tags$h3("Third level title"),
  tags$h4("Fourth level title"),
  tags$h5("Fifth level title"),
  tags$h6("Sixth level title")
)

## ---- eval=FALSE--------------------------------------------------------------
#  bs_vars_font(
#    size_base = "12px"
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  bs_vars_modal(
#    md = "80%",backdrop_bg = "#112446",
#    backdrop_opacity = 1,
#    header_border_color = "#112446",
#    footer_border_color = "#112446"
#  )

