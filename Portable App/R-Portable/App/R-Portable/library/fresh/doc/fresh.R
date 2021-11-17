## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup--------------------------------------------------------------------
#  library(fresh)

## -----------------------------------------------------------------------------
#  create_theme(
#    theme = "default",
#    bs_vars_button(
#      default_color = "#FFF",
#      default_bg = "#112446",
#      default_border = "#112446",
#      border_radius_base = "15px"
#    ),
#    bs_vars_wells(
#      bg = "#FFF",
#      border = "#112446"
#    )
#  )

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
knitr::include_graphics(path = "figures/example_shiny.png")

## -----------------------------------------------------------------------------
#  mytheme <- create_theme(
#    theme = "default",
#    bs_vars_navbar(
#      default_bg = "#75b8d1",
#      default_color = "#FFFFFF",
#      default_link_color = "#FFFFFF",
#      default_link_active_color = "#75b8d1",
#      default_link_active_bg = "#FFFFFF",
#      default_link_hover_color = "firebrick"
#    ),
#    output_file = NULL
#  )
#  
#  navbarPage(
#    title = "Custom navbar",
#    header = tagList(
#      use_theme(mytheme) # <-- use your theme
#    ),
#    tabPanel("Tab 1"),
#    tabPanel("Tab 2")
#  )

## -----------------------------------------------------------------------------
#  create_theme(
#    theme = "default",
#    ...,
#    output_file = "my-custom-theme.css"
#  )

