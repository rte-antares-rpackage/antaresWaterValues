## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  results = "hide"
)

## ----setup--------------------------------------------------------------------
library(fresh)

## -----------------------------------------------------------------------------
bs4dash_status(primary = "#5E81AC", danger = "#BF616A")

## -----------------------------------------------------------------------------
bs4dash_layout(main_bg = "#353c42")

## -----------------------------------------------------------------------------
bs4dash_color(gray_900 = "#FFF", white = "#272c30")

## -----------------------------------------------------------------------------
bs4dash_sidebar_light(
  bg = "#272c30",
  color = "#bec5cb",
  hover_color = "#FFF",
  submenu_bg = "#272c30", 
  submenu_color = "#FFF",
  submenu_hover_color = "#FFF"
)

## -----------------------------------------------------------------------------
bs4dash_status(light = "#272c30")

bs4dash_vars(
  navbar_light_color = "#bec5cb",
  navbar_light_active_color = "#FFF",
  navbar_light_hover_color = "#FFF"
)

## -----------------------------------------------------------------------------
bs4dash_yiq(contrasted_threshold = 10, text_dark = "#FFF", text_light = "#272c30")

## ---- eval=FALSE--------------------------------------------------------------
#  create_theme(
#    bs4dash_vars(
#      navbar_light_color = "#bec5cb",
#      navbar_light_active_color = "#FFF",
#      navbar_light_hover_color = "#FFF"
#    ),
#    bs4dash_yiq(
#      contrasted_threshold = 10,
#      text_dark = "#FFF",
#      text_light = "#272c30"
#    ),
#    bs4dash_layout(
#      main_bg = "#353c42"
#    ),
#    bs4dash_sidebar_light(
#      bg = "#272c30",
#      color = "#bec5cb",
#      hover_color = "#FFF",
#      submenu_bg = "#272c30",
#      submenu_color = "#FFF",
#      submenu_hover_color = "#FFF"
#    ),
#    bs4dash_status(
#      primary = "#5E81AC", danger = "#BF616A", light = "#272c30"
#    ),
#    bs4dash_color(
#      gray_900 = "#FFF"
#    )
#  )

