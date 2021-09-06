# launch this scripts to install all dependencies of watervalues package ----
to_install <- c("antaresEditObject", "antaresRead", "assertthat", "cowplot", "data.table", "dplyr", "ggplot2", "mice", "periscope", "shiny", "shinybusy", "shinycustomloader", "shinyjs", "shinythemes", "shinyWidgets", "spsComps", "stringr", "tibble", "tidyr", "usethis", "viridis", "zoo")
  for (i in to_install) {
    message(paste("looking for ", i))
    if (!requireNamespace(i)) {
      message(paste("     installing", i))
      install.packages(i)
    }
  }
