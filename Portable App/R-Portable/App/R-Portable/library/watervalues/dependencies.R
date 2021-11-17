# launch this scripts to install all dependencies of watervalues package ----
to_install <- c("antaresEditObject", "assertthat", "bsplus", "cowplot",
                "data.table", "doParallel", "dplyr", "DT", "foreach",
                "ggplot2", "mice", "parallelly", "periscope", "shiny",
                "shinyBS", "shinybusy", "shinycustomloader", "shinyjs",
                "shinythemes", "shinyWidgets", "spsComps", "stringr",
                "tibble", "tidyr", "usethis", "viridis", "zoo")


for (i in to_install) {
    message(paste("looking for ", i))
    if (!requireNamespace(i)) {
      message(paste("     installing", i))
      install.packages(i)
    }
  }

devtools::install_github("rte-antares-rpackage/antaresRead")
