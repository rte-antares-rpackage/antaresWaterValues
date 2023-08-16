# launch this scripts to install all dependencies of watervalues package ----
to_install <- c("antaresEditObject", "assertthat",
                "data.table", "dplyr",
                "ggplot2", "stringr",
                 "tidyr", "viridis","shiny","shinyBS")


for (i in to_install) {
    message(paste("looking for ", i))
    if (!requireNamespace(i)) {
      message(paste("     installing", i))
      install.packages(i)
    }
  }

devtools::install_github("rte-antares-rpackage/antaresRead")
