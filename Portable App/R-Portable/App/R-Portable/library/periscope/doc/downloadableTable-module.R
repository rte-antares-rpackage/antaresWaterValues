## ---- eval=F------------------------------------------------------------------
#  # Inside ui_body.R or ui_sidebar.R
#  
#  downloadableTableUI("object_id1",
#                      downloadtypes = c("csv", "tsv"),
#                      hovertext = "Download the data here!",
#                      contentHeight = "300px",
#                      singleSelect = FALSE)

## ---- eval = F----------------------------------------------------------------
#  # Inside server_local.R
#  sketch <- htmltools::withTags(table(
#      class = "display",
#      thead(
#          tr(
#              th(rowspan = 2, "Location"),
#              th(colspan = 2, "Statistics")
#          ),
#          tr(
#              th("Change"),
#              th("Increase")
#          )
#  
#      )
#  ))
#  
#  selectedrows <- downloadableTable("exampleDT1",
#                                    ss_userAction.Log,
#                                    "exampletable",
#                                    list(csv = load_data3, tsv = load_data3),
#                                    load_data3,
#                                    colnames = c("Area", "Delta", "Increase"),
#                                    filter = "bottom",
#                                    callback = htmlwidgets::JS("table.order([1, 'asc']).draw();"),
#                                    container = sketch,
#                                    formatStyle = list(columns = c("Total.Population.Change"),
#                                                       color = DT::styleInterval(0, c("red", "green"))),
#                                    formatStyle = list(columns = c("Natural.Increase"),
#                                                       backgroundColor = DT::styleInterval(
#                                                           c(7614, 15914, 34152),
#                                                           c("blue", "lightblue", "#FF7F7F", "red"))))
#  
#  # NOTE: selectedrows is the reactive return value, captured for later use

## ---- eval=F------------------------------------------------------------------
#  library(periscope)
#  
#  app_dir = tempdir()
#  create_new_application('mysampleapp', location = app_dir, sampleapp = TRUE)
#  runApp(paste(app_dir, 'mysampleapp', sep = .Platform$file.sep))

