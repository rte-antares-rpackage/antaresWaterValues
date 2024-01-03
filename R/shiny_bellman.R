bellmanUI <- function(id,opts) {
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::h1("Bellman plot"),
      shiny::sliderInput(
        NS(id,"week_id"),
        "Week to show",
        value = c(2, 2),
        min = 1,
        max = 52
      ),

      shinyBS::bsTooltip(
        NS(id,"week_id"),
        "The number of the week you want to plot (Bellman values are plotted for the end of the week and watervalues for the beginning of the week)",
        "bottom"
      ),


    ),
    #siderbarpanel
    shiny::mainPanel(
      shiny::plotOutput(NS(id,"plot_Bellman")),
      shinyWidgets::downloadBttn(
        outputId = NS(id,"download_Bellman_plot"),
        style = "unite",
        color = "primary",
        block = T
      ),

    )
  )
  # Siderbar

}

bellmanServer <- function(id, opts, watervalues, pen_high, pen_low,
                          force_final_level,penalty_final_level) {
  moduleServer(id, function(input, output, session) {
    plot <- reactive(plot_Bellman(
      watervalues(),
      input$week_id[1]:input$week_id[2],
      pen_low(),
      pen_high(),
      force_final_level(),
      penalty_final_level()
    ))

    output$plot_Bellman <-
      shiny::renderPlot(plot())

    output$download_Bellman_plot <- shiny::downloadHandler(
      filename = function() {
        paste('Bellman-', Sys.Date(), '.png', sep = '')
      },
      content = function(con) {
        grDevices::png(con , width = 1200,
                       height = 766)
        print(
          plot()
        )
        grDevices::dev.off()
      }
    )
  })
}
