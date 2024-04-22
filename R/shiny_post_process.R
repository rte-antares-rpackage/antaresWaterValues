postProcessUI <- function(id,opts) {
  shiny::sidebarLayout(
    fluid = TRUE,

    shiny::sidebarPanel(
      shiny::h1("Post process"),
      shiny::h3("Remove extreme values"),
      shinyWidgets::switchInput(
        NS(id,"Run_remove_out"),
        value = F,
        offStatus = "danger",
        onStatus = "success"
      ) %>%
        bsplus::shinyInput_label_embed(
          bsplus::shiny_iconlink() %>%
            bsplus::bs_embed_popover(title = "Activate filter to remove extreme values")
        ),





      shiny::conditionalPanel(
        ns = NS(id),
        condition = "input.Run_remove_out",
        shiny::numericInput(
          inputId = NS(id,"min_rm"),
          label = "Minimum value to keep",
          value = 0,
          step = 1,
          width = '100%'
        ),
        shiny::numericInput(
          inputId = NS(id,"max_rm"),
          label = "Maximum value to keep",
          value = 200,
          step = 1,
          width = '100%'
        ),

        shiny::h3("Replace not defined values"),

        shinyWidgets::radioGroupButtons(
          inputId = NS(id,"replace_na_method"),
          label = "Method to replace NaN values",
          choices = c("Constant values", "Extreme values"),
          individual = TRUE,
          justified = TRUE
        ),

        shiny::conditionalPanel(
          ns = NS(id),
          condition = "input.replace_na_method=='Constant values'",
          shiny::numericInput(NS(id,"max_vu"), "Max Water value", value =
                                200),
          shinyBS::bsTooltip(
            NS(id,"max_vu"),
            "The highest water value that you want to affect",
            "bottom"
          ),
          shiny::numericInput(NS(id,"min_vu"), "Min Water value", value =
                                0),
          shinyBS::bsTooltip(
            NS(id,"min_vu"),
            "The smallest water value that you want to affect",
            "bottom"
          ),
        ),
      ),
      shinyBS::bsTooltip(
        NS(id,"min_rm"),
        "Delete all water values that are under this value",
        "bottom"
      ),
      shinyBS::bsTooltip(
        NS(id,"max_rm"),
        "Delete all water values that are bigger then this value",
        "bottom"
      ),





      shiny::tags$button(
        id = NS(id,"to_antares"),
        type = "button",
        class = "btn action-button btn-large btn-primary",
        shiny::img(
          src = "https://antares-simulator.org/static/img/antares-256.png",
          height = "50px",
          shiny::HTML('<i class="icon-star"></i>To Antares')
        )
      ),
      shinyBS::bsTooltip(
        NS(id,"to_antares"),
        "convert the water values to antares format than implemented in the desired area",
        "bottom"
      ),

      shinyWidgets::downloadBttn(
        outputId = NS(id,"download_vu"),
        style = "unite",
        color = "primary",
        block = T
      )

    ),
    #sidebarPanel




    shiny::mainPanel(
      shinycustomloader::withLoader(
        shiny::plotOutput(NS(id,"post_process")),
        type = "html",
        loader = "dnaspin"
      ),
      shinyWidgets::downloadBttn(
        outputId = NS(id,"download_pp_plot"),
        style = "unite",
        color = "primary",
        block = T
      ),

    )
  ) #sidebarLayout


}

postProcessServer <- function(id, opts, watervalues, area) {
  moduleServer(id, function(input, output, session) {

    final_result <- shiny::reactive({
      if (input$Run_remove_out) {
        remove_out(
          watervalues(),
          min = input$min_rm,
          max = input$max_rm,
          replace_na_method = input$replace_na_method,
          max_vu = input$max_vu,
          min_vu = input$min_vu
        )
      } else{
        watervalues()
      }
    })



    output$post_process <-
      shiny::renderPlot(waterValuesViz(final_result()))

    output$download_pp_plot <- shiny::downloadHandler(
      filename = function() {
        paste('full water values-', Sys.Date(), '.png', sep = '')
      },
      content = function(con) {
        grDevices::png(con , width = 1200,
                       height = 766)
        print(waterValuesViz(final_result()))
        grDevices::dev.off()
      }
    )

    shiny::observeEvent(input$to_antares, {
      results <- final_result()
      reshaped_values <- to_Antares_Format(results)
      antaresEditObject::writeWaterValues(area = area(),
                                          data = reshaped_values)
      shinyWidgets::show_alert(title = "Implement water values in Antares",
                               text = " Done !!",
                               type = "success")
    })

    vu <- shiny::reactiveValues()
    shiny::observe({
      if (!is.null(final_result()))
        shiny::isolate(vu <<- final_result())
    })

    output$download_vu <- shiny::downloadHandler(
      filename <- function() {
        paste('Watervalues-', Sys.Date(), '.Rdata', sep = '')
      },

      content = function(file) {
        save(vu, file = file)
      }
    )

  })
}
