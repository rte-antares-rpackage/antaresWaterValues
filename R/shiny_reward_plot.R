rewardUI <- function(id,opts) {
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::h1("Rewards plot"),

      shiny::sliderInput(
        NS(id,"week_id_rew"),
        "Week to show",
        value = c(2, 2),
        min = 1,
        max = 52
      ),

      shinyBS::bsTooltip(NS(id,"week_id_rew"), "The weeks you want to plot",
                         "bottom"),

      shiny::selectInput(
        NS(id,"param_rew"),
        "Type",
        c(
          "Reward" = "r",
          "Reward 1 MC" = "r1",
          "Reward variation" = "rv",
          "reward variation 1Mc" =
            "rv1"
        )
      ),

      shiny::conditionalPanel(
        ns = NS(id),
        condition = "['rv1','r1'].includes(input.param_rew)",
        shiny::sliderInput(
          NS(id,"Mc_year"),
          label = "Monte-Carlo year",
          min = 1,
          max = opts$parameters$general$nbyears,
          value = c(1, 1)
          ,
          step = 1
        )
      ),


      shinyBS::bsTooltip(NS(id,"Mc_year"), "The scenarios that you want to plot",
                         "bottom"),

      shinyWidgets::downloadBttn(
        outputId = NS(id,"download_reward_base"),
        style = "unite",
        color = "primary",
        block = TRUE
      )
    ),

    shiny::mainPanel(
      shiny::plotOutput(NS(id,"rewardplot")),
      shinyWidgets::downloadBttn(
        outputId = NS(id,"download_reward_plot"),
        style = "unite",
        color = "primary",
        block = TRUE
      ),

      DT::dataTableOutput(NS(id,"reward_table"))
    )
  )
}

rewardServer <- function(id, opts, reward_db) {
  moduleServer(id, function(input, output, session) {


    rewardplot <- shiny::reactive({

      week_id_rew <- input$week_id_rew[1]:input$week_id_rew[2]
      Mc_year <- input$Mc_year[1]:input$Mc_year[2]
      if (input$param_rew == "r")
      {
        plot_reward(reward_db()$reward, week_id_rew)
      } else{
        if (input$param_rew == "rv")
        {
          plot_reward_variation(reward_db()$reward, week_id_rew)
        } else{
          if (input$param_rew == "r1")
          {
            plot_reward_mc(reward_db()$reward, week_id_rew,
                           Mc_year)
          } else{
            if (input$param_rew == "rv1")
            {
              plot_reward_variation_mc(reward_db()$reward, week_id_rew,
                                       Mc_year)
            }
          }
        }
      }
    })

    output$rewardplot <- shiny::renderPlot(rewardplot()$graph)
    output$reward_table <- DT::renderDataTable(rewardplot()$table)
    output$download_reward_plot <- shiny::downloadHandler(
      filename = function() {
        paste('Reward-', Sys.Date(), '.png', sep = '')
      },
      content = function(con) {
        grDevices::png(con , width = 1200, height = 766)
        print(rewardplot()$graph)
        grDevices::dev.off()
      }
    )

    reward_base <- shiny::reactiveValues()
    shiny::observe({
      if (!is.null(reward_db()))
        shiny::isolate(reward_base <<- reward_db()$reward)
    })

    output$download_reward_base <- shiny::downloadHandler(
      filename <- function() {
        paste('Reward-Base-', Sys.Date(), '.Rdata', sep = '')
      },

      content = function(file) {
        save(reward_base, file = file)
      }
    )




  })
}
