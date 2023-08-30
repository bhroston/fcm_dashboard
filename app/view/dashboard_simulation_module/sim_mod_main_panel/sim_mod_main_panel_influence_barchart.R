
# app/view/dashboard_simulation_module/sim_mod_main_panel/sim_mod_main_panel_influence_barcharts.R

# load custom functions
box::use(
  app/logic/influence_utility_funs[...],
  app/logic/lambda_math_funs[...]
)

# load packages
box::use(
  shiny[...],
  ggplot2[...],
  shinyjs[...],
)

#' @export
ui <- function(id, reformat_data) {
  shinyjs::useShinyjs()
  ns <- NS(id)

  shiny::tabPanel(
    "Influence",
    shiny::column(
      width = 4,
      shiny::selectInput(ns("signal_selection"), label = "Signal:", choices = c("Peak", "First Signal"), selected = "Peak")
    ),
    shiny::column(
      width = 8,
      shiny::checkboxInput(ns("normalize_signals"), label = HTML(paste("Normalize Values", "<br/>", "(only for calculated lambda)")), value = NULL)
    ),
    shiny::plotOutput(ns("influence_barchart"))
  )
}

#' @export
server <- function(id, sim_data, sim_specs) {
  moduleServer(id, function(input, output, session) {
    # Create bar chart similar to Mental Modeler
    timeseries_signals <- reactive({
      signals <- get_influenced_concept_signals_df(sim_data$fcm_simulation_results(), input$signal_selection)

      error_from_max_lambda <- abs(sim_specs$lambda() - sim_specs$max_lambda())/sim_specs$max_lambda()
      if (error_from_max_lambda < 0.001) {
        if (input$normalize_signals == TRUE) {
          signals$signal <- get_normalized_concept_values(signals$signal)
        }
      }

      return(signals)
    })


    output$influence_barchart <- shiny::renderPlot({
      if (input$normalize_signals == TRUE) {
        signals <- as.numeric(timeseries_signals()$signal)
        signals[signals > 1] <- 1

        timeseries_signals_df <- data.frame(
          "concept" = names(timeseries_signals()$signal),
          "signal" = signals,
          "sign" = timeseries_signals()$sign
        )
      } else {
        timeseries_signals_df <- timeseries_signals()
      }

      ggplot(timeseries_signals_df, aes(x = signal, y = concept)) +
        geom_col(aes(fill = sign),  color = "black", size = 0.25) +
        scale_fill_manual(values = c("activated" = "#eeeeee", "positive" = "#CCEEFF", "negative" = "#FFDDDD"), guide = FALSE) +
        #scale_x_continuous(expand = c(0, 0)) +
        geom_text(aes(x = signal + 0.055*sign(signal), label = round(signal, digits = 2))) +
        theme_classic(base_size = 15) +
        theme(
          legend.position = "none",
          axis.title.y = element_blank()
        )
    })
  })
}
