
# app/view/dashboard_simulation_module/sim_mod_main_panel/sim_mod_main_panel_timeseries.R

# load custom functions
box::use(
  app/logic/simulate_fcm[simulate_fcm],
  app/logic/sim_math_funs[...],
  app/logic/sim_utility_funs[...],
)

# load packages
box::use(
  shiny[...],
  ggplot2[...],
  plotly[...]
)

#' @export
ui <- function(id, data) {
  ns <- NS(id)

  shiny::tabPanel(
    "Time-Series",
    plotly::plotlyOutput(ns("simulation_plot"))
  )
}

#' @export
server <- function(id, data, sim_specs) {
  moduleServer(id, function(input, output, session) {

    initial_state_vector <- reactive({
      state_vector <- rep(0, length(data()$fcm$concepts))
      names(state_vector) <- data()$fcm$concepts
      node_to_activate_index <- data()$fcm$concepts == sim_specs$node_to_activate()
      state_vector[node_to_activate_index] <- 1
      return(state_vector)
    })

    format_fcm_adj_matrix <- reactive({
      adj_matrix <- data()$fcm$adj_matrix
      adj_matrix[col(adj_matrix) == row(adj_matrix)] <- as.numeric(sim_specs$self_connection_value())

      edge_simplification <- sim_specs$simplify_edges_selection()
      if (edge_simplification == "OFF") {
        format_adj_matrix <- adj_matrix
      } else if (edge_simplification == "Bivalent [0, 1]") {
        format_adj_matrix <- adj_matrix
        format_adj_matrix[format_adj_matrix != 0] <- 1
      } else if (edge_simplification == "Trivalent [-1, 0, 1]") {
        format_adj_matrix <- adj_matrix
        format_adj_matrix[format_adj_matrix > 0] <- 1
        format_adj_matrix[format_adj_matrix < 0] <- -1
      }
      return(format_adj_matrix)
    })

    fcm_simulation_results <- reactive({
      simulate_fcm(
        fcm_adj_matrix = format_fcm_adj_matrix(),
        initial_state_vector = initial_state_vector(),
        f_squashing = sim_specs$squashing_function(),
        lambda = sim_specs$lambda(),
        f_activation = sim_specs$activation_function(),
        iter = sim_specs$iterations()
      )
    })

    output$simulation_plot <- plotly::renderPlotly({
      fcm_simulation_data_longer <- elongate_fcm_simulation_df(fcm_simulation_results())

      fig <- plotly::plot_ly()
      fig <- fig %>%
        add_trace(
          type = 'scatter',
          mode = 'lines',
          x = fcm_simulation_data_longer$iteration,
          y = fcm_simulation_data_longer$value,
          color = fcm_simulation_data_longer$concept,
          hovertemplate = paste('<b>%{y:.2f}</b>')
        ) %>%
        layout(legend = list(orientation = 'h'))

      return(fig)
    })

    return(
      list(
        reformat_fcm_adj_matrix = reactive({format_fcm_adj_matrix()}),
        fcm_simulation_results = reactive({fcm_simulation_results()})
      )
    )
  })
}
