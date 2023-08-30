box::use(
  shiny[...],
)

box::use(
  app/view/dashboard_load_data,
  app/view/dashboard_network_plot,
  app/view/dashboard_simulation_module/simulation_module_main,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  bootstrapPage(
    dashboard_load_data$ui(ns("data")),
    shiny::tabsetPanel(
      # Graph
      dashboard_network_plot$ui(ns("network_plot")),
      # Simulate
      simulation_module_main$ui(ns("simulation"))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- dashboard_load_data$server("data")

    dashboard_network_plot$server("network_plot", data)

    simulation_module_main$server("simulation", data)
  })
}
