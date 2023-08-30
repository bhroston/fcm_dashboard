
# app/view/dashboard_simulation_module/sim_mod_main_panel/sim_mod_main_panel_main.R

box::use(
  app/view/dashboard_simulation_module/sim_mod_main_panel/sim_mod_main_panel_timeseries,
  app/view/dashboard_simulation_module/sim_mod_main_panel/sim_mod_main_panel_influence_barchart,
  app/view/dashboard_simulation_module/sim_mod_main_panel/sim_mod_main_panel_data
)

box::use(
  shiny[...]
)

#' @export
ui <- function(id, data) {
  ns <- NS(id)

  shiny::mainPanel(
    shiny::tabsetPanel(
      sim_mod_main_panel_influence_barchart$ui(ns("influence")),
      sim_mod_main_panel_timeseries$ui(ns("timeseries")),
      sim_mod_main_panel_data$ui(ns("timeseries_data"))
    )
  )
}

#' @export
server <- function(id, data, sim_specs) {
  moduleServer(id, function(input, output, session) {
    sim_data <- sim_mod_main_panel_timeseries$server("timeseries", data, sim_specs)

    sim_mod_main_panel_influence_barchart$server("influence", sim_data, sim_specs)

    sim_mod_main_panel_data$server("timeseries_data", data, sim_data)

    return(sim_data)
  })
}
