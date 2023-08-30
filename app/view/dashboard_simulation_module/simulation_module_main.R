
# app/view/dashboard_simulation_module/simulation_module_main.R

# load custom modules
box::use(
  app/view/dashboard_simulation_module/sim_mod_sidebar_panel,
  app/view/dashboard_simulation_module/sim_mod_main_panel/sim_mod_main_panel_main
)

# load custom functions
box::use(
  app/logic/simulate_fcm[simulate_fcm],
  app/logic/sim_utility_funs[...],
)

# load packages
box::use(
  shiny[...],
  ggplot2[...],
)

#' @export
ui <- function(id, data) {
  ns <- NS(id)

  shiny::tabPanel(
    "Simulation",

    # Sidebar Panel UI
    sim_mod_sidebar_panel$ui(ns("sidebar")),

    # Main Panel Module
    sim_mod_main_panel_main$ui(ns("main_panel"))
  )
}

#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    simulation_specs <- sim_mod_sidebar_panel$server("sidebar", data, simulation_data)

    simulation_data <- sim_mod_main_panel_main$server("main_panel", data, simulation_specs)
  })
}
