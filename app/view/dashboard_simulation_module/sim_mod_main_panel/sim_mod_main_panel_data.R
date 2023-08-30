
# app/view/dashboard_simulation_module/sim_mod_main_panel/sim_mod_main_panel_data.R

# load packages
box::use(
  shiny[...]
)

#' @export
ui <- function(id, data) {
  ns <- NS(id)

  shiny::tabPanel(
    "Data",
    shiny::downloadButton(ns("sim_data_download"), "Download Time-series Data"),
    shiny::tableOutput(ns("simulation_data"))
  )
}

#' @export
server <- function(id, data, sim_data) {
  moduleServer(id, function(input, output, session) {
    output$simulation_data <- renderTable(sim_data$fcm_simulation_results())

    output$sim_data_download <- shiny::downloadHandler(
      filename = function() {
        paste0("fcm_sim", Sys.Date(), ".csv")
      },
      content = function(file) {
        utils::write.csv(sim_data$fcm_simulation_results(), file, row.names = TRUE)
      }
    )
  })
}
