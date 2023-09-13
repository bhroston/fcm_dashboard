
# app/view/dashboard_network_plot.R

box::use(
  shiny[...],
  visNetwork[...],
  dplyr[...],
  shinyWidgets[...],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  shiny::tabPanel(
    "Network Plot",
    shiny::column(
      width = 2, offset = 0,
      dropdownButton(
        shiny::numericInput(
          ns("network_plot_scale"),
          label = "Scale Plot by:",
          value = 2000,
          step = 1
        ),
        circle = TRUE, icon = icon("gear"), width = "300px", size = "xs"
      )
    ),
    visNetwork::visNetworkOutput(ns("fcm_network_proxy"))
  )

  #visNetwork::visNetworkOutput(ns("fcm_network_proxy"))

   # shiny::tabPanel(
   #  "Network Plot",
   #  # Sidebar Panel
   #  shiny::sidebarPanel(
   #    width = 2,
   #    dropdownButton(
   #      shiny::numericInput(
   #        ns("network_plot_scale"),
   #        label = "Scale Plot by:",
   #        value = 2000,
   #        step = 1
   #      ),
   #
   #      circle = TRUE, icon = icon("gear"), width = "300px"
   #    )
   #  ),
   #  #   shiny::numericInput(
   #  #     ns("network_plot_scale"),
   #  #     label = "Scale Plot by:",
   #  #     value = 2000,
   #  #     step = 1),
   #  #   p("FCM tends to plot off-screen. Drag up and left-ward to see it.")
   #  # ),
   #  shiny::mainPanel(
   #    visNetwork::visNetworkOutput(ns("fcm_network_proxy"))
   #  )
  # )
}


#' @export
server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    nodes <- reactive({
      if ("x" %in% colnames(data()$nodes)) {
        data()$nodes %>%
          mutate(x = x*as.numeric(input$network_plot_scale)) %>%
          mutate(y = y*as.numeric(input$network_plot_scale))
      } else {
        data()$nodes
      }
    })

    output$fcm_network_proxy <- visNetwork::renderVisNetwork({
      visNetwork::visNetwork(nodes(), data()$edges) %>%
        visNetwork::visNodes(scaling = list(min = 24, max = 26), font = list(size = 16, face = "Times"), physics = FALSE, opacity = 0.7) %>%
        visNetwork::visEdges(color = "black", physics = FALSE) %>%
        visNetwork::visOptions(width = "150%", height = "150%", manipulation = TRUE, nodesIdSelection = TRUE, highlightNearest = list(enabled = TRUE, degree = 0, labelOnly = TRUE))
    })
  })
}
