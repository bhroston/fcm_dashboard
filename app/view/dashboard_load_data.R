# app/view/dashboard_load_data.R

#' Variables
#'  fcm_adj_matrix
#'  fcm
#'  nodes
#'  edges

box::use(
  shiny[...],
  dplyr[...]
)
box::use(
  app/logic/load_data[load_adj_matrix_from_file],
  app/logic/fcm_class[...],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  shiny::fluidRow(
    #column(width = 1),
    column(
      width = 5,
      offset = 1,
      shiny::fileInput(
        ns("fcm_adj_matrix_file"),
        label = "Choose a .csv or .xlsx file to upload",
        accept = c(
          "text/csv",
          "text/comma-seperated-values",
          ".csv",
          "text/xslx",
          ".xlsx"
        ))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
      fcm_adj_matrix_and_coords <- reactive({
        shiny::req(input$fcm_adj_matrix_file)

        file <- input$fcm_adj_matrix_file
        raw_adj_matrix_and_coords <- load_adj_matrix_from_file(file$datapath)
        return(raw_adj_matrix_and_coords)
      })


      fcm <- reactive({
        shiny::req(input$fcm_adj_matrix_file)

        fcm_object <- fcm_from_adj_matrix(fcm_adj_matrix_and_coords()$adj_matrix)
        return(fcm_object)
      })


      coords <- reactive({
        shiny::req(input$fcm_adj_matrix_file)

        raw_coords <- fcm_adj_matrix_and_coords()$coords

        nodes_included <- fcm()$concepts
        updated_coords <- raw_coords[raw_coords$id %in% nodes_included,]
        return(updated_coords)
      })

      nodes <- reactive({
        nodes_df <- data.frame(
          "id" = fcm()$concepts,
          "label" = fcm()$concepts
        ) %>%
          dplyr::arrange(id) %>%
          dplyr::mutate(color = dplyr::case_when(
            substr(id, 1, 1) == "F" ~ "#4CA6A6",
            substr(id, 1, 1) == "C" ~ "#4D4D4B",
            substr(id, 1, 1) == "A" ~ "#A6A6A6",
            substr(id, 1, 1) == "S" ~ "#4C4DA6",
            TRUE ~ "blue"
          ))

        if (!identical(fcm_adj_matrix_and_coords()$coords, NA)) {
          nodes_df$x <- coords()$x
          nodes_df$y <- coords()$y
        }

        return(nodes_df)
      })


      edges <- reactive({
        fcm_edgelist <- fcm()$edgelist
        names(fcm_edgelist) <- c("from", "to", "weight")
        edges_df <- fcm_edgelist %>%
          # Arrows point toward the node in the "to" column
          dplyr::mutate(arrows = "to")
        return(edges_df)
      })

      # object to return
      reactive({
        list(
          "fcm_adj_matrix" = fcm()$adj_matrix,
          "fcm" = fcm(),
          "nodes" = nodes(),
          "edges" = edges(),
          "data_input" = input$fcm_adj_matrix_file
        )
      })
  })
}
