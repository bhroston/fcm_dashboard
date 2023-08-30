
# app/view/dashboard_simulation_module/sim_mod_sidebar_panel.R

# load custom functions
box::use(
  app/logic/lambda_math_funs[...]
)

# load packages
box::use(
  shiny[...]
)

#' @export
ui <- function(id, data) { # needs to get passed a calculation
  ns <- NS(id)

  shiny::sidebarPanel(
    width = 4,
    shiny::selectInput(ns("node_to_activate"), "Select concept to activate", choices = "", selected = "Select Concept"),
    shiny::withMathJax(shiny::uiOutput(ns("f_next_state_vector"))),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::selectInput(ns("squashing_function"), "Squashing function", choices = c("tanh", "sigmoid", "bivalent", "trivalent"), selected = "tanh")
      ),
      shiny::column(
        width = 5,
        shiny::numericInput(ns("lambda"), "Lambda", min = 0, max = 1, value = 0.5, step = 0.1)
      )
    ),
    shiny::withMathJax(shiny::uiOutput(ns("f_squashing_formula"))),
    shiny::fluidRow(
      #shiny::column(
        #width = 5,
        shiny::selectInput(ns("activation_function"), "Activation function", choices = c("Kosko (k)", "Modified-Kosko (mk)", "Rescale (r)"), selected = "Kosko (k)")
        #),
      # shiny::column(
      #   width = 3,
      #   shiny::numericInput(ns("k1"), "K1", min = 0, max = 1, value = 1, step = 0.1)
      # ),
      # shiny::column(
      #   width = 3,
      #   shiny::numericInput(ns("k2"), "K2", min = 0, max = 1, value = 1, step = 0.1)
      # )
    ),
    shiny::withMathJax(shiny::uiOutput(ns("f_activation_formula"))),
    shiny::selectInput(ns("simplify_edges_selection"), "Simplify Edge Weights", choices = c("OFF", "Bivalent [0, 1]", "Trivalent [-1, 0, 1]"), selected = "OFF"),
    shiny::selectInput(ns("diagonal_values"), "Concept Self-Connection Value", choices = c(0, 1), selected = 0),
    shiny::sliderInput(ns("iterations"), "Number of iterations", min = 10, max = 100, value = 50)
  )
}

#' @export
server <- function(id, data, simulation_data) {
  moduleServer(id, function(input, output, session) {
    # Update node_to_activate selection once an FCM has been loaded
    observeEvent(data()$data_input, {
      updateSelectInput(
        session,
        "node_to_activate",
        choices = data()$nodes$id
      )
    })

    output$f_next_state_vector <- shiny::renderUI({
      formula <- "$$
      \\begin{equation*}
      \\mathbf{V}_{i+1} =f_{squashing}(f_{activation}(\\mathbf{V}_{i} ,\ \\mathbf{A}))
      \\end{equation*}
      $$"

      shiny::withMathJax(tags$p(formula))
    })

    output$f_squashing_formula <- shiny::renderUI({
      chosen_function <- input$squashing_function
      if (chosen_function == "tanh") {
        formula <- "$$
        \\begin{equation*}
        f_{squashing} =\ \\frac{e^{\\lambda \\mathbf{V}_{i}} -e^{-\\lambda \\mathbf{V}_{i}}}{e^{\\lambda \\mathbf{V}_{i}} +e^{-\\lambda \\mathbf{V}_{i}}}
        \\end{equation*}
        $$"
      } else if (chosen_function == "sigmoid") {
        formula <- "$$
        \\begin{equation*}
        f_{squashing} =\ \\frac{1}{e^{-\\lambda \\mathbf{V}_{i}}}
        \\end{equation*}
        $$"
      } else if (chosen_function == "bivalent") {
        formula <- "$$
        \\begin{equation*}
        f_{squashing} =\ \\begin{cases}
        0 \\ if\ \\mathbf{V}_{i_{node}} \ =\\ 0\\\\
        1 \\ if\ \\mathbf{V}_{i_{node}} \ \\neq \ 0
        \\end{cases}
        \\end{equation*}
        $$"
      } else if (chosen_function == "trivalent") {
        formula <- "$$
        \\begin{equation*}
        f_{squashing} =\ \\begin{cases}
        0 \\ if\ \\mathbf{V}_{i_{node}} \ =\\ 0\\\\
        1 \\ if\ \\mathbf{V}_{i_{node}} \ >\\ \ 0\\\\
        -1 \\ if\ \\mathbf{V}_{i_{node}} \ <\\ \ 0
        \\end{cases}
        \\end{equation*}
        $$"
      }
      shiny::withMathJax(tags$p(formula))
    })

    output$f_activation_formula <- shiny::renderUI({
      activation_fun <- input$activation_function
      if (activation_fun == "Kosko (k)") {
        formula <- "$$
          \\begin{gather}
          \\sum _{\\mathrm{j=1;\ i\\neq j}}^{\\mathrm{M}}\\mathrm{V_{j}^{( t)} w_{ji}} \\hspace{0.2cm} \\mathrm{or} \\hspace{0.2cm} \\mathbf{V}_{i} *\\mathbf{A}
          \\end{gather}
          $$"
      } else if (activation_fun == "Modified-Kosko (mk)") {
        formula <- "$$
          \\begin{gather}
          \\sum _{\\mathrm{j=1;\ i\\neq j}}^{\\mathrm{M}}\\mathrm{V_{j}^{( t)} w_{ji}} +\\mathrm{V_{i}^{( t)}} \\newline
          or \\newline \\newline
          \\mathbf{V}_{i} *\\mathbf{A} +\\mathbf{V}_{i}
          \\end{gather}
          $$"
      } else if (activation_fun == "Rescale (r)") {
        formula <- "$$
          \\begin{gather}
          \\sum _{\\mathrm{j=1;\ i\\neq j}}^{\\mathrm{M}}( 2\\mathrm{V_{j}^{( t)} -1) w_{ji}} + ( 2\\mathrm{V_{i}^{( t)}} -1) \\newline
          or \\newline \\newline
          ( 2\\mathbf{V}_{i} -1) *\\mathbf{A} +( 2\\mathbf{V}_{i} -1)
          \\end{gather}
          $$"
      }

      shiny::withMathJax(tags$p(formula))
    })

    max_lambda <- reactive({
      if (input$squashing_function == "bivalent" | input$squashing_function == "trivalent") {
        return(NULL)
      }

      if (!identical(simulation_data$reformat_fcm_adj_matrix(), data()$fcm$adj_matrix)) {
        get_lambda_max(simulation_data$reformat_fcm_adj_matrix(), transfer_function = input$squashing_function)
      } else {
        get_lambda_max(data()$fcm$adj_matrix, transfer_function = input$squashing_function)
      }
    })

    observe({
      if (input$squashing_function == "bivalent" | input$squashing_function == "trivalent") {
        shiny::updateNumericInput(session, "lambda", value = 0.5, max = 1)
      } else {
        shiny::updateNumericInput(session, "lambda", value = round(max_lambda(), 5), max = 10)  # max = max_lambda())
      }
    })

    return(
      list(
        node_to_activate = reactive({input$node_to_activate}),
        squashing_function = reactive({input$squashing_function}),
        activation_function = reactive({input$activation_function}),
        # k1 = reactive({input$k1}),
        # k2 = reactive({input$k2}),
        lambda = reactive({input$lambda}),
        max_lambda = reactive({max_lambda()}),
        simplify_edges_selection = reactive({input$simplify_edges_selection}),
        self_connection_value = reactive({input$diagonal_values}),
        iterations = reactive({input$iterations})
      )
    )
  })
}
