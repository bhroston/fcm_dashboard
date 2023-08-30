
# app/logic/load_data.R

box::use(
  utils[read.csv],
  shinyalert[...],
)

#' @export
load_adj_matrix_from_file <- function(path = "") {
  file_type <- tools::file_ext(path)
  if (file_type == "csv") {
    return(load_adj_matrix_from_csv(path))
  } else if (file_type == "xlsx") {
    return(load_adj_matrix_from_xlsx(path))
  }
}


validate_raw_adj_matrix <- function(raw_adj_matrix) {
  has_nodes_row <- identical(colnames(raw_adj_matrix[-1]), raw_adj_matrix[,1])
  # if raw_adj_matrix does NOT have a nodes column
  if (!has_nodes_row) {
    raw_adj_matrix[is.na(raw_adj_matrix)] <- 0
    val_adj_matrix <- raw_adj_matrix
    # if raw_adj_matrix has a nodes column
  } else {
    # assume nodes in first column
    val_adj_matrix <- raw_adj_matrix[,-1]
    # replace NA values with 0's to work with fcm class
    val_adj_matrix[is.na(val_adj_matrix)] <- 0
  }

  if (ncol(val_adj_matrix) != nrow(val_adj_matrix)) {
    shinyalert(
      text = "Adjacency matrix data must be sqaure (n x n). Data should only include columns of the adjecency matrix. Do not include a column of node names."
    )
    return("fail")
  }

  return(val_adj_matrix)
}


load_adj_matrix_from_csv <- function(path) {
  raw_adj_matrix <- read.csv(path)

  adj_matrix <- validate_raw_adj_matrix(raw_adj_matrix)
  if (identical(adj_matrix, "fail")) {
    return("fail")
  }

  adj_matrix_and_coords <- list(
    "adj_matrix" = adj_matrix,
    "coords" = NA
  )

  return(adj_matrix_and_coords)
}


load_adj_matrix_from_xlsx <- function(path) {
  sheets <- readxl::excel_sheets(path)

  adj_mat_in_sheets <- length(grep("adjmat", sheets, ignore.case = TRUE)) > 0

  if (!adj_mat_in_sheets) {
    shinyalert(
      text = ".xslx file must have 1 sheet named 'ADJmat'"
    )
    return("fail")
  } else {
    adj_mat_sheet_index <- grep("adjmat", sheets, ignore.case = TRUE)
    raw_adj_matrix <- readxl::read_xlsx(path, sheet = sheets[adj_mat_sheet_index])
  }

  adj_matrix <- validate_raw_adj_matrix(raw_adj_matrix)
  if (identical(adj_matrix, "fail")) {
    return("fail")
  }

  coords_in_sheets <- length(grep("coords", sheets, ignore.case = TRUE)) > 0

  if (coords_in_sheets) {
    coords_sheet_index <- grep("coords", sheets, ignore.case = TRUE)
    coords <- readxl::read_xlsx(path, sheet = sheets[coords_sheet_index])
    colnames(coords) <- c("id", "x", "y")
    if (!identical(colnames(adj_matrix), unlist(coords$id, use.names = FALSE))) {
      shinyalert(
        text = "Node names in coords must be equivalent to node names in adjacency matrix (same order).\nEither edit the coords sheet or remove the sheet altogether."
      )
      coords <- NA
    }
  }

  adj_matrix_and_coords <- list(
    "adj_matrix" = adj_matrix,
    "coords" = coords
  )

  return(adj_matrix_and_coords)
}
