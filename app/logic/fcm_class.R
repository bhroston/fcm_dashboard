
# app/logic/fcm_class.R

box::use(
  igraph[graph.adjacency, as_adjacency_matrix, graph.data.frame, E, get.edgelist]
)

#' @export
construct_fcm <- function(adj_matrix = matrix(), edgelist = matrix()) {
  # Create class structure
  structure(
    list(
      "concepts" = colnames(adj_matrix),
      "adj_matrix" = adj_matrix,
      "edgelist" = edgelist
    ),
    class = "fcm"
  )
}


#' @export
validate_fcm <- function(fcm) {
  # Checks ====================
  # 1) Adjacency matrix is a nxn matrix (square)

  values <- unclass(fcm)

  # 1) Adjacency matrix is a nxn matrix (square)
  if (nrow(values$adj_matrix) != ncol(values$adj_matrix)) {
    stop(
      "W must be a square matrix with dimensions (n x n)",
      call. = FALSE
    )
  }

  return(fcm)
}


#' @export
explicitly_defined_adj_matrix <- function(adj_matrix) {
  included_concepts <- !(Matrix::rowMeans(adj_matrix) == 0 & Matrix::colMeans(adj_matrix) == 0)
  revised_adj_matrix <- adj_matrix[included_concepts, included_concepts]
  return(revised_adj_matrix)
}



#' @export
fcm_from_adj_matrix <- function(adj_matrix = matrix(), directed = TRUE) {
  raw_adj_matrix <- as.matrix(adj_matrix)
  rownames(raw_adj_matrix) <- colnames(raw_adj_matrix)

  # Revise adj_matrix to remove concepts not included in fcm (rows/cols == 0)
  adj_matrix_revised <- as.matrix(explicitly_defined_adj_matrix(raw_adj_matrix))
  #adj_matrix_revised <- as.matrix(raw_adj_matrix)
  if (directed)
    igraph_obj <- igraph::graph.adjacency(adj_matrix_revised, weighted = TRUE, mode = "directed")
  else if (!directed)
    igraph_obj <- igraph::graph.adjacency(adj_matrix_revised, weighted = TRUE, mode = "undirected")

  adj_matrix <- igraph::as_adjacency_matrix(igraph_obj, attr = "weight", sparse = FALSE)

  edgelist <- data.frame(cbind(igraph::get.edgelist(igraph_obj), igraph::E(igraph_obj)$weight))
  colnames(edgelist) <- c("source", "target", "weight")
  edgelist$weight <- as.numeric(edgelist$weight)

  # Create fcm from adjacency matrix
  # Validate adjacency matrix
  new_fcm <- validate_fcm(
    # Construct fcm object
    construct_fcm(adj_matrix, edgelist)
  )

  return(new_fcm)
}


#' @export
fcm_from_edgelist <- function(edgelist = matrix(), directed = TRUE) {
  igraph_obj <- igraph::graph.data.frame(edgelist, directed = TRUE)
  adj_matrix <- igraph::as_adjacency_matrix(igraph_obj, attr = "weight", sparse = FALSE)

  # Create fcm from adjacency matrix
  # Validate adjacency matrix
  new_fcm <- validate_fcm(
    # Construct fcm object
    construct_fcm(adj_matrix, edgelist)
  )

  return(new_fcm)
}
