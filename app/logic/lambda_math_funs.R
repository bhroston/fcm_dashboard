
# app/logic/lambda_math_funs

#' @export
# Frobenius Norm -> ||W||_F
get_frobenius_norm <- function(adj_matrix) {
  norm(as.matrix(adj_matrix), "F")
}

#' @export
# s-Norm -> ||W||_s
get_s_norm <- function(adj_matrix) {
  positive_weights <- adj_matrix[adj_matrix > 0]
  negative_weights <- adj_matrix[adj_matrix < 0]

  first_eq <- abs(0.211*sum(positive_weights) + 0.789*sum(negative_weights))
  second_eq <- abs(0.211*sum(negative_weights) + 0.789*sum(positive_weights))

  s_norm <- max(first_eq, second_eq)

  return(s_norm)
}

#' @export
# infinite-norm -> ||W||_infinity
get_infinite_norm <- function(adj_matrix) {
  norm(as.matrix(adj_matrix), "I")
}

#' @export
get_lambda_prime <- function(adj_matrix, transfer_function = "tanh") {
  frobenius_norm <- get_frobenius_norm(adj_matrix)

  if (transfer_function == "tanh") {
    coefficient <- 1
  } else if (transfer_function == "sigmoid") {
    coefficient <- 4
  } else {
    stop("transfer_function must be either tanh or sigmoid")
  }

  lambda_prime <- coefficient/frobenius_norm

  return(lambda_prime)
}

#' @export
get_lambda_star <- function(adj_matrix, transfer_function = "tanh") {
  if (transfer_function == "tanh") {
    coefficient <- 1.14
    norm_value <- get_infinite_norm(adj_matrix) # infinite norm
  } else if (transfer_function == "sigmoid") {
    coefficient <- 1.317
    norm_value <- get_s_norm(adj_matrix)
  } else {
    stop("transfer_function must be either tanh or sigmoid")
  }

  lambda_star <- coefficient/norm_value

  return(lambda_star)
}

#' @export
get_lambda_max <- function(adj_matrix, transfer_function = "tanh") {
  if (transfer_function != "tanh" & transfer_function != "sigmoid") {
    stop("transfer_function must be either tanh or sigmoid")
  } else {
    lambda_prime <- get_lambda_prime(adj_matrix, transfer_function)
    lambda_star <- get_lambda_star(adj_matrix, transfer_function)
    lambda <- min(lambda_prime, lambda_star)
    return(lambda)
  }
}

#' @export
get_normalized_concept_values <- function(adj_matrix, lambda, transfer_function = "tanh") {
  if (transfer_function == "tanh") {
    normalized_adj <- 1.733*adj_matrix
  } else if (transfer_function == "sigmoid") {
    normalized_adj <- (adj_matrix - 0.211)/0.578
  } else {
    stop("transfer_function must be either tanh or sigmoid")
  }

  return(normalized_adj)
}
