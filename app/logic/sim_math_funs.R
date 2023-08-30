
# app/logic/sim_math_funs.R

#' @export
activation_fun <- function(adj_matrix, state_vector, method = "standard") {
  if (method == "Kosko (k)") {
    A_1 <- colSums(state_vector * adj_matrix)
    #A_1 <- state_vector %*% adj_matrix
  } else if (method == "Modified-Kosko (mk)") {
    A_1 <- state_vector %*% adj_matrix + state_vector
  } else if (method == "Rescale (r)") {
    A_1 <- (2*state_vector - 1) %*% adj_matrix + (2*state_vector - 1)
  }

  return(A_1)
}


#' @export
squashing_fun <- function(state_vector, f_squashing = "tanh", lambda = 0.5) {
  if (f_squashing == "tanh") {
    squashed_vec <- (exp(lambda*state_vector) - exp(-lambda*state_vector))/(exp(lambda*state_vector) + exp(-lambda*state_vector))
  } else if (f_squashing == "sigmoid") {
    squashed_vec <- 1 / (1 + exp(-lambda*state_vector))
  } else if (f_squashing == "bivalent") {
    squashed_vec <- ifelse(state_vector == 0, yes = 0, no = 1)
  } else if (f_squashing == "trivalent") {
    squashed_vec <- ifelse(state_vector == 0, yes = 0, no = ifelse(state_vector > 0, yes = 1, no = -1))
  }
  return(squashed_vec)
}
