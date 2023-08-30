
# app/logic/simulate_fcm.R

box::use(
  app/logic/sim_math_funs[...],
  app/logic/sim_utility_funs[...],
)

#' @export
simulate_fcm <- function(fcm_adj_matrix = matrix(), initial_state_vector = matrix(), iter = 10,
                         f_activation = "standard", f_squashing = "tanh", lambda = 1) {
  A_0 <- initial_state_vector
  ## Check
  if (length(A_0) != nrow(fcm_adj_matrix)) {
    stop("Length of state vector is not equivalent to the number of
         concepts in fcm",
         .call = FALSE
    )
  }

  ## Calculations
  # Create empty state vectors dataframe to populate
  state_vectors <- create_empty_state_vectors_matrix(fcm_adj_matrix, iters = iter)

  for (i in 1:(iter + 1)) {
    # First row of state vectors = initial_state_vector
    if (i == 1) {
      state_vectors <- rbind(initial_state_vector, state_vectors)
      #print(state_vectors)
    } else {
      # Activation function
      if (lambda == 1 & i < 4) {
        #print(state_vectors[i - 1,])
        #print(colSums(state_vectors[i - 1,]*fcm_adj_matrix))
      }
      next_state_vector <- activation_fun(
        state_vector = state_vectors[i - 1, ], fcm_adj_matrix, f_activation
      )
      #if (lambda == 1) print(next_state_vector)
      # Squashing function
      next_state_vector <- squashing_fun(next_state_vector, f_squashing, lambda = lambda)
      state_vectors[i, ] <- next_state_vector
    }
    #if (lambda == 1) print(state_vectors)
  } # end for loop

  #print(length(initial_state_vector))
  #print(nrow(fcm_adj_matrix))

  #if (lambda == 1) print(fcm::fcm.infer(initial_state_vector, fcm_adj_matrix, infer = "k", transform = "t", lambda = 1))

  # Clean up output; add iteration column to front
  state_vectors_df <- data.frame(state_vectors)
  state_vectors_df <- cbind(iteration = c(0, seq(1:iter)), state_vectors_df)
  rownames(state_vectors_df) <- NULL

  return(state_vectors_df)
}
