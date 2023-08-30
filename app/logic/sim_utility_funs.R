
# app/logic/sim_utility_funs.R

box::use(
  tidyr[pivot_longer]
)

#' @export
create_empty_state_vectors_matrix <- function(fcm_adj_matrix, iters) {
  empty_state_vectors_matrix <- matrix(NA, nrow = iters, ncol = ncol(fcm_adj_matrix))
  colnames(empty_state_vectors_matrix) <- colnames(fcm_adj_matrix)
  return(empty_state_vectors_matrix)
}


#' @export
elongate_fcm_simulation_df <- function(fcm_simulation_df) {
  # Remove nodes from legend that won't be a part impacted by node_to_activate
  fcm_simulation_df[colSums(fcm_simulation_df) == 0] <- NULL

  # Elongate simulation_df to simplify ggplot color/group-scheme
  fcm_simulation_df_longer <- as.data.frame(
    tidyr::pivot_longer(fcm_simulation_df, c(2:ncol(fcm_simulation_df)),
                        names_to = "concept", values_to = "value"
    )
  )
  return(fcm_simulation_df_longer)
}
