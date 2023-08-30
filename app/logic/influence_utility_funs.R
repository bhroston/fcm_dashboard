
# app/logic/influence_utility_funs

box::use(
  dplyr[...]
)

get_influenced_concept_signals <- function(concept_timeseries_df, signal_selection) {
  if (sum(concept_timeseries_df) == 0) return(NULL) # concept not influenced by activated concept
  if (signal_selection == "Peak") {
    signal_index <- which(abs(concept_timeseries_df) == max(abs(concept_timeseries_df)))
  } else if (signal_selection == "First Signal") {
    signal_index <- which(concept_timeseries_df != 0)[1]
  }

  # Sometimes multiple indeces have the same signal; need to return a single value
  if (length(signal_index) > 1) {
    signal_replicates <- concept_timeseries_df[signal_index]
    if (length(unique(signal_replicates)) == 1) {
      signal_index <- signal_index[1]
    }
  }

  signal <- concept_timeseries_df[signal_index]
  return(signal)
}

get_influenced_concept_signals_df <- function(timeseries_df, signal_selection) {
  concept_cols <- which(names(timeseries_df) != "iteration")

  concept_timeseries <- timeseries_df[, concept_cols]

  concept_signals <- unlist(apply(X = concept_timeseries, 2, FUN = get_influenced_concept_signals, signal_selection = signal_selection))

  concept_signals_df <- tibble::tibble(
    concept = factor(names(concept_signals)),
    signal = concept_signals,
    sign = dplyr::case_when(
      concept_signals > 0 ~ "positive",
      concept_signals < 0 ~ "negative",
      concept_signals == 1 ~ "activated"
    )
  )

  return(concept_signals_df)
}
