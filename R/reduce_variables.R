reduce_variables <- function(data, n_replicates, n_vars) {
  list_dfs <- list()
  for (i in 1:n_replicates) {
    selected_cols <- sample(colnames(data), n_vars)
    selected_cols <- sort(selected_cols)
    df_selected <- data[ , selected_cols, drop = FALSE]
    if (!any(sapply(list_dfs, function(df) identical(df, df_selected)))) {
      list_dfs[[length(list_dfs) + 1]] <- df_selected
    }
  }
  return(list_dfs)
}
