coef_bin <- function(df, num_variables) {
  if (num_variables > ncol(df)) {
    stop("Variables exceed the number of columns in df")
  }
  column_combinations <- list()
  for (r in 8:num_variables) {
    column_combinations <- c(column_combinations, combn(names(df), r, simplify = FALSE))
  }
  subset_dfs <- lapply(column_combinations, function(cols) df[, cols, drop = FALSE])
  return(subset_dfs)
}
