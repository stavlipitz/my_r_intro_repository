summarise_df <- function(df) {
  # Apply function to each column in the data frame
  result <- lapply(df, function(col) {
    if (is.character(col)) {
      # For character columns: return table of unique values
      obs_number <- table(col)
      return(list(summary = obs_number))
    } else {
      # For numeric or other columns: return mean, min, and max
      mean_col <- mean(col, na.rm = TRUE)
      min_col <- min(col, na.rm = TRUE)
      max_col <- max(col, na.rm = TRUE)
      return(list(mean = mean_col, min = min_col, max = max_col))
    }
  })
  
  return(result)
}
