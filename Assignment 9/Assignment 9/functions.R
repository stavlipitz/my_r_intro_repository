summarise_df <- function(df, subject_start, subject_end) {
  if (nrow(df) < 10) {
    print("data is too short")
    return(NULL)
  }
  else{
  # Turn rows to NA if subject is outside the range
  df[df$subject < subject_start | df$subject > subject_end, ] = NA
  
  # Apply function to each column in the data frame
  results <- lapply(df, function(col) {
    # For character columns: return table of unique values
    if (class(col) == "character") {
      obs_number <- table(col)
      return(list(obs_number))
    } else {
      # For numeric or other columns: return mean, min, and max
      mean_col <- mean(col, na.rm = TRUE)
      min_col  <- min(col, na.rm = TRUE)
      max_col  <- max(col, na.rm = TRUE)
      return(list(mean = mean_col, min = min_col, max = max_col))
    }
  })}
  
  # Combine results into a data frame
  return(as.data.frame(results))
}
