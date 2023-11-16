standarize_data_frame <-
function(df) {
  #Function standardize each column of the data frame
  #Args: df: data frame
  #Returns: standardized_df: data frame (Data Frame with standradized values in each column)
  stopifnot(is_numeric_values(df))
  standardized_df <- as.data.frame(lapply(df, function(x) (x - mean(x)) / sd(x)))
  return(standardized_df)
}
