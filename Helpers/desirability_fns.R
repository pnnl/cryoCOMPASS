
desirability_predictors_values <- function(column) {
  if (is.numeric(column)) {
    # Numeric: Min, Max and Sequence
    min_val <- min(column)
    max_val <- max(column)
    sequence <- seq(from = min_val, to = max_val, length.out = 100)
    sequence <- round(sequence, digits = 4)
    sequence <- unique(sequence)
    return(list(type = "numeric", unique_values = sequence))
  } else {
    # Categorical: Unique values
    unique_values <- unique(column)
    return(list(type = "categorical", unique_values = unique_values))
  }
}

convert_columns_to_numeric <- function(data_frame, columns) {
  data_frame[columns] <- lapply(data_frame[columns], function(col) {
    as.numeric(as.character(col))
  })
  return(data_frame)
}


convert_columns_to_categorical <- function(data_frame, columns) {
  data_frame[columns] <- lapply(data_frame[columns], function(col) {
    as.character(col)
  })
  return(data_frame)
}