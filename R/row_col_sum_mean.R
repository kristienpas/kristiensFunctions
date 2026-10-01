#' Calculate sum/mean across numeric rows/columns
#'
#' @param df A data frame
#' @param rowsum Boolean value for whether sum of rows shall be computed
#' @param colsum Boolean value for whether sum of columns shall be computed
#' @param rowmean Boolean value for whether mean of rows shall be computed
#' @param colmean Boolean value for whether mean of columns shall be computed
#'
#' @returns A nested list of sums and means for rows and columns
#' @export
#'
#' @examples row_col_sum_mean(df, rowsum = T, colsum = F, rowmean = T, colmean = F)
row_col_sum_mean = function(df,
                            rowsum = TRUE,
                            colsum = TRUE,
                            rowmean = TRUE,
                            colmean = TRUE) {

  # Only numeric columns
  numeric_df = df[vapply(df, is.numeric, logical(1))]

  # Check if there are any numeric columns
  if (ncol(numeric_df) == 0) {
    warning("There are no numeric columns.")

    return(list(
      row = list(sum = NA, mean = NA),
      col = list(sum = NA, mean = NA)
    ))
  }

  # Calculate sums and means
  sum_row = if (rowsum) {
    rowSums(numeric_df, na.rm = TRUE)
  } else {
    NA
  }

  sum_col = if (colsum) {
    colSums(numeric_df, na.rm = TRUE)
  } else {
    NA
  }

  mean_row = if (rowmean) {
    rowMeans(numeric_df, na.rm = TRUE)
  } else {
    NA
  }

  mean_col = if (colmean) {
    colMeans(numeric_df, na.rm = TRUE)
  } else {
    NA
  }

  # Save results in a list
  results = list(
    row = list(sum = sum_row, mean = mean_row),
    col = list(sum = sum_col, mean = mean_col)
  )

  # Return results
  return(results)
}
