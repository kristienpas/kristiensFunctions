#' Find NAs and duplicates in dataframe
#'
#' @param df A data frame to inspect
#'
#' @returns A nested list with number and indices of NAs and duplicates
#' @export
#'
#' @examples initial_data_check(df)

initial_data_check = function(df) {

  # Number of NAs in dataset
  number_NAs = sum(is.na(df))

  # Index of NAs in dataset
  if (number_NAs != 0) {
    index_NAs = which(is.na(df), arr.ind = TRUE)
  } else {
    print("No NAs found!")
    index_NAs = NA
  }

  # Number of duplicated rows
  df_duplicates = df[duplicated(df), ]
  number_duplicates = dim(df_duplicates)[1]

  # Duplicated rows
  if (number_duplicates != 0) {
    index_duplicates = which(
      duplicated(df) | duplicated(df, fromLast = TRUE))
  } else {
    print("No duplicates found!")
    index_duplicates = NA
  }

  # Save results in a list
  results = list(
    NAs = list(number_NAs = number_NAs, index_NAs = index_NAs),
    duplicates = list(
      number_duplicates = number_duplicates,
      index_duplicates = index_duplicates)
  )

  # Return results
  return(results)
}
