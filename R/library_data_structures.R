#' Cross-tabulates columns of a dataframe into a matrix with NAs for unspecified pairs
#' @param model A \code{formula} indicating relevant columns from data frame
#' @param df A \code{data.frame} of columns to cross-tabulate
#' @param rows A \code{vector} of row names for the matrix to sort by
#' @param columns A \code{vector} of column names for the matrix to sort by
df_xtab_matrix <- function(model, df, rows, columns) {
  # identify required relationships
  model_parts <- strsplit(deparse(model), '~')[[1]]
  ones <- rep(1,nrow(df))
  params_model <- as.formula(paste("ones", '~', model_parts[2]))
  params <- xtabs(params_model, df)

  # create model matrix
  xtabs(model, df) -> .
  .[params == 0] <- NA
  as.data.frame.matrix(.) -> .
  as.matrix(.[rows, columns])
}
