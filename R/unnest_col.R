#' @name unnest_col
#' @title Unnest a column containing vectors
#' @author Nicolas Mangin
#' @description Faster implementation of unnest applicable only when the nested column is a vector (i.e. not a table or dataframe).
#' @param x   tibble. database with a column to unnest.
#' @param col character. Name of the column to unnest.
#' @return A dataframe where all variables are discrete.
#' @source https://stackoverflow.com/questions/40420597/r-flatten-nested-data-table/40420690#40420690
#' @importFrom data.table as.data.table
#' @importFrom rlang ensyms
#' @importFrom rlang syms
#' @importFrom rlang expr
#' @export

unnest_col <- function(x, col) {
  x <- data.table::as.data.table(x)
  col <- rlang::ensyms(col)
  clnms <- rlang::syms(setdiff(colnames(x), as.character(col)))
  x <- data.table::as.data.table(x)
  x <- eval(
    rlang::expr(x[, as.character(unlist(!!!col)), by = list(!!!clnms)])
  )
  colnames(x) <- c(as.character(clnms), as.character(col))
  x
}
