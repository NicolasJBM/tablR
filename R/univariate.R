#' @name univariate
#' @title Compute and format univariate statistics
#' @author Nicolas Mangin
#' @description Generate a table displaying univariate statistics for all the variables included in x which are numeric.
#' @param x         Tibble or dataframe. Table with only numeric variables.
#' @param include   Character vector. Names of the statistics to report: "Count", "Min", "Median", "Mean", "Max", "Range", "St.Dev","Skew" and "Kurt".
#' @return Dataframe. Table of univariate statistics.
#' @importFrom psych skew
#' @importFrom psych kurtosi
#' @importFrom stats na.omit
#' @importFrom stats median
#' @importFrom stats sd
#' @export


univariate <- function(x,
                       include = c(
                         "Variable", "Missing", "Count", "Min",
                         "Median", "Mean", "Max", "Range",
                         "St.Dev", "Skew", "Kurt"
                       )) {
  
  # Reformat to allow processing
  x <- as.data.frame(x)
  x <- x[, unlist(lapply(x, is.numeric))]
  colnbr <- ncol(x)
  variables <- as.vector(names(x))
  
  # Create and fill in a matrix containing all descriptive statistics about
  # the columns
  var_desc <- as.data.frame(matrix(nrow = colnbr, ncol = 11))
  names(var_desc) <- c(
    "Variable", "Count", "Missing", "Min", "Med", "Mean",
    "Max", "Range", "SD", "Skew", "Kurt"
  )
  var_desc[, "Variable"] <- variables
  for (i in 1:colnbr) {
    var_desc[i, "Count"] <- length(stats::na.omit(x[, i]))
    var_desc[i, "Missing"] <- mean(is.na(x[, i]))
    var_desc[i, "Min"] <- min(x[, i], na.rm = TRUE)
    var_desc[i, "Median"] <- stats::median(x[, i], na.rm = TRUE)
    var_desc[i, "Mean"] <- mean(x[, i], na.rm = TRUE)
    var_desc[i, "Max"] <- max(x[, i], na.rm = TRUE)
    var_desc[i, "Range"] <- max(x[, i], na.rm = TRUE) - min(x[, i], na.rm = TRUE)
    var_desc[i, "St.Dev"] <- stats::sd(x[, i], na.rm = TRUE)
    var_desc[i, "Skew"] <- psych::skew(x[, i], na.rm = TRUE)
    var_desc[i, "Kurt"] <- psych::kurtosi(x[, i], na.rm = TRUE)
  }
  
  var_desc <- var_desc[, include]
  var_desc[, "Variable"] <- paste0(
    seq_len(nrow(var_desc)),
    ". ",
    var_desc$Variable
  )
  
  return(var_desc)
}
