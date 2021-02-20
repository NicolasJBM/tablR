#' @name make_unique
#' @title Force unique observation
#' @author Nicolas Mangin
#' @description Remove duplicated entries. If there are several discrepant pieces of information about the observation, take the most frequent unique value, or the mean, or a random value of the duplicates to keep only one observation.
#' @param x Dataframe. All entries for one observation.
#' @return Dataframe with only one row for the observation.
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate_all
#' @importFrom dplyr summarise_all
#' @export

make_unique <- function(x) {
  if (nrow(x) > 1) {
    x <- x %>%
      dplyr::mutate_all(function(x) {
        keep_class <- class(x)
        x <- table(x)
        if (length(x) == 0) x <- NA else
          x <- names(sort(x, decreasing = TRUE))[1]
        class(x) <- keep_class
        x
      }) %>%
      unique()
  } else if (nrow(x) == 1) x <- x else
    x <- dplyr::summarise_all(x, function(x) NA)
  return(x)
}
