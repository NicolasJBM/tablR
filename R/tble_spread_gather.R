#' @name tble_spread_gather
#' @title Complete missing pairs
#' @author Nicolas Mangin
#' @description Spread and gather the table so that missing pairs are added.
#' @param x   tibble or dataframe.
#' @param col character. Name of the column which should be srepad.
#' @param val character. Name of the column populating the cells.
#' @param fill NA or numeric. Value used to fill missing data.
#' @return A tibble or dataframe where missing pairs have beend added.
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>%
#' @importFrom dplyr all_of
#' @export

tble_spread_gather <- function(x, col, val, fill) {
  x %>%
    tidyr::pivot_wider(
      names_from = dplyr::all_of(col),
      values_from = dplyr::all_of(val),
      values_fill = fill
    ) %>%
    tidyr::pivot_longer(
      cols = unique(x[, col]),
      names_to = col,
      values_to = val
    )
}
