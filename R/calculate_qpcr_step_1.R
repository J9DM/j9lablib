#' Calculates delta CT for each sample.
#'
#' @param df input tibble in wide format
#' @param target_now target gene
#' @param housekeeping_now reference gene
#'
#' @importFrom rlang .data
#'
#' @return A tibble with delta CT values
#' @export
#'
# @examples
calculate_qpcr_step_1 <-  function(df, target_now, housekeeping_now) {

  target_var <- rlang::sym(target_now)
  housekeeping_var <- rlang::sym(housekeeping_now)

  df %>%
    dplyr::mutate(
      target = target_now,
      housekeeping = housekeeping_now,
      deltaCT = (!! target_var) - (!! housekeeping_var)
    )
}
