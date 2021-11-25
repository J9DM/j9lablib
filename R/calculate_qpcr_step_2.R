#' Calculates delta-delta CT and fold-change for each sample.
#'
#' @param df input tibble in wide format with deltaCT
#' @param group_vars grouping variable(s), quoted
#' @param filter_var filtering variable, quoted
#' @param ctrl_condition control condition for filtering variable equality test
#' @param summarise_vars variable(s) to summarise, quoted
#'
#' @importFrom rlang .data
#'
#' @return A tibble with expression fold-changes
#' @export
#'
# @examples
calculate_qpcr_step_2 <- function(df, group_vars, filter_var, ctrl_condition,
                              summarise_vars) {

  deltaCT_ctrl <- summarise_control(df,
                                    group_vars = group_vars,
                                    filter_var = filter_var,
                                    ctrl_condition = ctrl_condition,
                                    summarise_vars = summarise_vars)
  df %>%
    dplyr::left_join(deltaCT_ctrl) %>%
    dplyr::mutate(
      deltadeltaCT = .data$deltaCT - .data$deltaCT_ctrl,
      expression_FC = 2 ^ (- .data$deltadeltaCT)
    )

}
