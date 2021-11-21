#' Returns the mean and sd of the control condition for each group.
#'
#' @param df input data set as tibble
#' @param group_vars grouping variable(s), quoted
#' @param filter_var filtering variable, quoted
#' @param ctrl_condition control condition for filtering variable equality test
#' @param summarise_vars variable(s) to summarise, quoted
#'
#' @return A tibble of mean control values and sd
#' @export
#'
# @examples
summarise_control <- function(df, group_vars, filter_var, ctrl_condition,
                              summarise_vars) {

  group_vars <- rlang::syms(group_vars)
  filter_var <- rlang::sym(filter_var)

  df %>%
    dplyr::group_by(!!! group_vars) %>%
    dplyr::filter(!! filter_var == ctrl_condition) %>%
    dplyr::summarise(
      ctrl = ctrl_condition,
      dplyr::across({{ summarise_vars }}, mean, .names = "{.col}_ctrl"),
      n = dplyr::n(),
      dplyr::across({{ summarise_vars }}, stats::sd, .names = "sd_{.col}_ctrl")
    )
}
