#' QC plot for qPCR data
#'
#' @param df input data set as tibble, normalized to control condition
#' @param facet_vars variables for faceting, quoted
#' @param color_by variable for coloring, quoted
#' @param pch_by variable for plotting character, quoted
#'
#' @return A plot
#' @export
#'
# @examples
plot_qpcr_target <- function(df, facet_vars = c("target", "condition"),
                             color_by, pch_by) {

  facet_vars <- rlang::syms(facet_vars)
  color_by <- rlang::sym(color_by)
  pch_by <- rlang::sym(pch_by)
  ctrl_condition <- df$ctrl %>% unique()

  df %>%
    ggplot2::ggplot(aes(x = .data$replicate, y = .data$fold_ctrl)) +
    ggplot2::facet_grid(vars(!! facet_vars[[1]]), vars(!! facet_vars[[2]])) +
    ggplot2::geom_hline(yintercept = 1, color = "red") +
    ggplot2::geom_boxplot() +
    ggplot2::geom_point(aes(color = !! color_by, pch = !! pch_by)) +
    ggplot2::ylab(paste0("Fold change over ", ctrl_condition))
}
