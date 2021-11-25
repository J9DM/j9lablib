#' Plot barplot with data points and error bars
#'
#' @param df input data set as tibble
#' @param x_var aesthetic for plotting
#' @param y_var aesthetic for plotting
#' @param facet_var variable for faceting, quoted
#' @param fill_by variable for fill, quoted
#' @param summary_stat summary statistic for geom_errorbar
#' @param label include text label to indicate summary_stat
#'
#' @return A barplot with data points and error bars
#' @export
#'
# @examples
plot_mybarplot <- function(df, x_var = "concentration", y_var = "expression_FC",
                                  facet_var = "treatment", fill_by = "treatment",
                                  summary_stat = "mean_se", label = FALSE) {

  facet_var <- rlang::sym(facet_var)
  fill_by <- rlang::sym(fill_by)

  label_text <-switch(summary_stat,
                      "mean_se" = "mean +/- sem",
                      "mean_sdl" = "mean +/- n sd",
                      "mean_cl_boot" = "mean +/- CI, bootstrap",
                      "mean_cl_normal", "mean +/- CI, normal distribution")

  df %>%
    ggplot2::ggplot(ggplot2::aes_string(x = x_var, y = y_var)) +
    ggplot2::facet_wrap(ggplot2::vars(!! facet_var), nrow = 1) +

    ggplot2::geom_hline(yintercept = 1, color = "gray70") +
    ggplot2::geom_bar(stat = "summary", fun = "mean",
                      ggplot2::aes(fill = !! fill_by)) +
    ggplot2::geom_point(position = ggplot2::position_dodge2(width = 0.3)) +
    ggplot2::geom_errorbar(stat = "summary",
                           fun.data = summary_stat, width = 0.4) +
    {if (label) ggplot2::labs(caption = label_text)} +

    ggplot2::scale_y_continuous(expand =ggplot2::expansion(c(0, 0.05)))
}
