#' Vector of quantiles
#' 
#' @details Designed for use with ggplot2's \code{stat_summary} as a means of 
#' creating a boxplot with user-defined hinges and whiskers. See discussion at 
#' https://stackoverflow.com/questions/21310609/ggplot2-box-whisker-plot-show-95-confidence-intervals-remove-outliers
#' 
#' @param x     numeric vector
#' @param probs numeric vector of percentiles of interest
#' 
#' @return named vector of numeric values corrsponding to quantiles of interest
boxplot_quantiles <- function(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)) {
  q_vec <- quantile(x, probs = probs)
  names(q_vec) <- c("ymin", "lower", "middle", "upper", "ymax")
  return(q_vec)
}