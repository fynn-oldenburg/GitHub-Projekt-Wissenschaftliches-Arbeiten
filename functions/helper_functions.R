stats_metric.inner <- function(x) {
  result <- data.frame("Anzahl" = length(x),
                       "Mittelwert" = mean(x),
                       "Standardabweichung" = sd(x),
                       "Minimum" = min(x),
                       "Maximum" = max(x))
  return(result)
}


bivariate_stats_categorical.cramer_v <- function(contingency_table, n, rows, cols) {
  #' calculate cramer's v
  #'
  #' @param contingency_table die Kontingenztabelle

  # functionality
  chisq <- chisq.test(contingency_table)$statistic

  
  return(sqrt(chisq / (n * (min(rows, cols) - 1))))
}