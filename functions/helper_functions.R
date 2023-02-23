stats_metric.inner <- function(x) {
  result <- data.frame("Anzahl" = length(x),
                       "Mittelwert" = mean(x),
                       "Standardabweichung" = sd(x),
                       "Minimum" = min(x),
                       "Maximum" = max(x))
  return(result)
}


cramer_v <- function(contingency_table) {
  #' calculate cramer's v
  #'
  #' @param contingency_table die Kontingenztabelle

  # functionality
  chisq <- chisq.test(contingency_table)$statistic
  n <- sum(contingency_table)
  rows <- nrow(contingency_table)
  cols <- ncol(contingency_table)
  
  return(sqrt(chisq / (n * (min(rows, cols) - 1))))
}