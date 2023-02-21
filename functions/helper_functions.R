stats_metric.inner <- function(x) {
  result <- data.frame("Anzahl" = length(x),
                       "Mittelwert" = mean(x),
                       "Standardabweichung" = sd(x),
                       "Minimum" = min(x),
                       "Maximum" = max(x))
  return(result)
}