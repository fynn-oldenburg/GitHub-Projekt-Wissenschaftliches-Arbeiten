library(docstring)

stats_metric.inner <- function(x) {
  #' stats_metric.inner - berechnet deskriptive Statistiken für metrische Variablen
  #'
  #' Die Funktion berechnet die Anzahl, den Mittelwert, die Standardabweichung, das Minimum und das Maximum
  #' für eine gegebene metrische Variable.
  #'
  #' @param x Ein numerischer Vektor der metrischen Variablen.
  #'
  #' @return Ein Datenrahmen mit den berechneten Statistiken für die gegebene metrische Variable.
  #'
  #' @examples
  #' stats_metric.inner(c(1, 2, 3, 4, 5))
  #'
  #' @export  
  result <- data.frame("Anzahl" = length(x),
                       "Mittelwert" = mean(x),
                       "Standardabweichung" = sd(x),
                       "Minimum" = min(x),
                       "Maximum" = max(x))
  return(result)
}

# ?stats_metric.inner
# ?stats_metric.inner()
# docstring(stats_metric.inner)

# stats_metric.inner(c(1, 2, 3, 4, 5))


bivariate_stats_categorical.cramer_v <- function(contingency_table, n, rows, cols) {
  #' calculate cramer's v
  #'
  #' @param contingency_table die Kontingenztabelle

  # functionality
  chisq <- chisq.test(contingency_table)$statistic

  
  return(sqrt(chisq / (n * (min(rows, cols) - 1))))
}