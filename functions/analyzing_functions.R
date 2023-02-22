library(tidyverse)
library(docstring)

categorize_ordinal <- function (data, by=data[,1], bins=4) {
    #' categorize ordinal variables based on quantiles
    #'
    #' @param data Ein Data Frame
    #' @param by eine mindestens ordinale Variable des Datensatzes, nach der kategorisiert werden soll
    #' @param bins Anzahl der Kategorien

    # exceptions
    if (!any(class(data) == 'data.frame')) {
        stop('x has to be a data.frame')
    }

    if (bins <= 0) {
        stop('bins has to be a positive integer')
    }

    # functionality
    return(mutate(data, category = ntile(by, bins)))
}

# usage example
y <- data.frame(rnorm(100, 25, 2))
categorize_ordinal(y)


?categorize_ordinal()



stats_metric <- function (x) {
  #' calculate descriptive statistics for metric variables
  #'
  #' @param x Ein Data Frame

  # exceptions
  if (!any(class(x) == 'data.frame')) {
    stop('x has to be a data.frame')
  }

  # functionality
  y = unlist(x)
  
  result = data.frame("Anzahl" = length(y),
                      "Mittelwert" = mean(y),
                      "Standardabweichung" = sd(y),
                      "Minimum" = min(y),
                      "Maximum" = max(y))
  
  return(result)
}

# usage example
y <- data.frame(rnorm(100, 25, 2))
stats_metric(y)


?stats_metric


stats_categorical <- function (x) {
    # TODO
}

## Cramer_v Funktion f�r kurze Zeit addiert
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

## (c) Funktion für deskriptive bivariate Statistiken für zwei kategoriale Variablen
bivariate_stats_categorical <- function(data, x_var, y_var) {
  #' Funktion zur Berechnung von Bivariate Kategorialen Statistiken
  #' 
  #' Diese Funktion nimmt ein Datenrahmen und zwei kategoriale Variablen auf und gibt eine
  #' Reihe von Statistiken zurück, die die Beziehung zwischen diesen beiden Variablen darstellen.
  #' 
  #' @param data Ein Datenrahmen, der die Variablen enthält.
  #' @param x_var Der Name der ersten kategorialen Variable.
  #' @param y_var Der Name der zweiten kategorialen Variable.
  #' 
  #' @return Eine Liste mit den folgenden Elementen:
  #' \item{contingency_table}{Eine Kontingenztabelle der beiden Variablen.}
  #' \item{row_percents}{Zeilenprozentsätze der Kontingenztabelle.}
  #' \item{col_percents}{Spaltenprozentsätze der Kontingenztabelle.}
  #' \item{chi_squared}{Die Chi-Quadrat-Statistik.}
  #' \item{p_value}{Der p-Wert der Chi-Quadrat-Statistik.}
  #' \item{phi}{Der Phi-Koeffizient.}
  #' \item{cramer_v}{Der Cramér's V-Koeffizient.}
  #' \item{contingency_coefficient}{Der Kontingenzkoeffizient.}
  #' \item{fisher_test}{Der Ergebnis von Fisher's Exact Test, falls die Tabelle 2x2 ist.}
  #' \item{odds_ratio}{Die Odds Ratio, falls die Tabelle 2x2 ist.}
  #'
  #' @examples
  #' data(mtcars)
  #' bivariate_stats_categorical(mtcars, "cyl", "vs")
  #' 
  #' @importFrom stats chisq.test fisher.test
  #' @importFrom stats table prop.table
  #' 
  #' @export
  
  # Fehler bei der Suche für x_var und y_var
  if (!(x_var %in% names(data))) {
    stop("x_var ist nicht in data")
  }
  if (!(y_var %in% names(data))) {
    stop("y_var ist nicht in data")
  }
  
  # Kontingenztabelle erstellen
  contingency_table <- table(data[[x_var]], data[[y_var]])
  
  # Zeilen- und Spaltenprozentsätze berechnen
  row_percents <- prop.table(contingency_table, margin = 1) * 100
  col_percents <- prop.table(contingency_table, margin = 2) * 100
  
  # Berechnung der Chi-Squared-Statistik und des p-Werts
  chisq <- chisq.test(contingency_table)$statistic
  p_value <- chisq.test(contingency_table)$p.value
  
  # Berechnung Phi-Koeffizient
  phi <- sqrt(chisq / sum(contingency_table))
  
  # Berechnung Cramer's V
  #cramer_v <- helper_functions::cramer_v(contingency_table)
  cramer_v <- cramer_v(contingency_table)
  
  # Berechnung des Kontingenzkoeffizienten
  contingency_coefficient <- sqrt(chisq / (chisq + n))
  
  # Berechnung von Fisher's Exact Test und Odds Ratio (falls die Tabelle 2x2 ist)
  fisher_test <- NULL
  odds_ratio <- NULL
  if (rows == 2 && cols == 2) {
    fisher_test <- fisher.test(contingency_table)
    odds_ratio <- fisher_test$estimate
  }
  
  # Ergebnisse in einer Liste
  result <- list(contingency_table = contingency_table,
                 row_percents = row_percents,
                 col_percents = col_percents,
                 chi_squared = chisq,
                 p_value = p_value,
                 phi = phi,
                 cramer_v = cramer_v,
                 contingency_coefficient = contingency_coefficient,
                 fisher_test = fisher_test,
                 odds_ratio = odds_ratio)
  return(result)
}


?bivariate_stats_categorical
?bivariate_stats_categorical()
docstring(bivariate_stats_categorical)


## Deskriptive bivariate Statistiken für zwei kategoriale Variablen
## Beispiel:
## bivariate_stats_categorical(data, "Studienfach", "Mathe-LK (ja/nein)")

# usage test

biv_data <- data.frame('x' = sample(c(1,2,3), 10, replace = TRUE),
                       'y' = sample(c('one', 'two', 'three'), 10, replace = TRUE))

bivariate_stats_categorical(biv_data, 'x', 'y')

# Für row_- und col_percents addieren die Prozentsätze nicht zu 100

