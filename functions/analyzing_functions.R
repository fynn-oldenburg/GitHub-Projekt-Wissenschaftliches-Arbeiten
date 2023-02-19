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


## (c) Funktion für deskriptive bivariate Statistiken für zwei kategoriale Variablen

bivariate_stats_categorical <- function(data, x_var, y_var) {
  
  # Fehler bei der Suche für x_var und y_var
  if (!(x_var %in% names(data))) {
    stop("x_var ist nicht in data")
  }
  if (!(y_var %in% names(data))) {
    stop("y_var ist nicht in data")
  }
  
  # Kontingenztabelle erstellen
  contingency_table <- table(data[[x_var]], data[[y_var]])
  
  # Hinzufügt Zeilen- und Spaltensummen in der Kontingenztabelle
  contingency_table <- addmargins(contingency_table)
  
  # Berechnung Zeilen- und Spaltenprozentsätze
  row_percents <- prop.table(contingency_table, margin = 1) * 100
  col_percents <- prop.table(contingency_table, margin = 2) * 100
  
  # Ergebnisse in einer Liste
  result <- list(contingency_table = contingency_table,
                 row_percents = row_percents,
                 col_percents = col_percents)
  return(result)
}

## Deskriptive bivariate Statistiken für zwei kategoriale Variablen
## Beispiel:
## bivariate_stats_categorical(data, "Studienfach", "Mathe-LK (ja/nein)")





