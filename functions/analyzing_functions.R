library(tidyverse)
library(docstring)


test.data <- data.frame(
    "one" = rnorm(10),
    "two" = rnorm(10, 50, 2),
    "three" = rnorm(10, 10, 7)
)


categorize_ordinal <- function (data, by=1, bins=3, in_place=FALSE) {
    #' categorize ordinal variables based on quantiles
    #'
    #' @param data data.frame
    #' @param by int/string/vector - mindestens ordinale Variable(n) des Datensatzes, nach denen kategorisiert werden soll
    #' @param bins int - Anzahl der Kategorien
    #' @param in_place bool - Wenn TRUE werden die Originalwerte der Variable(n) mit den errechneten Kategorien Ã¼berschrieben

    # exceptions
    if (!any(class(data) == 'data.frame')) {
        stop('data should be a data.frame')
    }

    if (bins <= 0) {
        stop('bins has to be a positive integer')
    }

    # functionality
    if (in_place == TRUE && bins == 3) {
        return(mutate(data, across(by,
                                   ~ recode(ntile(.x, bins),
                                            '1' = 'low',
                                            '2' = 'medium',
                                            '3' = 'high'))))
    }

    if (in_place == TRUE && bins != 3) {
        return(mutate(data, across(by,
                                   ~ ntile(.x, bins))))
    }

    if (bins == 3) {
        return(mutate(data, across(by,
                            ~ recode(ntile(.x, bins),
                                     '1' = 'low',
                                     '2' = 'medium',
                                     '3' = 'high'),
                            .names = 'category_{.col}')))
    }

    return(mutate(data, across(by,
                        ~ ntile(.x, bins),
                        .names = 'category_{.col}')))
}

## usage example
# categorize_ordinal(test.data, c(1,3), bins=5)
# categorize_ordinal(test.data, c('one', 'two'))



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


stats_categorical <- function (X) {
  Xtable <- apply(X, 2, table)
  # Häufigkeitstabellen der einzelnen Faktoren
  
  quantity <- function(X){
    barplot(table(X))
  }
  
  n <- ceiling(sqrt(ncol(X)))
  par(mfrow = c(ceiling(ncol(X)/n), n))
  apply(X, 2, quantity)
  # Die Barplots der Häufigkeiten aller Spalten (Variablen) sollen dargestellt
  # werden
  
  return(Xtable)
}
