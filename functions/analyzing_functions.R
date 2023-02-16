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
