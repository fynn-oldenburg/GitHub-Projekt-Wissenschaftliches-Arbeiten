source("functions/helper_functions.R")

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



stats_metric <- function (data) {
  #' calculate descriptive statistics for metric variables
  #'
  #' @param data Ein Data Frame mit metrisch-skalierten Spalten.
  
  # exceptions
  if (!any(class(data) == 'data.frame')) {
    stop('x has to be a data.frame')
  }
  
  list.tmp <- apply(data, 2, stats_metric.inner) 
  result <- do.call(rbind.data.frame, list.tmp)
  return(result)
}

## example
test.data %>% 
  select(c("one", "three")) %>% 
  stats_metric()




stats_categorical <- function (data) {
    # TODO
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
  #' \item{cramer_v}{Der Cramer's V-Koeffizient.}
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
  cramer_v <- helper_functions::cramer_v(contingency_table)
  
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

# biv_data <- data.frame('x' = sample(c(1,2,3), 10, replace = TRUE),
#                        'y' = sample(c('one', 'two', 'three'), 10, replace = TRUE))
# 
# bivariate_stats_categorical(biv_data, 'x', 'y')

# Für row_- und col_percents addieren die ProzentsÃ¤tze nicht zu 100
data(mtcars)
bivariate_stats_categorical(mtcars, "cyl", "vs")
