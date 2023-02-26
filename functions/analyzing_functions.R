source("functions/helper_functions.R")

library(tidyverse)
library(docstring)


test.data <- data.frame(
    "one" = rnorm(10),
    "two" = rnorm(10, 50, 2),
    "three" = rnorm(10, 10, 7),
    "four" = as.ordered(sample(1:5, size = 10, replace = T)),
    "five" = as.ordered(sample(1:5, size = 10, replace = T)),
    "six" = as.factor(sample(c("A", "B", "C", "D"), size = 10, replace = T)),
    "seven" = as.factor(sample(c("y", "n"), size = 10, replace = T))
    
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
  #' calculate descriptive statistics for metric variables
  #'
  #' @param X Ein Data Frame
  
  Xcat <- X
  colnames(Xcat) <- c(1:ncol(Xcat))
  fac <- rep(NULL, ncol(Xcat))
  for(i in 1:ncol(Xcat)){
    fac[i] <- is.factor(Xcat[,i])
  }
  catfactors <- which(fac == TRUE)
  X <- X[, catfactors]
  # Es werden nur die kategoriellen Variablen weiterverarbeitet
  
  Xtable <- apply(X, 2, table)
  # Haeufigkeitstabellen der einzelnen Faktoren
  
  quantity <- function(X){
    barplot(table(X))
  }
  
  n <- ceiling(sqrt(ncol(X)))
  par(mfrow = c(ceiling(ncol(X)/n), n))
  apply(X, 2, quantity)
  # Die Barplots der Haeufigkeiten aller Spalten (Variablen) sollen dargestellt
  # werden
  
  modus <- function(x){
    ux <- unique(x)
    tab <- tabulate(match(x, ux))
    ux[tab == max(tab)]
  }
  Xmodus <- apply(X, 2, modus)
  Xtable$Modus <- Xmodus
  
  freq <- function(X){
    table(X)/length(X)
  }
  
  normentropy <- function(x){
    -sum(freq(x) * log2(freq(x)))/log2(length(x))
  }
  Xentropy <- apply(X, 2, normentropy)
  Xtable$NormEntropie <- Xentropy
  # Berechnung der normierten Entropie der einzelnen Faktoren
  
  Xtable$AnzahlNA <- length(which(is.na(test.data) == TRUE))
  # Anzahl der fehlenden Werte im gesamten Data Frame
  
  return(Xtable)
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
  n <- sum(contingency_table)
  rows <- nrow(contingency_table)
  cols <- ncol(contingency_table)
  cramer_v <- bivariate_stats_categorical.cramer_v(contingency_table, n, rows, cols)
  
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

stats_bivariate_metric_dichotom <- function(metric_var, dichotomous_var){
  #' @param metric_var Vektor aus Auspraegungen einer metrischen Variablen
  #' @param dichotomous_var Vektor aus Auspraegungen einer metrischen Variablen,
  #'                        kodiert mit 0 und 1
  
  group_0 <- metric_var[dichotomous_var == 0]
  group_1 <- metric_var[dichotomous_var == 1] 
  
  mean <- c(mean(group_0), mean(group_1))
  sd <- c(sd(group_0), sd(group_1))
  
  dist <- data.frame(mean = mean, sd = sd)
  
  return(dist)
}

## Usage example:
# test.data$seven <- as.factor(sample(c(0, 1), size = 10, replace = T))
# stats_bivariate_metric_dichotom(test.data$three, testdata$seven)


library(reshape2)
library(ggplot2)

visualize_categorical <- function(data, id.1, id.2 = NULL,
                                  title = NULL, x.title = NULL,
                                  y.title = NULL, legend.title = NULL) {
  #' Bar-Plot for three or four categorical variables.
  #' Required packages: ggplot2, reshape2
  #' 
  #' 
  #' @param data Data Frame
  #' @param id.1 String. id for reshape2::melt()
  #' @param id.2 String. id for reshape2::melt(). For visualizing 4 Variables
  #' @param title String. title
  #' @param x.title String. x-axis label 
  #' @param y.title String. y-axis label
  #' @param legend.title String. legend label
  library(ggplot2)
  ## rearrange data
  data <- data %>% 
    reshape2::melt(id.vars = c(id.1, id.2))
  
  ## Bar Plot
  p <- data %>% 
    ggplot(aes(x = variable, fill = value)) +
    geom_bar(position = "dodge", alpha = .8) 
  
  if (is.null(id.2)) { 
    ## split by one variable
    p <- p + facet_grid(col = vars(data[,1]))
  }  else {
    ## split by two variables
    p <- p + facet_grid(col = vars(data[,1]),  row = vars(data[,2]))
  }
  ## labels
  p + labs(title = title, x = x.title, y = y.title, fill = legend.title)
  
}
# ?visualize_categorical

## example: four variables
test.data %>% 
  select(c("six", "four", "five", "seven")) %>% 
  visualize_categorical(id.1 = "six", id.2 = "seven",
                        title = "Barplot", x.title = "Interesse", y.title = "Anzahl",
                        legend.title = "Faktor")
## example: four variables
test.data %>% 
  select(c("six", "four", "five", "seven")) %>% 
  visualize_categorical(id.1 = "six",
                        title = "Barplot", x.title = "Interesse / Mathe LK", y.title = "Anzahl",
                        legend.title = "Faktor")


## example: three variables
test.data %>% 
  select(c("six", "four", "five")) %>% 
  visualize_categorical(id.1 = "six")
