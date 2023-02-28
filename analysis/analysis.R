suppressWarnings(source("functions/helper_functions.R"))
suppressWarnings(source("functions/analyzing_functions.R"))

# Einlesen der Datei

data <- read.csv("students-data.csv")

# Ausführen der Funktionen

## Metrische Variablen

stats_metric(data.frame(data$Alter, data$Mathe, data$Programmieren))

## Kategoriale Variablen

stats_categorical(data)

## bivariate Statistiken für zwei kategoriale Variablen

bivariate_stats_categorical(data, data$Studienfach, data$Mathe)
bivariate_stats_categorical(data, data$Studienfach, data$Programmieren)
bivariate_stats_categorical(data, data$Studienfach, data$MatheLK)
bivariate_stats_categorical(data, data$Mathe, data$Programmieren)
bivariate_stats_categorical(data, data$Mathe, data$MatheLK)
bivariate_stats_categorical(data, data$Programmieren, data$MatheLK)

## bivariate Statistiken für eine metrische und eine kategoriale Variable

stats_bivariate_metric_dichotom(data$Alter, data$MatheLK)

## Kategorisierung ordinaler Variablen

categorize_ordinal(data, by = c(2, 4, 5), bins = 3)

## Visualisierung kategorialer Variablen

# TODO

