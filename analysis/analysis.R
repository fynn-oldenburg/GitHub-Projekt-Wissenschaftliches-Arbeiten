
source("functions/helper_functions.R")
source("functions/analyzing_functions.R")

# Einlesen der Datei

data <- read.csv("students-data.csv")


# Ausführen der Funktionen

stats_metric(data.frame(data$Alter, data$Mathe, data$Programmieren))


