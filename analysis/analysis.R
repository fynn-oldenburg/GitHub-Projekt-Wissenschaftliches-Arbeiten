library(dplyr)
library(ggplot2)
library(gridExtra)
library(reshape2)

source("functions/helper_functions.R")
source("functions/analyzing_functions.R")

## data import
data <- read.csv("students-data.csv") %>%
    mutate(MatheLK = as.factor (MatheLK),
           Programmieren = as.ordered(Programmieren),
           Mathe = as.ordered(Mathe))

## first look
str(data)
apply(data, 2, table)


## all individually
p1 <- data %>% 
  ggplot(aes(x = Alter)) +
  geom_bar()
p2 <- data %>% 
  ggplot(aes(x = Studienfach)) +
  geom_bar()
p3 <- data %>% 
  ggplot(aes(x = Programmieren)) +
  geom_bar()
p4 <- data %>% 
  ggplot(aes(x = Mathe)) +
  geom_bar()
p5 <- data %>% 
  ggplot(aes(x = MatheLK)) +
  geom_bar()

grid.arrange(p1, p2, p3, p4, p5, 
             ncol = 2, nrow = 3)



## visualize
data.vis <- data %>% 
  mutate(
    Mathe = as.ordered(Mathe),
    Programmieren = as.ordered(Programmieren),
  ) %>% 
  select(
    c(Studienfach, Mathe, Programmieren, MatheLK)
  )


levels(data.vis$MatheLK) = c("Mathe-LK", "kein Mathe-LK")
data.vis %>% 
  visualize_categorical(id.1 = "Studienfach", id.2 = "MatheLK",
                        title = "Interessen nach Studienfach und Mathe-LK",
                        x.title = "Interesse", y.title = "Anzahl" 
  )

data.vis %>%
  select(Studienfach, Mathe, Programmieren) %>% 
  visualize_categorical(id.1 = "Studienfach",
                        title = "Interessen nach Studienfach",
                        x.title = "Interesse", y.title = "Anzahl" 
  )



# Ausführen der Funktionen

## Metrische Variablen

stats_metric(data.frame(data$Alter, data$Mathe, data$Programmieren))



## Kategoriale Variablen

stats_categorical(data)



## bivariate Statistiken für zwei kategoriale Variablen

bivariate_stats_categorical(data, "Studienfach", "Mathe")
bivariate_stats_categorical(data, "Studienfach", "Programmieren")
bivariate_stats_categorical(data, "Studienfach", "MatheLK")
bivariate_stats_categorical(data, "Mathe", "Programmieren")
bivariate_stats_categorical(data, "Mathe", "MatheLK")
bivariate_stats_categorical(data, "Programmieren", "MatheLK")

# Ein Cramer's V von über 0.7 spricht für einen starken Zusammenhang 
# von dem Interesse an Mathe und der Wahl für oder gegen den Mathe-Leistungskurs


## bivariate Statistiken für eine metrische und eine kategoriale Variable

stats_bivariate_metric_dichotom(data$Alter, data$MatheLK)



## Kategorisierung ordinaler Variablen

categorize_ordinal(data, by = 'Mathe', bins = 3, in_place=TRUE) %>%
    group_by(Mathe) %>%
    summarize(n = sum(MatheLK == 'ja'))
# Wenn niedriges Interesse an Mathe vorliegt, dann wird eher nicht der MatheLK gewählt

categorize_ordinal(data, by = 'Mathe', bins = 3, in_place=TRUE) %>%
    group_by(Mathe) %>%
    summarize(n = sum(Studienfach == 'Mathe'))
# Wenn niedriges Interesse an Mathe vorliegt, dann wird gar nicht das Studienfach Mathe gewählt?

categorize_ordinal(data, by = 'Programmieren', bins = 3, in_place=TRUE) %>%
    group_by(Programmieren) %>%
    summarize(n = sum(Studienfach == 'Informatik'))
# bei höherem Interesse am Programmieren wird eher der Studiengang Informatik gewählt

categorize_ordinal(data, by = 'Programmieren', bins = 3, in_place=TRUE) %>%
    group_by(Programmieren) %>%
    summarize(n = sum(Studienfach == 'Data Science'))
# Interesse an Programmieren scheint einen positiven Einfluss auf die Wahl von Data Science als Studienfach zu haben.

categorize_ordinal(data, by = 'Programmieren', bins = 3, in_place=TRUE) %>%
    group_by(Programmieren) %>%
    summarize(n = sum(Studienfach == 'Statistik'))
# Interesse am Programmieren ist nicht ausschlaggebend für die Wahl des Statistik-Studiums.
# Eher ein gegenteiliger Effekt ist zu vermerken. Vermutlich wählen die Personen dann eher Data Science?



