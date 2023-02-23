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
    #' @param in_place bool - Wenn TRUE werden die Originalwerte der Variable(n) mit den errechneten Kategorien überschrieben

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


library(reshape2)
library(ggplot2)

visualize_categorical <- function(data, id.1, id.2 = NULL,
                                  title = NULL, x.title = NULL,
                                  y.title = NULL, legend.title = NULL) {
  #' Bar-Plot for three or four categorical variables
  #'
  #' 
  #' @param data Data Frame
  #' @param id.1 String. id for reshape2::melt()
  #' @param id.2 String. id for reshape2::melt(). For visualizing 4 Variables
  #'
  data <- data %>% 
    reshape2::melt(id.vars = c(id.1, id.2))
  
  p <- data %>% 
    ggplot(aes(x = variable, fill = value)) +
    geom_bar(position = "dodge", alpha = .8) 
  
  if (is.null(id.2)) { ## 3 Variablen
    p <- p + facet_grid(col = vars(data[,1]))
  }  else { ## 4 Variablen
    p <- p + facet_grid(col = vars(data[,1]),  row = vars(data[,2]))
  }
  
  p + labs(title = title, x = x.title, y = y.title, fill = legend.title)
  
}

## example: four variables
test.data %>% 
  select(c("six", "four", "five", "seven")) %>% 
  visualize_categorical(id.1 = "six", id.2 = "seven",
                        title = "Barplot", x.title = "Interesse", y.title = "Anzahl",
                        legend.title = "Faktor")
## example: for variables
test.data %>% 
  select(c("six", "four", "five", "seven")) %>% 
  visualize_categorical(id.1 = "six",
                        title = "Barplot", x.title = "Interesse / Mathe LK", y.title = "Anzahl",
                        legend.title = "Faktor")


## example: three variables
test.data %>% 
  select(c("six", "four", "five")) %>% 
  visualize_categorical(id.1 = "six")



