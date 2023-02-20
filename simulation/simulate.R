library(docstring)
library(dplyr)

## Parameter
n <- 100    ## Anzahl Studierende
seed <- 23  ## Reproduzierbarkeit


## Studienfach
set.seed(seed) 
studienfach <- sample(c("Statistik", "Data Science", "Mathe", "Informatik"),
                      size = n,
                      prob = c(.35, .35, .1, .2),
                      replace = TRUE
                      )


## Simulation des Interesses in Abhaengigkeit vom Studienfach 
interesse <- function(pobStat, probDS, probMath, probInfo, seed) {
  #' Simulation des Interesses der Studierenden in
  #' Abhaengigkeit vom Studienfach (1 = wenig, 7 = hohes Interesse)
  #' 
  #' @param probStat Vector der Laenge 7. Wahrscheinlichkeits-Gewichte fuer Statistik 
  #' @param probDS Vector der Laenge 7. Wahrscheinlichkeits-Gewichte fuer Data Science
  #' @param probMath Vector der Laenge 7. Wahrscheinlichkeits-Gewichte fuer Mathe
  #' @param probInfo Vector der Laenge 7. Wahrscheinlichkeits-Gewichte fuer Informatik
  #' @param seed Integer. Reproduzierbarkeit der samples
  set.seed(seed)
  interesse <- sapply(studienfach, function(x){
    if (x == "Statistik") {
      sample(1:7, size = 1, prob = pobStat)
    } else if (x == "Data Science") {
      sample(1:7, size = 1, prob = probDS)
    } else if (x == "Mathe") {
      sample(1:7, size = 1, prob = probMath)
    } else {
      sample(1:7, size = 1, prob = probInfo)
    }
  })
  attributes(interesse) <- NULL
  return(interesse)
}
# ?interesse


## Interesse an Mathematik
mathe.interesse <- interesse(pobStat = c(.01, .05, .09, .15, .2, .3, .2), 
                             probDS = c(.05, .1, .15, .25, .25, .1, .1), 
                             probMath = c(0, 0, 0, .02, .18, .3, .5), 
                             probInfo = c(.05, .05, .2, .2, .3, .1, .1),
                             seed = seed
                             )


## Interesse an Programmieren
prog.interesse <- interesse(pobStat = c(.03, .05, .12, .25, .35, .15, .05), 
                            probDS = c(.01, .02, .05, .1, .3, .32, .2), 
                            probMath = c(.05, .05, .2, .3, .2, .15, .05), 
                            probInfo = c(.01, .01, .05, .13, .2, .35, .25),
                            seed = seed
                            )


## Mathe LK
prob.interesse <- function(i, probMatheLK, seed) {
  set.seed(seed)
  if (mathe.interesse[i] > 4){
    probMatheLK <- probMatheLK + .2
  }
  if (prog.interesse[i] > 4) {
    probMatheLK <- probMatheLK + .05
  }
  return(sample(c("ja", "nein"), size = 1, prob = c(probMatheLK, 1 - probMatheLK)))
}

mathe.LK <- c()
for (i in 1:n){
  if (studienfach[i] == "Statistik") {
    probMatheLK <- .6 
    mathe.LK[i] <- prob.interesse(i, probMatheLK, seed)
    
  } else if (studienfach[i] == "Data Science") {
    probMatheLK <- .5 
    mathe.LK[i] <- prob.interesse(i, probMatheLK, seed)
    
  } else if (studienfach[i] == "Mathe") {
    probMatheLK <- .7
    mathe.LK[i] <- prob.interesse(i, probMatheLK, seed)
    
  } else { ##Informatik
    probMatheLK <- .5
    mathe.LK[i] <- prob.interesse(i, probMatheLK, seed)
  }
}




set.seed(seed)
data <- data.frame(
  
  ## Alter 
  "Alter" = floor(rnorm(n, 25, 2)),
  
  ## Studienfach
  "Studienfach" = studienfach,
  
  ## Interesse an Mathematik
  "Mathe" = mathe.interesse,
   
  ## Interesse an Programmieren
  "Programmieren" = prog.interesse,
  
  ## Mathe-LK (ja/nein)
  "MatheLK" = mathe.LK
  
) %>%
  write.csv("students-data.csv")
