library(docstring)
library(dplyr)

#### Anzahl Studierende ####
n <- 100



#### Alter aus einer N(25, 2)-Verteilung ####
set.seed(8)
alter <- floor(rnorm(n, 25, 2))



## Studienfach
set.seed(35) 
studienfach <- sample(c("Statistik", "Data Science", "Mathe", "Informatik"),
                      size = n,
                      prob = c(.35, .35, .1, .2),
                      replace = TRUE
                      )



#### Simulation des Interesses in Abhaengigkeit vom Studienfach ####
##
## Das Interesse wird anhand der eingegebenen Wahrscheinlichkeits-Gewichte fuer jeden 
## Studierenden in Anhaengigkeit vom Studienfach gezogen
## 
interesse <- function(pobStat, probDS, probMath, probInfo, seed) {
  #' Simulation des Interesses der Studierenden in
  #' Abhaengigkeit vom Studienfach (1 = wenig, 7 = hohes Interesse)
  #' 
  #' @param probStat Vektor der Laenge 7. Wahrscheinlichkeits-Gewichte fuer Statistik 
  #' @param probDS Vektor der Laenge 7. Wahrscheinlichkeits-Gewichte fuer Data Science
  #' @param probMath Vektor der Laenge 7. Wahrscheinlichkeits-Gewichte fuer Mathe
  #' @param probInfo Vektor der Laenge 7. Wahrscheinlichkeits-Gewichte fuer Informatik
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
# docstring(interesse)



#### Interesse an Mathematik ####
mathe.interesse <- interesse(pobStat = c(.01, .05, .09, .15, .2, .3, .2), 
                             probDS = c(.05, .1, .15, .25, .25, .1, .1), 
                             probMath = c(0, 0, 0, .02, .18, .3, .5), 
                             probInfo = c(.05, .05, .2, .2, .3, .1, .1),
                             seed = 456
)



#### Interesse an Programmieren ####
prog.interesse <- interesse(pobStat = c(.03, .05, .12, .25, .35, .15, .05), 
                            probDS = c(.01, .02, .05, .1, .3, .32, .2), 
                            probMath = c(.05, .05, .2, .3, .2, .15, .05), 
                            probInfo = c(.01, .01, .05, .13, .2, .3, .3),
                            seed = 256
)



#### Mathe LK ####
## 
## Die Funktion  interesseMLK erhoeht die Wahrscheinlichkeit fuer "ja"
## um 0.1 falls das Interesse der Person groesser 4 ist und um 0.5 falls
## das Interesse an Programmieren groesser 4 ist.
## Die Wahrscheinlichkeit fuer "nein" ist die Gegenwahrscheinlichkeit 
## von "ja".
##
interesseMLK <- function(i, probMatheLK, seed) {
  #' Erhoehung der Wahrscheinlichkeit, falls das Interesse groesser 4 ist.
  #' 
  #' @param i Integer. Laufvariable der Schleife
  #' @param probMatheLK Numeric. Zahl zwischen 0 und 1
  #' @param seed Integer. Reproduzierbarkeit der samples
  set.seed(seed)
  if (mathe.interesse[i] > 4){
    probMatheLK <- probMatheLK + .1
  }
  if (prog.interesse[i] > 4) {
    probMatheLK <- probMatheLK + .05
  }
  return(sample(c("ja", "nein"), size = 1, prob = c(probMatheLK, 1 - probMatheLK)))
}
# docstring(interesseMLK)

## Mit der for-Schleife wird fuer die Studierenden eine Basis-Wahrscheinlichkeit 
## festgelegt. Diese wird dann mit der Funktion interesseMLK entsprechend erhoeht, 
## falls das Interesse der Person groesser 4 ist.
##
seed <- 102
mathe.LK <- c()
for (i in 1:n){
  if (studienfach[i] == "Statistik") {
    probMatheLK <- .55 
    mathe.LK[i] <- interesseMLK(i, probMatheLK, seed)
    
  } else if (studienfach[i] == "Data Science") {
    probMatheLK <- .5 
    mathe.LK[i] <- interesseMLK(i, probMatheLK, seed)
    
  } else if (studienfach[i] == "Mathe") {
    probMatheLK <- .7
    mathe.LK[i] <- interesseMLK(i, probMatheLK, seed)
    
  } else { ##Informatik
    probMatheLK <- .5
    mathe.LK[i] <- interesseMLK(i, probMatheLK, seed)
  }
}



#### Die fertigen Daten ####
##
## Data frame mit den oben genrierten Daten und ID-Spalte,
## exportiert als csv-file mit der Bezeichnung "students-data.csv"
##
data.frame(
  
  ## ID
  "ID" = 1:n,
  
  ## Alter 
  "Alter" = alter,
  
  ## Studienfach
  "Studienfach" = studienfach,
  
  ## Interesse an Mathematik
  "Mathe" = mathe.interesse,
   
  ## Interesse an Programmieren
  "Programmieren" = prog.interesse,
  
  ## Mathe-LK (ja/nein)
  "MatheLK" = mathe.LK
  
) %>%
  write.csv("students-data.csv", row.names = FALSE)