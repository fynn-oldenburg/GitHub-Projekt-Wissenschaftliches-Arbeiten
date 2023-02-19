
## Parameter
n <- 100     ## Anzahl Studierende
seed <- 123  ## Reproduzierbarkeit


## Studienfach
set.seed(seed) 
studienfach <- sample(c("Statistik", "Data Science", "Mathe", "Informatik"),
                      size = n,
                      prob = c(.35, .35, .1, .2),
                      replace = TRUE
                      )


## Simualtion des Interesses in Abhaengigkeit vom Studienfach 
interesse <- function(pobStat, probDS, probMath, probInfo, seed) {
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


## Interesse an Mathematik
mathe.interesse <- interesse(pobStat = c(0, .05, .075, .1, .175, .35, .25), 
                             probDS = c(.05, .05, .1, .2, .3, .2, .1), 
                             probMath = c(0, 0, 0, 0, .2, .3, .5), 
                             probInfo = c(.05, .05, .2, .2, .3, .1, .1),
                             seed = seed)


## Interesse an Programmieren


data <- data.frame(
  
  ## Alter 
  "Alter" = floor(rnorm(n, 25, 2)),
  
  ## Studienfach
  "Studienfach" = studienfach,
  
  ## Interesse an Mathematik
  "Mathe" = mathe.interesse
  
  
  ## Interesse an Programmieren
  
  
  ## Mathe-LK (ja/nein)
  
  
)