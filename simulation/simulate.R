
## Anzahl Studierende
n <- 100 

## Studienfach
studienfach <- sample(c("Statistik", "Data Science", "Mathe", "Informatik"), 
                      size = n, 
                      prob = c(.35, .35, .1, .2),
                      replace = TRUE  
)

data <- data.frame(
  
  ## Alter 
  "Alter" = floor(rnorm(n, 25, 2)),
  
  ## Studienfach
  "Studienfach" = studienfach
   
  ## Interesse an Mathematik
  
  
  ## Interesse an Programmieren
  
  
  ## Mathe-LK (ja/nein)
  
  
)