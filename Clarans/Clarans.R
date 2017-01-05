x = iris[1:2]
k = 2
medians = randomElements(x, k)

clarans <-function(x, k, metric = "euclidean", stand = FALSE, l = 5, m = 10) {
  changesInMedian <- FALSE; 
  #medians <- data.frame(matrix(, nrow=k, ncol=0))
  #clusters <- 
  
  # 1. Repetir l veces
  # while(l > 0) {
  # 1. Selecciona k instancias al azar como medianas.
  medians <- randomElements(x, k);
  # 2. Repetir
  # repeat {
  #   # 1. Re/asignar instancias a la partición con la mediana más próxima
  # 
  #   # 2. Selecciona una de las medianas al azar y otra instancia del cluster de la mediana al azar.
  # 
  #   # 3. Si la nueva instancia mejora el criterio de error absoluto, se reemplaza la mediana.
  # 
  #   if(!changesInMedian || m <= 0) {
  #     break
  #   }
  #   m <- m - 1
  # }
  # l <- l - 1
  # }
    
  medians = randomElements(x, k);
  return (medians);
}

randomElements <- function(x, n) {
  if (length(x) <= 1) {
    return(x)
  } else {
    return(x[sample(nrow(x), n), ])
  }
}

