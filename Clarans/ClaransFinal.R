

clarans <-function(x, k, metric = "euclidean", stand = FALSE, l = 5, m = 10) {
  xWithMedians <- x;
  xWithMedians["Medians"] <- NA;
  xWithMedians["DistanceToMedian"] <- NA;
  xWithMedians["TempMedians"] <- NA;
  xWithMedians["TempDistanceToMedian"] <- NA;
  
  while(l > 0) {
    # 1. Selecciona k instancias al azar como medianas.
    # medians <- randomElements(x, k);
    
    #Selecciona al azar k filas del data.frame x
    medians <- x[sample(nrow(x), k), ]
    
    iterations = 0
    # 2. Repetir
    repeat {
      
      # 1. Re/asignar instancias a la partición con la mediana más próxima // TODO: guardar las distancias
      lapply(x[1:2], function(x) {
        lapply(medians[1:2], function(y){
          euc.dist(x,y)
        })
      })
      
      lapply(x[1:2], 1, function(x) {
        euc.dist(x, medians[1,]) 
      })
      
      # 2. Selecciona una de las medianas al azar y otra instancia del cluster de la mediana al azar.
      
      # 3. Si la nueva instancia mejora el criterio de error absoluto, se reemplaza la mediana.
      
      
      # Condición de salida del repeat - Mientras haya cambios en las medianas o se alcance m iteraciones sin cambios
      if(!changesInMedian || iterations >= m) {
        break
      }
      iterations = iterations + 1
    }
    l <- l - 1
  }
  return (medians);
}