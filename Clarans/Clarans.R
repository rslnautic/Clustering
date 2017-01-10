x <- iris[1:2]
k = 2

clarans(x,k,"euclidean", FALSE, 5, 10)

clarans <-function(x, k, metric = "euclidean", stand = FALSE, l = 5, m = 10) {
  changesInMedian <- FALSE; 
  xWithMedians <- x;
  xWithMedians["Medians"] <- NA;
  xWithMedians["DistanceToMedian"] <- NA;
  
  # 1. Repetir l veces
  while(l > 0) {
  # 1. Selecciona k instancias al azar como medianas.
  medians <- randomElements(x, k);
  
  iterations = 0
  # 2. Repetir
  repeat {
    
    # 1. Re/asignar instancias a la partición con la mediana más próxima // TODO: guardar las distancias
    for(i in 1:nrow(x)) {
      distances <- c()

      for(j in 1:nrow(medians)) {
        distances[j] <- euc.dist(x[i,], medians[j,])
      }
      xWithMedians[i,3] = getPosMinValue(distances)
      xWithMedians[i,4] = min(distances)

    }
    # 2. Selecciona una de las medianas al azar y otra instancia del cluster de la mediana al azar.
    randomMedian = randomElements(medians, 1)
    clusterMedianInstance = NA

    clusterMedianInstance = xWithMedians[sample(which(xWithMedians$Medians==row.name(randomMedian)), 1),]
      
    # 3. Si la nueva instancia mejora el criterio de error absoluto, se reemplaza la mediana.
    absolutError = 0
    for(i in 1:nrow(medians)) {
      for(j in 1:nrow(x)) {
        if(clusterMedianInstance[,3] == xWithMedians[j,3]) {
          clInstance = clusterMedianInstance[1:2]
          absolutError = absolutError + euc.dist(x[j,], clInstance)
        }
      }
    }
    
    # Mientras haya cambios en las medianas o se alcance m iteraciones sin cambios
    if(!changesInMedian || iterations >= m) {
      break
    }
    iterations = iterations + 1
  }
  l <- l - 1
  }
    
  return (medians);
}

getPosInDataFrame <- function(value, dataFrame) {
  indexC1 = which(dataFrame$Sepal.Length == value$Sepal.Length)
  indexC2 = which(dataFrame$Sepal.Width == value$Sepal.Width)
  
  if(indexC1 == indexC2) {
    return (indexC1)
  } else {
    return (-1)
  }
}

getPosMinValue <- function(vector) {
  minValue <- 999999
  posMinValue <- -1
  for(i in 1:length(vector)) {
    if(vector[i] < minValue) {
      minValue = vector[i]
      posMinValue = i
    }
  }
  return (posMinValue)
}

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

randomElements <- function(x, n) {
  if (length(x) <= 1) {
    return(x)
  } else {
    return(x[sample(nrow(x), n), ])
  }
}

