x <- iris[1:2]
k = 2

clarans(x,k,"euclidean", FALSE, 5, 10)

clarans <-function(x, k, metric = "euclidean", stand = FALSE, l = 5, m = 10) {
  changesInMedian <- FALSE; 
  xWithMedians <- x;
  xWithMedians["Medians"] <- NA;
  xWithMedians["DistanceToMedian"] <- NA;
  xWithMedians["TempMedians"] <- NA;
  xWithMedians["TempDistanceToMedian"] <- NA;
  
  # 1. Repetir l veces
  while(l > 0) {
    # 1. Selecciona k instancias al azar como medianas.
    medians <- randomElements(x, k);
    
    iterations = 1
    # 2. Repetir
    repeat {
      
      # 1. Re/asignar instancias a la partición con la mediana más próxima
      for(i in 1:nrow(x)) {
        distances <- c()
  
        for(j in 1:nrow(medians)) {
          distances[j] <- euc.dist(x[i,], medians[j,])
        }
        xWithMedians[i,3] = row.names(medians[getPosMinValue(distances), ])
        xWithMedians[i,4] = min(distances)
  
      }
      # 2. Selecciona una de las medianas al azar y otra instancia del cluster de la mediana al azar.
      randomMedian = randomElements(medians, 1)
      clusterMedianInstance = NA
  
      clusterMedianInstance = xWithMedians[sample(which(xWithMedians$Medians==row.names(randomMedian)), 1),]
        
      # 3. Si la nueva instancia mejora el criterio de error absoluto, se reemplaza la mediana.
      
      auxmMedians = medians
      
      for(i in 1:nrow(auxmMedians)) {
        if(row.names(auxmMedians[i,]) == row.names(randomMedian)) {
          auxmMedians[i,] = clusterMedianInstance[1:2]
          
          # TODO: cambiar row name de auxmMedians[i,] -> "134" por el name de clusterMedianInstance[1:2] -> "2"
          # row.names(auxmMedians[i,]) = clusterMedianInstance[1:2]$name
          # 
          # rownames(auxmMedians)[1] <- row.names(clusterMedianInstance[1:2])
        }
      }
      
      
      for(i in 1:nrow(x)) {
        distances <- c()
        
        for(j in 1:nrow(auxmMedians)) {
          distances[j] <- euc.dist(x[i,], medians[j,])
        }
        xWithMedians[i,5] = row.names(medians[getPosMinValue(distances), ])
        xWithMedians[i,6] = min(distances)
        
      }
      
      absolutErrorA = sum(xWithMedians$DistanceToMedian)
      
      absolutErrorB = sum(xWithMedians$TempDistanceToMedian)
      
      if(absolutErrorB < absolutErrorA) {
        
      }
      
      # absolutError = 0
      # for(i in 1:nrow(medians)) {
      #   for(j in 1:nrow(x)) {
      #     if(clusterMedianInstance[,3] == xWithMedians[j,3]) {
      #       absolutError = absolutError + xWithMedians[j,4]
      #     }
      #   }
      # }
      # 
      # absolutErrorT = sum(xWithMedians$DistanceToMedian)
      
      
      # Mientras haya cambios en las medianas o se alcance m iteraciones sin cambios
      if(!changesInMedian || iterations >= m) {
        break
      }
      iterations = iterations + 1
    }
    l <- l - 1
  }
    
  return (x);
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
    if(vector[i] <= minValue) {
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

