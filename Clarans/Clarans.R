x <- iris[1:2]
k = 2
l = 5
m = 10

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

calculateMediansAndDistances<-function(x, medians) {
  dataframeOfMediansAndDistances = data.frame(matrix(0, ncol = 2, nrow = nrow(x)))
  colnames(dataframeOfMediansAndDistances) <- c("Medians","DistanceToMedian")
  
  for(i in 1:nrow(x)) {
    distances <- c()
    
    for(j in 1:nrow(medians)) {
      distances[j] <- euc.dist(x[i,], medians[j,])
    }
    dataframeOfMediansAndDistances[i,1] = row.names(medians[which.min(distances), ])
    dataframeOfMediansAndDistances[i,2] = min(distances)
    
  }
  
  return (dataframeOfMediansAndDistances)
}

clarans(x,k,"euclidean", FALSE, 5, 10)

clarans <-function(x, k, metric = "euclidean", stand = FALSE, l = 5, m = 10) {
  # xWithMedians <- x;
  
  mediansWithDistances = data.frame(matrix(0, ncol = 2, nrow = nrow(x)))
  colnames(mediansWithDistances) <- c("Medians","DistanceToMedian")
  
  tempMediansWithDistances = data.frame(matrix(0, ncol = 2, nrow = nrow(x)))
  colnames(tempMediansWithDistances) <- c("Medians","DistanceToMedian")
  
  # 1. Repetir l veces
  while(l > 0) {
    # 1. Selecciona k instancias al azar como medianas.
    medians = x[sample(nrow(x),k), ]
    
    iterations = 0
    # 2. Repetir
    repeat {
      
      # 1. Re/asignar instancias a la partición con la mediana más próxima
      mediansWithDistances = calculateMediansAndDistances(x, medians)
      # 2. Selecciona una de las medianas al azar y otra instancia del cluster de la mediana al azar.
      randomMedian <- sample(nrow(medians),1)
      
      clusterMedianInstance = x[sample(which(mediansWithDistances$Medians==row.names(medians[][randomMedian,])), 1),]
        
      # 3. Si la nueva instancia mejora el criterio de error absoluto, se reemplaza la mediana.
      
      swapMedians = medians
      
      swapMedians <- swapMedians[][-randomMedian,]
      swapMedians<-rbind(swapMedians,clusterMedianInstance)

      tempMediansWithDistances = calculateMediansAndDistances(x, swapMedians)
      
      bestAbsolutError = sum(mediansWithDistances$DistanceToMedian)
      temoAbsolutError = sum(tempMediansWithDistances$DistanceToMedian)
      
      if(temoAbsolutError < bestAbsolutError) {
        mediansWithDistances$Medians = tempMediansWithDistances$Medians
        mediansWithDistances$DistanceToMedian = tempMediansWithDistances$DistanceToMedian
        medians = swapMedians
        iterations = 0
      } else {
        iterations = iterations + 1
      }
      
      # Mientras haya cambios en las medianas o se alcance m iteraciones sin cambios
      if(iterations >= m) {
        break
      }
      
    }
    l <- l - 1
  }
    
  return (tempMediansWithDistances);
}




