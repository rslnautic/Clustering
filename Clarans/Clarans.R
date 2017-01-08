x <- iris[1:2]
k = 2

clarans(x,k,"euclidean", FALSE, 5, 10)

clarans <-function(x, k, metric = "euclidean", stand = FALSE, l = 5, m = 10) {
  changesInMedian <- FALSE; 
  xWithMedians <- x;
  xWithMedians["Medians"] <- NA;
  
  # 1. Repetir l veces
  while(l > 0) {
  # 1. Selecciona k instancias al azar como medianas.
  medians <- randomElements(x, k);
  # 2. Repetir
  repeat {
    # 1. Re/asignar instancias a la partición con la mediana más próxima
    for(i in 1:nrow(x)) {
      distances <- c()
      # value = x[i,] %in% medians
      value = nrow(merge(x[i,], medians))>0
      if(!value) {
        for(j in 1:nrow(medians)) {
          distances[j] <- euc.dist(x[i,], medians[j,])
        }
        xt[i,3] = getPosMinValue(distances)
      }
    }
    # 2. Selecciona una de las medianas al azar y otra instancia del cluster de la mediana al azar.
    randomMedian = randomElements(medians, 1)
    repeat {
      clusterMedianInstance = randomElements(xWithMedians, 1)
      if(getPosInDataFrame(randfomMedian, medians) == clusterMedianInstance[,3]) {
        break
      }
    }
    # 3. Si la nueva instancia mejora el criterio de error absoluto, se reemplaza la mediana.

    if(!changesInMedian || m <= 0) {
      break
    }
    m <- m - 1
  }
  l <- l - 1
  }
    
  medians = randomElements(x, k);
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

