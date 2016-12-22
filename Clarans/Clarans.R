clarans <-function(x, k, metric = "euclidean", stand = FALSE, l = 5, m =10) {
  changesInMedian <- FALSE; 
  medians <- data.frame(matrix(, nrow=k, ncol=0))
  clusters <- 
  while(l > 0) {
    repeat {
      
      
      if(!changesInMedian || m <= 0) {
        break
      }
      m <- m - 1
    }
    l <- l - 1
  }
}