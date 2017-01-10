#x<-matrix(c(3,6,3,4,8),nrow=5,ncol=7,byrow = TRUE)     
#y<-matrix(c(1,4,4,1,9),nrow=5,ncol=7,byrow = TRUE)

k <- 2
y <- iris[1:2]
x <- y[sample(nrow(y), k), ]

interior <- function(x,y)dist(rbind(x,y))

exterior <- function(y) min(apply(x,1,interior,y))

apply(y,1,exterior)