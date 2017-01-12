#x<-matrix(c(3,6,3,4,8),nrow=5,ncol=7,byrow = TRUE)     
#y<-matrix(c(1,4,4,1,9),nrow=5,ncol=7,byrow = TRUE)

k <- 2
y <- iris[1:2]
x <- y[sample(nrow(y), k), ]

interior <- function(x,y)dist(rbind(x,y))

exterior <- function(y) min(apply(x,1,interior,y))

apply(y,1,exterior)


c(1,2)

list(cluster=c(1,2), medoids=c(1,2,3), error = 2.5)

temp = list(clusters=c(1,2), medoids=c(1,2,3), error = 2.5)

temp$clusters

a = c(1,2)
b = c(3,4)

help("rbind")
rbind(a,b)
cbind(a,b)
cbind(a,0)
rbind(a,0)

min(2, 3, 4, 5)
which.min(c(2, 3, 4, 5))
which.max(c(2, 3, 4, 5))

iris[5][which(iris$Species == "versicolor"), ]

which(iris$Species == "versicolor")

sample(which(iris$Species == "versicolor"), 1)

sample(which(iris$Species == "versicolor"), 2)

miprint <- function(x) print(x)

miprint(2)

c = rbind(a,b)
c

apply(c, 1, miprint)

apply(c, 2, miprint)

print(2,3)

miprint <- function(x, y) print(cbind(x,y))


apply(c, 1, miprint, 0)

a
b
a+b

as.factor(a)

# importante para hacer plot de los cluster -> Ejemplo apuntes kmedians plot con factor
as.factor(c(1,2,1,2,1,2))

data.frame(a)

row.names(data.frame(a[1]))

data.frame(a,b)

#ggplot
head(iris)

colnames(iris)

rownames(iris)

c = data.frame(a,b)

colnames(c)

colnames(c) <- c("Primera", "Segunda")

colnames(c)

rownames(c)[2] <- "4"
















