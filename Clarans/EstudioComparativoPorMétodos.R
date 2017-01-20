install.packages("ggfortify")
install.packages("ggplot2")
install.packages("cluster")

library(ggfortify)
library(ggplot2)
library(cluster)

library(stats)
library(datasets)

#S�palo <- CLARANS
cluster <- clarans(x <- iris[1:2], 3,"euclidean", FALSE, 5, 10)
cluster$clustering <- as.factor(cluster$clusters)
cluster$medoids <- as.data.frame(cluster$medoids)
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = cluster$clusters)) +
  geom_point() +
  geom_point(data= cluster$medoids, mapping=aes(Sepal.Length, Sepal.Width, color = factor(c('centroid 1', 'centroid 2','centroid3'))))


#SÉPALO <- K-MEANS
cluster <- kmeans(iris[1:2], 3)
cluster$cluster <- as.factor(cluster$cluster)
cluster$medoids <- as.data.frame(cluster$medoids)
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = cluster$cluster)) +
  geom_point() +
  geom_point(data= cluster$centers, mapping=aes(Sepal.Length, Sepal.Width, color = factor(c('centroid 1', 'centroid 2','centroid3')))) +
  coord_fixed(ratio = 1)


#SÉPALO <- PAM
cluster <- pam(iris[1:2], 3, metric = "euclidean")
cluster$clustering <- as.factor(cluster$clustering)
cluster$medoids <- as.data.frame(cluster$medoids)
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = cluster$clustering)) +
  geom_point() +
  geom_point(data= cluster$medoids, mapping=aes(Sepal.Length, Sepal.Width, color = factor(c('centroid 1', 'centroid 2','centroid3'))))

#SÉPALO <- CLARA
cluster <- clara(iris[1:2], 3, metric = "eucliedan")
cluster$clustering <- as.factor(cluster$clustering)
cluster$medoids <- as.data.frame(cluster$medoids)
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = cluster$clustering)) +
  geom_point() +
  geom_point(data= cluster$medoids, mapping=aes(Sepal.Length, Sepal.Width, color = factor(c('centroid 1', 'centroid 2','centroid3'))))












#S�palo <- CLARANS
cluster <- clarans(x <- iris[3:4], 3,"euclidean", FALSE, 5, 10)
cluster$clustering <- as.factor(cluster$clusters)
cluster$medoids <- as.data.frame(cluster$medoids)
ggplot(iris, aes(Petal.Length, Petal.Width, color = cluster$clusters)) +
  geom_point() +
  geom_point(data= cluster$medoids, mapping=aes(Petal.Length, Petal.Width, color = factor(c('centroid 1', 'centroid 2','centroid3'))))


#PÉTALO <- K-MEANS
Cluster <- kmeans(iris[3:4], 3)
Cluster$cluster <- as.factor(Cluster$cluster)
cluster$medoids <- as.data.frame(Cluster$medoids)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Cluster$cluster)) +
  geom_point() +
  geom_point(data= Cluster$centers, mapping=aes(Petal.Length, Petal.Width, color = factor(c('centroid 1', 'centroid 2','centroid3'))))
  

#PÉTALO <- PAM
cluster <- pam(iris[3:4], 3, metric = "euclidean")
cluster$clustering <- as.factor(cluster$clustering)
cluster$medoids <- as.data.frame(cluster$medoids)
ggplot(iris, aes(Petal.Length, Petal.Width, color = cluster$clustering)) +
  geom_point() +
  geom_point(data= cluster$medoids, mapping=aes(Petal.Length, Petal.Width, color = factor(c('centroid 1', 'centroid 2','centroid3'))))

#PÉTALO <- CLARA
cluster <- clara(iris[3:4], 3, metric = "eucliedan")
cluster$clustering <- as.factor(cluster$clustering)
cluster$medoids <- as.data.frame(cluster$medoids)
ggplot(iris, aes(Petal.Length, Petal.Width, color = cluster$clustering)) +
  geom_point() +
  geom_point(data= cluster$medoids, mapping=aes(Petal.Length, Petal.Width, color = factor(c('centroid 1', 'centroid 2','centroid3'))))