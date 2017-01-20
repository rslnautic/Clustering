# EstudioComparativoPorMetodos.R - Sergio Roselló y Ramón Serrano

install.packages("ggfortify")
install.packages("ggplot2")
install.packages("cluster")

library(ggfortify)
library(ggplot2)
library(cluster)

library(stats)
library(datasets)

#-------------------------------------------SEPAL-------------------------------------------------------------------

#SEPAL <- CLARANS
distance = "euclidean"
k = 3
l = 5
m = 10

cluster <- clarans(x <- iris[1:2], k,distance, FALSE, l, m)
cluster$clustering <- as.factor(cluster$clusters)
cluster$medoids <- as.data.frame(cluster$medoids)
colors <- paste(row.names(cluster$medoids), " - Centroid", sep="")
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = cluster$clusters)) +
  stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = cluster$clusters)) +
  geom_point() +
  ggtitle(paste0("Sepal - Clarans - ", "k = ", k, " - Distance = ", distance, " - l = ", l, " - m =", m)) +
  geom_point(shape = 15, size = 2, data= cluster$medoids, mapping=aes(Sepal.Length, Sepal.Width, color = factor(colors))) +
  coord_fixed(ratio = 1)


#SEPAL <- K-MEANS
k = 3

cluster <- kmeans(iris[1:2], k)
cluster$cluster <- as.factor(cluster$cluster)
cluster$medoids <- as.data.frame(cluster$centers)
colors <- paste(row.names(cluster$medoids), " - Centroid", sep="")
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = cluster$cluster)) +
  stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = cluster$cluster)) +
  geom_point() +
  ggtitle(paste0("Sepal - K-Means - ", "k = ", k)) +
  geom_point(shape = 15, size = 2, data= cluster$centers, mapping=aes(Sepal.Length, Sepal.Width, color = factor(colors))) +
  coord_fixed(ratio = 1)


#SEPAL <- PAM
distance = "euclidan"
k = 3

cluster <- pam(iris[1:2], k, metric = distance)
cluster$clustering <- as.factor(cluster$clustering)
cluster$medoids <- as.data.frame(cluster$medoids)
colors <- paste(row.names(cluster$medoids), " - Centroid", sep="")
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = cluster$clustering)) +
  stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = cluster$clustering)) +
  geom_point() +
  ggtitle(paste0("Sepal - Pam - ", "k = ", k, " - Distance = ", distance)) +
  geom_point(shape = 15, size = 2, data= cluster$medoids, mapping=aes(Sepal.Length, Sepal.Width, color = factor(colors))) +
  coord_fixed(ratio = 1)

#SEPAL <- CLARA
distance = "euclidan"
k = 3

cluster <- clara(iris[1:2], k, metric = distance)
cluster$clustering <- as.factor(cluster$clustering)
cluster$medoids <- as.data.frame(cluster$medoids)
colors <- paste(row.names(cluster$medoids), " - Centroid", sep="")
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = cluster$clustering)) +
  stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = cluster$clustering)) +
  geom_point() +
  ggtitle(paste0("Sepal - Clara - ", "k = ", k, " - Distance = ", distance)) +
  geom_point(shape = 15, size = 2, data= cluster$medoids, mapping=aes(Sepal.Length, Sepal.Width, color = factor(colors))) +
  coord_fixed(ratio = 1)

#-------------------------------------------PETAL-------------------------------------------------------------------

#PETAL <- CLARANS
distance = "euclidean"
k = 3
l = 5
m = 10

cluster <- clarans(x <- iris[3:4], k,distance, FALSE, l, m)
cluster$clustering <- as.factor(cluster$clusters)
cluster$medoids <- as.data.frame(cluster$medoids)
colors <- paste(row.names(cluster$medoids), " - Centroid", sep="")
ggplot(iris, aes(Petal.Length, Petal.Width, color = cluster$clusters)) +
  stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = cluster$clusters)) +
  geom_point() +
  ggtitle(paste0("Petal - Clarans - ", "k = ", k, " - Distance = ", distance, " - l = ", l, " - m =", m)) +
  geom_point(shape = 15, size = 2, data= cluster$medoids, mapping=aes(Petal.Length, Petal.Width, color = factor(colors))) +
  coord_fixed(ratio = 1)


#PETAL <- K-MEANS
k = 3

cluster <- kmeans(iris[3:4], k)
cluster$cluster <- as.factor(cluster$cluster)
cluster$medoids <- as.data.frame(cluster$centers)
colors <- paste(row.names(cluster$medoids), " - Centroid", sep="")
ggplot(iris, aes(Petal.Length, Petal.Width, color = cluster$cluster)) +
  stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = cluster$cluster)) +
  geom_point() +
  ggtitle(paste0("Petal - K-Means - ", "k = ", k)) +
  geom_point(shape = 15, size = 2, data= cluster$centers, mapping=aes(Petal.Length, Petal.Width, color = factor(colors))) +
  coord_fixed(ratio = 1)
  

#PETAL <- PAM
distance = "euclidean"
k = 3

cluster <- pam(iris[3:4], k, metric = distance)
cluster$clustering <- as.factor(cluster$clustering)
cluster$medoids <- as.data.frame(cluster$medoids)
colors <- paste(row.names(cluster$medoids), " - Centroid", sep="")
ggplot(iris, aes(Petal.Length, Petal.Width, color = cluster$clustering)) +
  stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = cluster$cluster)) +
  geom_point() +
  ggtitle(paste0("Petal - Pam - ", "k = ", k, " - Distance = ", distance)) +
  geom_point(shape = 15, size = 2, data= cluster$medoids, mapping=aes(Petal.Length, Petal.Width, color = factor(colors))) +
  coord_fixed(ratio = 1)

#PETAL <- CLARA
distance = "euclidean"
k = 3

cluster <- clara(iris[3:4], k, metric = distance)
cluster$clustering <- as.factor(cluster$clustering)
cluster$medoids <- as.data.frame(cluster$medoids)
colors <- paste(row.names(cluster$medoids), " - Centroid", sep="")
ggplot(iris, aes(Petal.Length, Petal.Width, color = cluster$clustering)) +
  stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = cluster$clustering)) +
  geom_point() +
  ggtitle(paste0("Petal - Clara - ", "k = ", k, " - Distance = ", distance)) +
  geom_point(shape = 15, size = 2, data= cluster$medoids, mapping=aes(Petal.Length, Petal.Width, color = factor(colors))) +
  coord_fixed(ratio = 1)