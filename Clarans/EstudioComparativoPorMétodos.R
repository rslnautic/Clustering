install.packages("ggfortify")
install.packages("ggplot2")
install.packages("cluster")

library(ggfortify)
library(ggplot2)
library(cluster)

library(stats)
library(datasets)


#SEPAL <- CLARANS
cluster <- clarans(x <- iris[1:2], 3,"euclidean", FALSE, 5, 10)
cluster$clustering <- as.factor(cluster$clusters)
cluster$medoids <- as.data.frame(cluster$medoids)
colors <- paste(row.names(cluster$medoids), " - Centroid", sep="")
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = cluster$clusters)) +
  stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = cluster$clusters)) +
  geom_point() +
  ggtitle("Sepal - Clarans") +
  geom_point(shape = 15, size = 2, data= cluster$medoids, mapping=aes(Sepal.Length, Sepal.Width, color = factor(colors))) +
  coord_fixed(ratio = 1)


#SEPAL <- K-MEANS
cluster <- kmeans(iris[1:2], 3)
cluster$cluster <- as.factor(cluster$cluster)
cluster$medoids <- as.data.frame(cluster$centers)
colors <- paste(row.names(cluster$medoids), " - Centroid", sep="")
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = cluster$cluster)) +
  stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = cluster$cluster)) +
  geom_point() +
  ggtitle("Sepal - K-Means") +
  geom_point(shape = 15, size = 2, data= cluster$centers, mapping=aes(Sepal.Length, Sepal.Width, color = factor(colors))) +
  coord_fixed(ratio = 1)


#SEPAL <- PAM
cluster <- pam(iris[1:2], 3, metric = "euclidean")
cluster$clustering <- as.factor(cluster$clustering)
cluster$medoids <- as.data.frame(cluster$medoids)
colors <- paste(row.names(cluster$medoids), " - Centroid", sep="")
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = cluster$clustering)) +
  stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = cluster$clustering)) +
  geom_point() +
  ggtitle("Sepal - Pam") +
  geom_point(shape = 15, size = 2, data= cluster$medoids, mapping=aes(Sepal.Length, Sepal.Width, color = factor(colors))) +
  coord_fixed(ratio = 1)

#SEPAL <- CLARA
cluster <- clara(iris[1:2], 3, metric = "eucliedan")
cluster$clustering <- as.factor(cluster$clustering)
cluster$medoids <- as.data.frame(cluster$medoids)
colors <- paste(row.names(cluster$medoids), " - Centroid", sep="")
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = cluster$clustering)) +
  stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = cluster$clustering)) +
  geom_point() +
  ggtitle("Sepal - Clara") +
  geom_point(shape = 15, size = 2, data= cluster$medoids, mapping=aes(Sepal.Length, Sepal.Width, color = factor(colors))) +
  coord_fixed(ratio = 1)

#--------------------------------------------------------------------------------------------------------------

#PETAL <- CLARANS
cluster <- clarans(x <- iris[3:4], 3,"euclidean", FALSE, 5, 10)
cluster$clustering <- as.factor(cluster$clusters)
cluster$medoids <- as.data.frame(cluster$medoids)
colors <- paste(row.names(cluster$medoids), " - Centroid", sep="")
ggplot(iris, aes(Petal.Length, Petal.Width, color = cluster$clusters)) +
  stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = cluster$clusters)) +
  geom_point() +
  ggtitle("Petal - Clarans") +
  geom_point(shape = 15, size = 2, data= cluster$medoids, mapping=aes(Petal.Length, Petal.Width, color = factor(colors))) +
  coord_fixed(ratio = 1)


#PETAL <- K-MEANS
cluster <- kmeans(iris[3:4], 3)
cluster$cluster <- as.factor(cluster$cluster)
cluster$medoids <- as.data.frame(cluster$centers)
colors <- paste(row.names(cluster$medoids), " - Centroid", sep="")
ggplot(iris, aes(Petal.Length, Petal.Width, color = cluster$cluster)) +
  stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = cluster$cluster)) +
  geom_point() +
  ggtitle("Petal - K-Means") +
  geom_point(shape = 15, size = 2, data= cluster$centers, mapping=aes(Petal.Length, Petal.Width, color = factor(colors))) +
  coord_fixed(ratio = 1)
  

#PETAL <- PAM
cluster <- pam(iris[3:4], 3, metric = "euclidean")
cluster$clustering <- as.factor(cluster$clustering)
cluster$medoids <- as.data.frame(cluster$medoids)
colors <- paste(row.names(cluster$medoids), " - Centroid", sep="")
ggplot(iris, aes(Petal.Length, Petal.Width, color = cluster$clustering)) +
  stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = cluster$cluster)) +
  geom_point() +
  ggtitle("Petal - Pam") +
  geom_point(shape = 15, size = 2, data= cluster$medoids, mapping=aes(Petal.Length, Petal.Width, color = factor(colors))) +
  coord_fixed(ratio = 1)

#PETAL <- CLARA
cluster <- clara(iris[3:4], 3, metric = "eucliedan")
cluster$clustering <- as.factor(cluster$clustering)
cluster$medoids <- as.data.frame(cluster$medoids)
colors <- paste(row.names(cluster$medoids), " - Centroid", sep="")
ggplot(iris, aes(Petal.Length, Petal.Width, color = cluster$clustering)) +
  stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = cluster$clustering)) +
  geom_point() +
  ggtitle("Petal - Clara") +
  geom_point(shape = 15, size = 2, data= cluster$medoids, mapping=aes(Petal.Length, Petal.Width, color = factor(colors))) +
  coord_fixed(ratio = 1)