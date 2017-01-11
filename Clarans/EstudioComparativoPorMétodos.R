library(ggfortify)
library(ggplot2)
library(cluster)

#SÉPALO

#K-MEANS - Sépalo
autoplot(kmeans(iris[1:2], 3), data = iris, frame = TRUE, frame.type = 'norm')

#PAM - sépalo
autoplot(pam(iris[1:2], 3, metric = "euclidean"), frame = TRUE, frame.type = 'norm')

#CLARA - Sépalo
autoplot(clara(iris[1:2], 3, metric = "eucliedan"), frame = TRUE, frame.type = 'norm')



#PÉTALO

#K-MEANS - Pétalo
autoplot(kmeans(iris[3:4], 3), data = iris, frame = TRUE, frame.type = 'norm')

#PAM - Pétalo
autoplot(pam(iris[3:4], 3, metric = "euclidean"), frame = TRUE, frame.type = 'norm')

#CLARA - Pétalo
autoplot(clara(iris[3:4], 3, metric = "eucliedan"), frame = TRUE, frame.type = 'norm')