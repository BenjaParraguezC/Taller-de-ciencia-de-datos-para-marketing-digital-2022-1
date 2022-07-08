#Taller 2 

library(ggplot2)
library(dplyr) #Esta libreria nos permite manipular filas y columnas de un df.
library(corrplot) #Libreria que permite realizar matriz de correlaciones

#Cargamos la data, ademas de separarla por comas y punto y comas. Es necesario buscar el archivo
datos = read.table(file.choose(), sep = ";", dec = ",", header = TRUE)

#Variables CustId, Age, Income, Assets, Creddit Contr_margin
#Sacamos la variable CustId, debido a que es informacion irrelevante para realizar la segmentacion, mediante la libreria dplyr

DataNueva <- select(datos,-CustID) #eliminamos CustID y Credit

pairs(DataNueva, upper.panel = NULL) 
cor(DataNueva)   # MATRIZ DE CORRELACIONES (Tabla)
corrplot(cor(DataNueva), addCoef.col = "black", type = "upper")

#Cargamos libreria cluster para poder realizar k-means
library(cluster)

summary(DataNueva)

set.seed(4201) #Escogemos una semilla inicial aleatoria

KmeansData <- kmeans(DataNueva, centers = 5) #Realizamos la segmentacion 

print(KmeansData$centers)#Imprimos los clusters generados

KmeansData$size #Podemos analizar la cantidad de clientes en cada cluster.


clusplot(DataNueva, KmeansData$cluster, color=TRUE, shade=TRUE,  #Graficar los clusters generados
         labels=4, lines=0, main="K-means cluster plot")

ggplot(DataNueva, aes(Age, Income, color = as.factor(KmeansData$cluster))) + geom_point(size = 3) #
ggplot(DataNueva, aes(Assets, Contr_Margin, color = as.factor(KmeansData$cluster))) + geom_point(size = 3)



#-------------------------------------------------------------------------------

#Normalizando la data ahora.

Dt <- DataNueva #Respaldamos el dataframe original en una nueva variable

DataNueva

#Normalizamos cada una de las variables.
Dt[,c("Age")] <- (Dt$Age-mean(DataNueva$Age))/sd(DataNueva$Age)
Dt[,c("Income")] <- (Dt$Income-mean(DataNueva$Income))/sd(DataNueva$Income)
Dt[,c("Assets")] <- (Dt$Assets-mean(DataNueva$Assets))/sd(DataNueva$Assets)
Dt[,c("Contr_Margin")] <- (Dt$Contr_Margin-mean(DataNueva$Contr_Margin))/sd(DataNueva$Contr_Margin)
Dt[,c("Credit")] <- (Dt$Credit-mean(DataNueva$Credit))/sd(DataNueva$Credit)

summary(Dt)

set.seed(4201) #Ocupamos misma semilla
KmeansDataNormalizada <- kmeans(Dt, centers = 5) #Aplicamos k means

KmeansDataNormalizada$size #Podemos analizar la cantidad de personas en cada cluser

KmeansDataNormalizada$centers #Centro de los clusters.

KmeansDataNormalizada$cluster

ggplot(Dt, aes(Contr_Margin, Income, color = as.factor(KmeansDataNormalizada$cluster))) + geom_point(size = 3) #graficamos
ggplot(Dt, aes(Age, Income, color = as.factor(KmeansData$cluster))) + geom_point(size = 3)

clusplot(DataNueva, KmeansDataNormalizada$cluster, color=TRUE, shade=TRUE,  #Graficamos los clusters
         labels=4, lines=0, main="K-means cluster plot") 

library("e1071")
library(factoextra)

cm_Normalizada <- cmeans(Dt, 5) #Aplicamos c means // en este caso no es obligatorio todavia, lo hice para comparar
pertenencia_Normalizada=cm_Normalizada$membership #Podemos ver el grado de pertenencia a los clusters

pertenencia_Normalizada

ggplot(Dt, aes(Contr_Margin, Income, color = as.factor(KmeansDataNormalizada$cluster))) + geom_point(size = 3)

fviz_cluster(list(data = Dt, cluster=cm_Normalizada$membership),  #Graficando c means
             palette = "jco",
             ggtheme = theme_minimal())

#Utilizando la regla del codo con la data normalizada.

fviz_nbclust(Dt, kmeans, method = c("wss")) 
fviz_nbclust(Dt, kmeans, method = c("silhouette")) 


