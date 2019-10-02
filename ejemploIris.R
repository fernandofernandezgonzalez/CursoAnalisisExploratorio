# Carga de datos
data(iris)

# Graficamos Pétalos Contra Sépalos por cada especie
plot(iris$Petal.Length, 
     iris$Petal.Width, 
     pch=21, 
     bg=c("red","green3","blue")[iris$Species], 
     main="Edgar Anderson's Iris Data")

plot(iris$Petal.Length, 
     iris$Sepal.Length, 
     pch=21, 
     bg=c("red","green3","blue")[iris$Species], 
     main="Edgar Anderson's Iris Data")

plot(iris$Petal.Width, 
     iris$Sepal.Width, 
     pch=21, 
     bg=c("red","green3","blue")[iris$Species], 
     main="Edgar Anderson's Iris Data")

# Seleccionamos solo las columnas numéricas
data<-iris[,1:4]

# Sustituimos valores perdidos por 0

data[is.na(data)]<-0

# Componentes principales
componentes<-prcomp(data)

summary(componentes)

# Calculamos la varianza recogida por las componentes
cumsum(componentes$sdev/sum(componentes$sdev))

# Con dos componentes explicamos más del 85% de la variabilidad
plot(componentes)

# Puntuamos las componentes elegidas (En este caso sobre los mismos datos de partida)
puntuaciones<-as.data.frame(predict(componentes,data)[,1:2])

# Graficamos las plantas con las nuevas componentes
plot(puntuaciones$PC1, 
     puntuaciones$PC2, 
     pch=21, 
     bg=c("red","green3","blue")[iris$Species], 
     main="Edgar Anderson's Iris Data")

# Eliminación de atípicos (Univariante o Multivariante)


# Determinar número óptimo de clusters

Ks<-2:15

calculaWss<-function(k,data){
  
  wss<-kmeans(data,k)$tot.withinss
  
}

wss<-sapply(Ks,calculaWss,data)

plot(wss)

# 3 parece un número óptimo de clusters!!!


# Calculo centroides iniciales
centroides<-kmeans(data,3,nstart=100)$centers

# Calculo los clusters definitivos
clustersFinales<-kmeans(data,centroides$centers)

# Grafico los clusters
plot(puntuaciones$PC1, 
     puntuaciones$PC2, 
     pch=21, 
     bg=c("red","green3","blue")[clustersFinales$cluster], 
     main="Edgar Anderson's Iris Data")

# Comparamos con la verdadera pertenencia a la clase
par(mfrow=c(1,2))
plot(puntuaciones$PC1, 
     puntuaciones$PC2, 
     pch=21, 
     bg=c("red","green3","blue")[iris$Species], 
     main="Edgar Anderson's Iris Data")
plot(puntuaciones$PC1, 
     puntuaciones$PC2, 
     pch=21, 
     bg=c("red","green3","blue")[clustersFinales$cluster], 
     main="Edgar Anderson's Iris Data")
    


