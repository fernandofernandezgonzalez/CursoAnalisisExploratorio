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

Ks<-1:15

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
    
##  Obtener el k programáticamente:
# install.packages("inflection")
require(inflection)
# calcular codo, paquete inflection
d2uik(1:length(wss),wss) # Recordar que el punto 2 es k=3!

getElbowPoint <- function(x_values, y_values) {
  
  # Max values to create line
  max_x_x <- max(x_values)
  max_x_y <- y_values[which.max(x_values)]
  max_y_y <- max(y_values)
  max_y_x <- x_values[which.max(y_values)]
  max_df <- data.frame(x = c(max_y_x, max_x_x), y = c(max_y_y, max_x_y))
  
  # Creating straight line between the max values
  fit <- lm(max_df$y ~ max_df$x)
  
  # Distance from point to line
  distances <- c()
  for(i in 1:length(x_values)) {
    distances <- c(distances, abs(coef(fit)[2]*x_values[i] - y_values[i] + coef(fit)[1]) / sqrt(coef(fit)[2]^2 + 1^2))
  }
  
  # Max distance point
  x_max_dist <- x_values[which.max(distances)]
  y_max_dist <- y_values[which.max(distances)]
  
  return(c(x_max_dist, y_max_dist, max(distances)))
} # Fuente: http://www.semspirit.com/artificial-intelligence/machine-learning/clustering/k-means-clustering/k-means-clustering-in-r/
getElbowPoint(1:length(wss),wss) # También acierta con 3!

