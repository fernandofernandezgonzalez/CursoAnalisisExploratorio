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
colnames(data)<-c("sepal_length","sepal_width","petal_length","petal_width") # Este paso es porque las variables tienen puntos, normalmente no seria necesario


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

wss<-sapply(Ks,calculaWss,puntuaciones)

plot(wss)

# 3 parece un número óptimo de clusters!!!


# Calculo centroides iniciales
centroides<-kmeans(puntuaciones,3,nstart=100)$centers

# Calculo los clusters definitivos
clustersFinales<-kmeans(puntuaciones,centroides)

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

# Usamos la misma técnica para detectar el número óptimo de componentes:
# 

varianza_componentes <- cumsum(componentes$sdev/sum(componentes$sdev))
plot(1:length(varianza_componentes),-varianza_componentes)
n_comp_optimo <- getElbowPoint(1:length(varianza_componentes),-varianza_componentes)  # 2 !
n_comp_optimo
# Transformamos las componentes elegidas en una sentencia SQL válida!!
# Esto nos permitiría implementar la puntuación de las componentes principales en cualquier sistema SQL
componentes_elegidas <- componentes$rotation[,1:n_comp_optimo]
centro <- componentes$center

library(glue)
obtener_sentencia <- function(x,centro=NULL){
  if(!is.null(centro)){
    nombres_x <- names(x)
    print(nombres_x)
    print(centro)
    nombres_x <- glue("{nombres_x}-{centro[nombres_x]}")
  } else{
    nombres_x <- names(x)
  }
  sentencia <- glue("({nombres_x})*{x}")
  sentencia <- as.character(sentencia)
  sentencia <- paste0(sentencia,collapse="+")
  #TODO  :aniadir scale if scale is TRUE
}

sentencias <- apply(componentes_elegidas,2,obtener_sentencia,centro=centro)
sentencias <- glue("({sentencias}) as {names(sentencias)}")

query_componentes <- glue(
"
-- create table componentes as
select id, 
{paste0(sentencias, collapse=',\n')}
from tabla_fuente"
)
cat(query_componentes) # Cambiar los nombres de las tablas como se prefiera

# Transformamos la asignacióna los centroides a sentencias SQL válidas!
# Esto nos permitiría implementar la asignación de los cluster en cualquier sistema SQL
centroides_finales <- as.data.frame(clustersFinales$centers) 

fake_table_centroides <- apply(centroides_finales,2,function(x)paste(x," as "))

centroides_variables <- colnames(fake_table_centroides)


fake_table_centroides <- as.data.frame(sapply(centroides_variables,function(x)paste(fake_table_centroides[,x],x)))
fake_table_centroides <- apply(fake_table_centroides,1,function(x)paste(x,collapse=", "))
fake_table_centroides <- paste ( glue("select {1:length(fake_table_centroides)} as cluster, {fake_table_centroides} \n") ,collapse= " \n union distinct ")
fake_table_centroides

# Ahora tenemos que hacer el cross join con esta tabla y poner la formula de la distancia cuadratica (con un group by)

formula <- paste("POW(a.",centroides_variables,"-b.",centroides_variables,",2)",collapse="+",sep="") 

distancias <- glue("
select id, cluster, sqrt({formula}) as distancia
from 
({query_componentes}) a 
cross join ({fake_table_centroides}) b
                 ")

# Finalmente elegimos la distancia menor con una top query
asignacion_cluster <- glue("
select * from 
(select id, cluster, rank() over (partition by id order by distancia) as rank_distancia
from ({distancias})
)
where rank_distancia = 1
")

# Ojo! La sintaxis generada esta pensada para Bigquery, es posible que otras plataformas necesiten ligeras variaciones

