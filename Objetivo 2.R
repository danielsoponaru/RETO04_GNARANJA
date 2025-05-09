### OBJETIVO 2, RETO 4 ### 

#Cargar librerias
library(recommenderlab)

#Cargar datos
data<- readRDS("Datos//matriz.RDS")

#Examinar datos y tipos de columnas
str(data)
summary(data)
class(data)
dim(data)

#Convertir a matriz de realRatingMatrix
dataM<- as(data, "matrix")
class(dataM)

image(dataM)

