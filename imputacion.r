# Datos perdidos
# Imputacion de datos
install.packages("mice")

x <- c(13,14,15,NA,12,13,14)
y <- c(12,NA,NA,13,14,12,13)
z <- c(12,12,12,12,12,NA,13)
A <- c(1,2,1,1,1,2,NA) # cualitativa (1=varon, 2= mujer)
mean(x)
# podemos eliminar NAs indicando la posicion del dato
x1 <- x[-4] # eliminando en x el dato de la 4ta posicion (NA)
x1
mean(x1)

# creando un data.frame 
datos <- data.frame(x,y,z,A)
head(datos,7)
str(datos)

# imputando los datos faltantes utilizando la media
library(base)
library(stats)
library(mice) 
# imputación de datos faltantes con la media
tempData <- mice(datos,m=5,maxit=2,method="mean",seed=500,print=F)
newdata <- round(complete(tempData),2)
head(newdata,7)
# observe la variable cualitativa A, se imputo inadecuadamente

# imputando data cualitativa
moda =getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

modaA =moda(datos$A)
modaA

datos$A[is.na(datos$A)]= modaA
datos

# imputando solo datos cuantitativos
tempData <- mice(datos,m=5,maxit=2,method="mean",seed=500,print=F)
newdata <- round(complete(tempData),0)
head(newdata,7)

# Recodificamos la variable A
newdata$A = factor(newdata$A, levels=c(1,2),labels=c("varon","mujer"))
newdata

# ejemplo, recodificando a numerico
d <- c("a","b","a")
d = factor(d, levels=c("a","b"),labels=c(1,2))
d = as.numeric(d)
str(d)

# imputando datos cualitativos y cuantitativos utilizando random forest
# imputa uno por uno en diferentes iteraciones
x <- c(13,14,15,NA,12,13,14)
y <- c(12,NA,NA,13,14,12,13)
z <- c(12,12,12,12,12,NA,13)
A <- c("varon","mujer","varon","varon","varon","mujer",NA) # cualitativa (2=varon, 1= mujer)
A <- as.factor(A)
datos <- data.frame(x,y,z,A)
datos
str(datos)

library(tidyverse)
imputed_data <- mice(datos%>%select(x,y,z,A), method = "cart")
Datos_imputados <- mice::complete(imputed_data)
colSums(is.na(Datos_imputados))
Datos_imputados

# imputando datos cualitativos y cuantitativos utilizando missForest
# imputa datos variablemente(azar)
x <- c(13,14,15,NA,12,13,14)
y <- c(12,NA,NA,13,14,12,13)
z <- c(12,12,12,12,12,NA,13)
A <- c("varon","mujer","varon","varon","varon","mujer",NA) # cualitativa (2=varon, 1= mujer)
A <- as.factor(A)
datos <- data.frame(x,y,z,A)
library(missForest)
imp1 <- missForest(datos,verbose =TRUE, variablewise=FALSE)
imp1$OOBerror # errores estimados (continuas y categoricas)
# error de cada variable
imp <- missForest(datos,verbose =TRUE, variablewise=TRUE)
imp$OOBerror # errores estimados (continuas y categoricas)
sapply(datos, class)

colSums(is.na(imp$ximp))
# guardar
dflimpio <- as.data.frame(imp$ximp)
dflimpio

comparacion <- cbind( Datos_imputados,dflimpio)
comparacion

############################################################################
# podemos tomar la desicion de eliminar la variables si
# esta cuenta con mas de 10% de datos faltantes
# existen muchos metodos de imputacion

# IMPUTAR DATOS NUMERICOS
# importando la data
mundo <- read.csv("mundo para imputar datos.csv",head=T, sep=";")
dim(mundo)
str(mundo)
head(mundo)
summary(mundo) # aparte de ver estadisticos descriptivos.. 
               # podemos ver datos faltantes 

# observando graficamnete los datos NAs 
#install.packages("VIM")
library(colorspace)
library(ggplot2)

library(VIM)
library(missForest)
library(datasets)
aggr(mundo,numbers=T,sortVar=T)

library(naniar)
gg_miss_var(mundo, show_pct = TRUE)

# identificacion de datos faltantes en analisis bi-variable
# observando variables lit_fema y calories
ggplot(
  data = mundo,
  mapping = aes(x = lit_fema, y = calories)) +     
  geom_miss_point()

#---------------------------------------
# Contar el total de NAs en la base de datos
sum(is.na(mundo))
# contar el numero de NAs por columna
colSums(is.na(mundo))

# la variable 12 (calories) tiene 34 datos NAs (31%)
# decidimos eliminar solo esa variable
nuevodataframe <- mundo[,-12] 
# observar el numero de NAs luego de eliminar
colSums(is.na(nuevodataframe))

#---------------------------------------
# Eliminar varias VARIABLES
mundo <- read.csv("mundo para imputar datos.csv",head=T, sep=";")
colSums(is.na(mundo))
nuevodataframe <- mundo[,c(-12,-23,-24)] 
# Saber el numero de NAs por columna
colSums(is.na(nuevodataframe))

#---------------------------------------
# Omitir las FILAS con observaciones NA
mundo <- read.csv("mundo para imputar datos.csv",head=T, sep=";")
colSums(is.na(mundo))

base1 <- na.omit(mundo)
colSums(is.na(base1))
str(base1) # se redujo a 59 observaciones (filas)

# -------------------------------------
# observando los NAs, usando la libreria Amelia
mundo <- read.csv("mundo para imputar datos.csv",head=T, sep=";")
library(Rcpp)
library(Amelia)
missmap(mundo)
misdate <- sort(sapply(mundo, function(x){sum(is.na(x)/length(x))}), decreasing = T)
round(misdate,2)
eliminados <- misdate[misdate > 0.1] # observando las variables que tienen mas del 10% de datos faltantes
eliminados #variables eliminadas

#--------------------------------------
# eliminando las variables con datos faltantes automaticamente
mundo <- read.csv("mundo para imputar datos.csv",head=T, sep=";")
colSums(is.na(mundo))
final <- mundo[, colMeans(is.na(mundo)) <= .10]
dim(final)
names(final)
str(final)
colSums(is.na(final))

##################################################
# crear una copia data1 <-data # en algunos casos nos puede servir
# ------------------------------------

# Imputacion automatica con la media aritmetica a toda la data
# para pocos datos faltantes
# Utilizando la data final
colSums(is.na(final))
library(base)
library(stats)
library(mice)

tempData <- mice(final,m=5,maxit=2,method="mean",seed=25)
newdata <- complete(tempData)
colSums(is.na(newdata))
# observar puntos imputados, ejm variable climate 
xyplot(tempData,cropgrow ~climate)
# observar puntos imputados, ejm variables aids 
xyplot(tempData,cropgrow ~aids)

# La imputacion con la media a variables con muchos datos faltantes,
# subestima la varianza, altera las relaciones entre las variables,
# sesga casi cualquier estimacion que no sea la media y sesga la estimacion de la media.

# graficando variables faltantes
par(mfrow=c(1,3))
plot(density(final$urban,na.rm = T),col=2,main="urban")+
lines(density(newdata$urban),col=3)
plot(density(final$alfabet,na.rm = T),col=2,main="alfabet")+
lines(density(newdata$alfabet),col=3)
plot(density(final$aids,na.rm = T),col=2,main="aids")+
lines(density(newdata$aids),col=3)

# Imputacion mediante regresion
mundo <- read.csv("mundo para imputar datos.csv",head=T, sep=";")
colSums(is.na(mundo))
tempData <- mice(mundo,m=5,maxit=2,method="norm.predict",seed=25)
newdata <- complete(tempData)
colSums(is.na(newdata))


# observando solo las que tienen mas datos faltantes
par(mfrow=c(1,3))
plot(density(mundo$calories,na.rm = T),col=2,main="calories")+
  lines(density(newdata$calories),col=3)
plot(density(mundo$lit_male,na.rm = T),col=2,main="lit_male")+
  lines(density(newdata$lit_male),col=3)
plot(density(mundo$lit_fema,na.rm = T),col=2,main="lit_fema")+
  lines(density(newdata$lit_fema),col=3)

# Imputacion mediante regresion estocastica
mundo <- read.csv("mundo para imputar datos.csv",head=T, sep=";")
colSums(is.na(mundo))
tempData <- mice(mundo,m=5,maxit=2,method="norm.nob",seed=500,print=F)
newdata <- complete(tempData,1)
colSums(is.na(newdata))

par(mfrow=c(1,3))
plot(density(mundo$calories,na.rm = T),col=2,main="calories")+
  lines(density(newdata$calories),col=3)
plot(density(mundo$lit_male,na.rm = T),col=2,main="lit_male")+
  lines(density(newdata$lit_male),col=3)
plot(density(mundo$lit_fema,na.rm = T),col=2,main="lit_fema")+
  lines(density(newdata$lit_fema),col=3)

# Imputacion multiple
mundo <- read.csv("mundo para imputar datos.csv",head=T, sep=";")
tempData <- mice(mundo,m=5,maxit=2,seed=500,print=F)
newdata <- complete(tempData,1)
colSums(is.na(newdata))

# Imputar datos con la mediana
mundo <- read.csv("mundo para imputar datos.csv",head=T, sep=";")
library(lattice)
library(ggplot2)
library(caret)
mundo.impute <- preProcess(mundo,method="medianImpute")
mundo.impute <- predict(mundo.impute,mundo) 
complete.data <- mice::complete(mundo)
complete.data <- mice::complete(mundo.impute)
dim(mundo.impute)
colSums(is.na(mundo.impute))
head(complete.data)


# Impute data con KNN
# KNN como algoritmo para imputar los valores perdidos
# a través de la medición de distancias Gower entre cada
# valor de la variable

# muestra datos estandarizados ojo
mundo <- read.csv("mundo para imputar datos.csv",head=T, sep=";")
colSums(is.na(mundo))
library(caret)
library(RANN)
mundo.impute <- preProcess(mundo,method="knnImpute")
mundo.impute <- predict(mundo.impute,mundo)
dim(mundo.impute)
colSums(is.na(mundo.impute))
head(mundo.impute)

# solo crea vase de datos con variables imputadas ojo
# imputacion con boostrap
library(tidyverse)
library(mice)
library(dplyr)
library(tidyr)
mundo <- read.csv("mundo para imputar datos.csv",head=T, sep=";")
colSums(is.na(mundo))
imputed_data <- mice(mundo %>% select(urban,alfabet,calories,aids,
                                 mortalidad, aids_rt,lg_aidsr,lit_male,
                                 lit_fema),
                     method = "norm.boot")
Data_Impu_boot <- mice::complete(imputed_data)
colSums(is.na(Data_Impu_boot))
head(Data_Impu_boot)


# ARBOL
mundo <- read.csv("mundo para imputar datos.csv",head=T, sep=";")
colSums(is.na(mundo))
imputed_data <- mice(mundo%>%select(urban,alfabet,calories,aids,
                                 mortalidad, aids_rt,lg_aidsr,lit_male,
                                 lit_fema), method = "cart")
Data_Impu_boot <- mice::complete(imputed_data)
colSums(is.na(Data_Impu_boot))


#RANDOM FOREST
mundo <- read.csv("mundo para imputar datos.csv",head=T, sep=";")
colSums(is.na(mundo))
imputed_data <- mice(mundo %>% select(urban,alfabet,calories,aids,
                                 mortalidad, aids_rt,lg_aidsr,lit_male,
                                 lit_fema), method = "rf")
Data_Impu_tree <- mice::complete(imputed_data)
sum(is.na(Data_Impu_tree))

#COMBINACION DE METODOS 
mundo <- read.csv("mundo para imputar datos.csv",head=T, sep=";")
colSums(is.na(mundo))
#imputed_data <- mice(mundo %>% select(urban,alfabet,calories,aids,
#                                      mortalidad, aids_rt,lg_aidsr,lit_male,
#                                 lit_fema), method = c("cart","norm"))
#Data_Impu_Mix <- mice::complete(imputed_data)
#sum(is.na(Data_Impu_Mix))



#########################################
# moda para variables categoricas
sexo <- c(1,2,NA,1,1,2,1)
estudios <- c(1,2,3,NA,1,2,3)
edad <- c(12,13,13,NA,12,13,12)
datos<- data.frame(sexo,estudios,edad)
datos

moda =getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

modasexo =moda(datos$sexo)
modasexo
modaestudios =moda(datos$estudios)
modaestudios

datos$sexo[is.na(datos$sexo)]= modasexo
datos
datos$estudios[is.na(datos$estudios)]= modasexo
datos


###########################################
# Imputacion para datos de series de tiempo  ojo
# Imputacion LOCF
# para datos longitudinales
data("AirPassengers")
serie=AirPassengers
serie[c(5,10,70)]<-NA
serie
library(imputeTS)
na.mean(serie)
serie

# Imputación de valores perdidos mediante médias móviles
data("AirPassengers")
serie=AirPassengers
serie[c(5,10,70)]<-NA
serie
na.ma(serie, 2) # k: medias movil
serie

# Imputación de valores perdidos mediante locf:
# option: locf para reemplazar con la anterior,
# nocb para reemplazar con la posterior
data("AirPassengers")
serie=AirPassengers
serie[c(5,10,70)]<-NA
serie
na_locf(serie, "nocb")

# Imputación de valores perdidos mediante interpolación:
data("AirPassengers")
serie=AirPassengers
serie[c(5,10,70)]<-NA
serie
na.interpolation(serie, option='linear')

# Imputación de valores perdidos mediante filtro de Kalman en modelos
# en el espacio de estados o estructurales:
data("AirPassengers")
serie=AirPassengers
serie[c(5,10,70)]<-NA
serie
na.kalman(serie, model="auto.arima", smooth=TRUE)

