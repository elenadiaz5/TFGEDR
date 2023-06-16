# MODELO ARIMA POBLACION --------------------------------------------------

#Cargamos librerias
library(readxl)
library(forecast)
library(tseries)
library(dplyr)
library(foreign)
library(lmtest)

## SERIE TEMPORAL VARIABLE POBLACION NACIONAL DESDE EL AÑO 1971 HASTA 2021 (SEMESTRAL)
#Leemos el archivo y los metemos en la variable "datos"
serie1 <- read_excel("~/Documents/TFG/datos/poblacionespañolaINE.xlsx")
View(serie1)


#Obtenemos una serie temporal de la columna donde aparecen nuestros datos
serie.ts1 = ts(serie1$`Total Poblacion Nacional`, start = c(1971,1), frequency = 2)
print(serie.ts1)
plot(serie.ts1)

#Descomponer una serie
serie.ts1.desc = decompose(serie.ts1)
plot(serie.ts1.desc, xlab='Año')

#Lo primero que hacemos es comprobar los estadísticos básicos
summary(serie.ts1)

#Graficamos la serie
plot(serie.ts1)
#podemos comprobar que se trata de una serie que no es estacionaria en media, dado que presenta una 
#tendencia claramente creciente. Por otro lado, podemos ver que la serie no presenta gran variabilidad ?, 
#sin embargo, aplicamos logaritmos para poder eliminar la posible no estacionariedad en varianza.


# Aplicamos logaritmos a la serie para reducir la varianza de esta.
serie.ts1_log <- log(serie.ts1)


#Volvemos a verificar si la serie transformada sigue siendo o no estacionaria en varianza.
#Para ello graficamos.
plot(serie.ts1_log)

#Hacemos dickey fuller para comprobarlo
#H0: la serie  posee raíces unitarias (no es estacionaria).
#H1: La serie es estacionaria
adf.test(serie.ts1_log)
# p-valor(0.01) < nivel de significación(0.05). Por tanto, es estacionaria y no es necesario aplicar diferencias
# Por tanto, rechazamos la hipótesis nula y la serie es estacionaria


#IDENTIFICACIÓN DEL MODELO
#Correlograma
par(mfrow=c(1,2))
acf(serie.ts1_log)
pacf(serie.ts1_log)

arima1 <- auto.arima(serie.ts1_log)
print(arima1)

modelo1 <- arima(serie.ts1_log, order=c(1,1,1))
modelo2 <- arima(serie.ts1_log, order=c(1,1,0))
modelo3 <- arima(serie.ts1_log, order=c(2,1,1)) # no valido
modelo4 <- arima(serie.ts1_log, order=c(2,1,0))

AIC(modelo1) # modelo optimo
AIC(modelo2)
AIC(modelo3) # no valido
AIC(modelo4)

coef(modelo1)
coeftest(modelo1)


pronostico1 <- forecast(serie.ts1, h=1)



# MODELO ARIMA PIB --------------------------------------------------------

# PIB a precios constantes y de mercado (1980-2020)
#Leemos el archivo y los metemos en la variable "datos"
serie2 <- read_excel("~/Documents/TFG/datos/PIB serie temporal.xlsx")
View(serie2)

serie2$`PIB Nacional`
#Obtenemos una serie temporal de la columna donde aparecen nuestros datos
serie.ts2 = ts(serie2$`PIB Nacional`, start = c(1980,1), frequency = 4)
print(serie.ts2)
plot(serie.ts2)

#Descomponer una serie
serie.ts2.desc = decompose(serie.ts2)
plot(serie.ts2.desc, xlab='Año')

#Lo primero que hacemos es comprobar los estadísticos básicos
summary(serie.ts2) 

#Graficamos la serie
plot(serie.ts2)
#podemos comprobar que se trata de una serie que no es estacionaria en media, dado que presenta una 
#tendencia claramente creciente. Por otro lado, podemos ver que la serie no presenta gran variabilidad ?, 
#sin embargo, aplicamos logaritmos para poder eliminar la posible no estacionariedad en varianza.


# Aplicamos logaritmos a la serie para reducir la varianza de esta.
serie.ts2_log <- log(serie.ts2)


#Volvemos a verificar si la serie transformada sigue siendo o no estacionaria en varianza.
#Para ello graficamos.
plot(serie.ts2_log)

#Hacemos dickey fuller pa comprobarlo
#H0: la serie  posee raíces unitarias (no es estacionaria).
#H1: La serie es estacionaria
adf.test(serie.ts2_log)
# p-valor(0.9855) > nivel de significación(0.05). Por tanto, no es estacionaria

#Aplicamos primeras diferencias para ver si conseguimos la estacionariedad en la media.
d.serie2 <- diff(serie.ts2_log)
plot(d.serie2)

#Dickey Fuller
adf.test(d.serie2)
# p-valor(0.1038) > nivel de significación(0.05). Por tanto, no es estacionaria todavia

#Aplicamos segundas diferencias para ver si conseguimos la estacionariedad en la media.

serie.ts2_log2 <- log(serie.ts2_log)
d2.serie2 <- diff(serie.ts2_log2)
plot(d2.serie2)

#Dickey Fuller
adf.test(d2.serie2)
# p-valor(0.08514) > nivel de significación(0.05). Por tanto, rechazamos 
# la hipótesis nula y la serie es estacionaria


#Aplicamos terceras diferencias para ver si conseguimos la estacionariedad en la media.

serie.ts2_log3 <- log(serie.ts2_log2)
d3.serie2 <- diff(serie.ts2_log3)
plot(d3.serie2)

#Dickey Fuller
adf.test(d3.serie2)
# p-valor(0.08232) > nivel de significación(0.05). Por tanto, rechazamos 
# la hipótesis nula y la serie es estacionaria

#Aplicamos cuartas diferencias para ver si conseguimos la estacionariedad en la media.

serie.ts2_log4 <- log(serie.ts2_log3)
d4.serie2 <- diff(serie.ts2_log4)
plot(d4.serie2)

#Dickey Fuller
adf.test(d4.serie2)
# p-valor(0.08232) > nivel de significación(0.05). Por tanto, rechazamos 
# la hipótesis nula y la serie es estacionaria


# PREGUNTA: despues de apicar 4 diferencias sigue sin ser estacionaria????


#IDENTIFICACIÓN DEL MODELO
#Correlograma
par(mfrow=c(1,2))
acf(d4.serie2)
pacf(d4.serie2)

arimaPIB <- auto.arima(d.serie2)
print(arimaPIB)


modeloPIB1 <- arima(d.serie2, order=c(1,1,1))
modeloPIB2 <- arima(d.serie2, order=c(1,1,0))
modeloPIB3 <- arima(d.serie2, order=c(2,1,1))
modeloPIB4 <- arima(d.serie2, order=c(2,1,0))


AIC(modeloPIB1)
AIC(modeloPIB2)
AIC(modeloPIB3)
AIC(modeloPIB4)

coef(modeloPIB3)
coeftest(modeloPIB3)



# MODELO ARIMA GASTO PENSIONES --------------------------------------------

# falta terminarlo, se requiere de frecuencia 2 (mas datos)

#Leemos el archivo y los metemos en la variable "datos"
serie3 <- read_excel("~/Documents/TFG/datos/dataframe final.xlsx")
View(serie3)

#Obtenemos una serie temporal de la columna donde aparecen nuestros datos
serie.ts3 = ts(serie3$`Total Gasto en Pensiones (en millones de euros)`, start = c(1985,1), frequency = 1)
print(serie.ts3)
plot(serie.ts3)

#Descomponer una serie
serie.ts3.desc = decompose(serie.ts3)
plot(serie.ts3.desc, xlab='Año')

#Lo primero que hacemos es comprobar los estadísticos básicos
summary(serie.ts3)

#Graficamos la serie
plot(serie.ts3)
#podemos comprobar que se trata de una serie que no es estacionaria en media, dado que presenta una 
#tendencia claramente creciente. Por otro lado, podemos ver que la serie no presenta gran variabilidad ?, 
#sin embargo, aplicamos logaritmos para poder eliminar la posible no estacionariedad en varianza.


# Aplicamos logaritmos a la serie para reducir la varianza de esta.
serie.ts3_log <- log(serie.ts3)


#Volvemos a verificar si la serie transformada sigue siendo o no estacionaria en varianza.
#Para ello graficamos.
plot(serie.ts3_log)

#Hacemos dickey fuller pa comprobarlo
#H0: la serie  posee raíces unitarias (no es estacionaria).
#H1: La serie es estacionaria
adf.test(serie.ts3_log)
# p-valor(0.43) > nivel de significación(0.05). Por tanto, no es estacionaria

#Aplicamos primeras diferencias para ver si conseguimos la estacionariedad en la media.
d.serie3 <- diff(serie.ts3_log)
plot(d.serie3)

#Dickey Fuller
adf.test(d.serie3)
# p-valor(0.01) < nivel de significación(0.05). Por tanto, rechazamos 
# la hipótesis nula y la serie es estacionaria


#IDENTIFICACIÓN DEL MODELO
#Correlograma
par(mfrow=c(1,2))
acf(d.serie3)
pacf(d.serie3)

arimagastopens <- auto.arima(d.serie3)
print(arima)


modelo1 <- arima(d.serie, order=c(1,1,1))
modelo2 <- arima(d.serie, order=c(1,1,0))
modelo3 <- arima(d.serie, order=c(2,1,1))
modelo4 <- arima(d.serie, order=c(2,1,0))


AIC(modelo1)
AIC(modelo2)
AIC(modelo3)
AIC(modelo4)

coef(modelo3)
coeftest(modelo3)