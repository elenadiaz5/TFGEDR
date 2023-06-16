library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(modeest)
library(plyr)
library(dplyr)
library(readr)
library(tidyr)
library(readxl)


### CREACION DE LAS VARIABLES Y CARGA DE CSV -> ETL (EXTRACCIÓN Y PROCESAMIENTO DE LOS DATOS)


# 1. Gasto en pensiones VARIABLE OBJETIVO (en millones) -------------------

gasto_pensiones <- read_excel("~/Documents/TFG/datos/tablacompletapensiones.xlsx")

View(gasto_pensiones)

# verificamos el tipo de clase de las variables

class(gasto_pensiones$`% del PIB`)
class(gasto_pensiones$Año)

# cambiamos la variable Año a numerica

if (!any(is.na(as.numeric(gasto_pensiones$Año)))) {
  gasto_pensiones$Año <- as.numeric(gasto_pensiones$Año)
}

class(gasto_pensiones$Año) # comprobamos que se ha cambiado el tipo

View(gasto_pensiones)

names(gasto_pensiones)[names(gasto_pensiones) == "Año"] <- "año" # cambiamos el nombre a la columna 

names(gasto_pensiones)[names(gasto_pensiones) == "Total Gasto en Pensiones (en millones de euros)"] <- "total_gasto_pensiones" # cambiamos el nombre a la columna 

names(gasto_pensiones)[names(gasto_pensiones) == "% del PIB"] <- "porcentaje_PIB" # cambiamos el nombre a la columna 

names(gasto_pensiones)[names(gasto_pensiones) == "Número de Pensionistas (en miles)"] <- "numero_pensionistas" # cambiamos el nombre a la columna 


ggplot(data = gasto_pensiones, aes(x = año, y = total_gasto_pensiones)) + 
  geom_line(color = "navy") + 
  xlab("Año") + 
  ylab("Gasto en pensiones (en millones de euros)") + 
  ggtitle("Evolución del gasto en pensiones en España (2000-2021)")


# GRAFICO DEL % DEL PIB QUE SUPONE EL GASTO EN PENSIONES

porcentajePIBpensiones <- read_excel("~/Documents/TFG/datos/dataframe final.xlsx")
View(porcentajePIBpensiones)

names(porcentajePIBpensiones)

unicavarporcentaje <- porcentajePIBpensiones %>%
  select(Año, "% del PIB")

names(unicavarporcentaje)[names(unicavarporcentaje) == "% del PIB"] <- "var_porcentaje_PIB" # cambiamos el nombre a la columna 

view(unicavarporcentaje)

ggplot(data = unicavarporcentaje, aes(x = Año, y = var_porcentaje_PIB)) + 
  geom_line(color = "navy") + 
  xlab("Año") + 
  ylab("% del PIB") + 
  ggtitle("% del PIB que supone el gasto en pensiones en España (2000-2021)")


# METRICAS DE LA VARIABLE Y DEPENDIENTE

resumen <- summary(gasto_pensiones$total_gasto_pensiones)
print(resumen)

desviacion_estandar <- sd(gasto_pensiones$total_gasto_pensiones)

sd_gasto <- sd(gasto_pensiones$total_gasto_pensiones)
boxplot(gasto_pensiones$total_gasto_pensiones, main = "Distribución del gasto en pensiones en España (2000-2022)")
lines(c(1,1), c(mean(gasto_pensiones$total_gasto_pensiones) - sd_gasto, mean(gasto_pensiones$total_gasto_pensiones) + sd_gasto), col = "red")






# Calculamos los estadísticos descriptivos
resumen <- summary(gasto_pensiones$total_gasto_pensiones)
media <- mean(gasto_pensiones$total_gasto_pensiones)
desviacion <- sd(gasto_pensiones$total_gasto_pensiones)

# Creamos el histograma
hist(gasto_pensiones$total_gasto_pensiones, 
     main = "Histograma de Gasto en Pensiones",
     xlab = "Gasto en Pensiones (millones de euros)",
     col = "lightblue", border = "white",
     breaks = 15)

# Agregamos línea vertical para la media
abline(v = media, col = "red", lwd = 2)

# Agregamos texto con los estadísticos descriptivos
texto_estadisticos <- paste("Media:", round(media, 2), "\n",
                            "Desviación estándar:", round(desviacion, 2), "\n",
                            "Mínimo:", resumen[1], "\n",
                            "1er Cuartil:", resumen[2], "\n",
                            "Mediana:", resumen[3], "\n",
                            "3er Cuartil:", resumen[5], "\n",
                            "Máximo:", resumen[6])
text(135000, 250, texto_estadisticos, pos = 4, cex = 1.2)




# 2. Población activa en España -------------------------------------------

poblacionactiva <- read_excel("~/Documents/TFG/datos/poblacion activa/poblacionactiva.xlsx")

View(poblacionactiva)

poblacionactiva <- na.omit(poblacionactiva) # quitamos los valores NA

poblacionactiva <- poblacionactiva %>% # borramos columnas innecesarias
  select(-Edad)

poblacionactiva <- poblacionactiva %>% # borramos columnas innecesarias
  select(-Sexo)

poblacionactiva <- poblacionactiva %>% # borramos columnas innecesarias
  select(-Unidad)

poblacionactiva <- poblacionactiva %>% # borramos columnas innecesarias
  select(-Periodo)

poblacionactiva <- poblacionactiva %>% # borramos columnas innecesarias
  select(-Total)

# comprobacion tipo de variable
class(poblacionactiva$poblacion_activa)


ggplot(data = poblacionactiva, aes(x = año, y = poblacion_activa )) + 
  geom_line(color = "navy") + 
  xlab("Año") + 
  ylab("Población activa (en miles)") + 
  ggtitle("Población activa en España (2000-2021)") +
          theme(plot.title = element_text(hjust = 0.5))



# 3. Numero de parados ----------------------------------------------------

evolucion_numero_parados <- read_delim("~/Documents/TFG/datos/numero parados/evolucion_del_numero_de_parados_en_españa.csv", 
                                                        ";", escape_double = FALSE, trim_ws = TRUE)
View(evolucion_numero_parados)

evolucion_numero_parados <- evolucion_numero_parados %>% # borramos columnas innecesarias
  select(-Año)

evolucion_numero_parados <- evolucion_numero_parados %>% # borramos columnas innecesarias
  select(-Periodo)

evolucion_numero_parados <- evolucion_numero_parados %>% # borramos columnas innecesarias
  select(-`Personas en paro`)

evolucion_numero_parados <- na.omit(evolucion_numero_parados) # quitamos los valores NA

names(evolucion_numero_parados)[names(evolucion_numero_parados) == "Media parados por año"] <- "numero_parados" # cambiamos el nombre a la columna 


ggplot(data = evolucion_numero_parados, aes(x = año, y = numero_parados)) + 
  geom_line(color = "navy") + 
  xlab("Año") + 
  ylab("Numero de parados (en millones)") + 
  ggtitle("Numero de parados en España (2000-2021)") +
  theme(plot.title = element_text(hjust = 0.5))




# 4. Poblacion ocupada en España ------------------------------------------

poblacionocupada <- read_delim("~/Documents/TFG/datos/poblacionocupada.csv", 
                                                         ";", escape_double = FALSE, trim_ws = TRUE)
View(poblacionocupada)

poblacionocupada <- na.omit(poblacionocupada)


poblacionocupada <- poblacionocupada %>% # borramos columnas innecesarias
  select(-Año)

poblacionocupada <- poblacionocupada %>% # borramos columnas innecesarias
  select(-Periodo)

poblacionocupada <- poblacionocupada %>% # borramos columnas innecesarias
  select(-`Personas ocupadas`)

names(poblacionocupada)[names(poblacionocupada) == "Poblacion ocupada"] <- "poblacion_ocupada" # cambiamos el nombre a la columna 

ggplot(data = poblacionocupada, aes(x = año, y = poblacion_ocupada)) + 
  geom_line(color = "navy") + 
  xlab("Año") + 
  ylab("Poblacion ocupada (en miles)") + 
  ggtitle("Población ocupada en España del año 2000 al 2021") +
  theme(plot.title = element_text(hjust = 0.5))




# 5. Poblacion total en España --------------------------------------------

poblaciontotal <- read_delim("~/Documents/TFG/datos/poblaciontotal.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)
View(poblaciontotal)


poblaciontotal <- poblaciontotal %>% # borramos columnas innecesarias
  select(-X3)

poblaciontotal <- poblaciontotal %>% # borramos columnas innecesarias
  select(-X2)

poblaciontotal <- poblaciontotal %>% # borramos columnas innecesarias
  select(-`Datos de series`)

poblaciontotal <- na.omit(poblaciontotal) # quitamos los valores NA

names(poblaciontotal)[names(poblaciontotal) == "X4"] <- "año" # cambiamos el nomnbre a la columna 
names(poblaciontotal)[names(poblaciontotal) == "X5"] <- "poblacion_total" # cambiamos el nomnbre a la columna 

poblaciontotal <- poblaciontotal[-1,] # elimino la primera fila porque es el nombre de la variable
poblaciontotal <- head(poblaciontotal, -1) # elimino la ultima fila (2022) porque no lo necesitamos

class(poblaciontotal$poblacion_total) # vemos que la variable poblacion_total es tipo chr 

if (!any(is.na(as.numeric(poblaciontotal$poblacion_total)))) {
  poblaciontotal$poblacion_total <- as.numeric(poblaciontotal$poblacion_total) # cambiamos de chr a num
}

class(poblaciontotal$año) # vemos que año es chr lo pasamos a num

if (!any(is.na(as.numeric(poblaciontotal$año)))) {
  poblaciontotal$año <- as.numeric(poblaciontotal$año) # cambiamos de chr a num
}


ggplot(data = poblaciontotal, aes(x = año, y = poblacion_total)) + 
  geom_line(color = "navy") + 
  xlab("Año") + 
  ylab("Poblacion total") + 
  ggtitle("Población total en España (2000 - 2021)") +
  theme(plot.title = element_text(hjust = 0.5))




# 6. Esperanza de vida ----------------------------------------------------

esperanzavida <- read_delim("~/Documents/TFG/datos/esperanzavida.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE)
View(esperanzavida)

esperanzavida <- esperanzavida %>% # borramos columnas innecesarias
  select(-X2)
esperanzavida <- esperanzavida %>% # borramos columnas innecesarias
  select(-X3)
esperanzavida <- esperanzavida %>% # borramos columnas innecesarias
  select(-X4)
esperanzavida <- esperanzavida %>% # borramos columnas innecesarias
  select(-`Datos de series`)

esperanzavida <- na.omit(esperanzavida) # quitamos los valores NA

esperanzavida <- esperanzavida[-1,] # elimino la primera fila porque es el nombre de la variable


names(esperanzavida)[names(esperanzavida) == "X5"] <- "año" # cambiamos el nomnbre a la columna 
names(esperanzavida)[names(esperanzavida) == "X6"] <- "esperanza_vida" # cambiamos el nomnbre a la columna 

class(esperanzavida$año) # cambiamos de tipo chr a num

if (!any(is.na(as.numeric(esperanzavida$año)))) {
  esperanzavida$año <- as.numeric(esperanzavida$año) # cambiamos de chr a num
}

class(esperanzavida$esperanza_vida) 
if (!any(is.na(as.numeric(esperanzavida$esperanza_vida)))) {
  esperanzavida$esperanza_vida <- as.numeric(esperanzavida$esperanza_vida) # cambiamos de chr a num
}


ggplot(data = esperanzavida, aes(x = año, y = esperanza_vida )) + 
  geom_line(color = "navy") + 
  xlab("Año") + 
  ylab("Esperanza de vida (edad)") + 
  ggtitle("Esperanza de vida (2000-2021)") +
  theme(plot.title = element_text(hjust = 0.5))


# 7. Evolucion del PIB ----------------------------------------------------

evolucion_del_pib_de_españa <- read_delim("~/Documents/TFG/datos/evolucion_del_pib_de_españa.csv", 
                                          ";", escape_double = FALSE, trim_ws = TRUE)
View(evolucion_del_pib_de_españa)

evolucion_del_pib_de_españa <- na.omit(evolucion_del_pib_de_españa) # quitamos los valores NA

evolucion_del_pib_de_españa <- evolucion_del_pib_de_españa %>% # borramos columnas innecesarias
  select(-Periodo)

evolucion_del_pib_de_españa <- evolucion_del_pib_de_españa[-1,] # elimino las 4 primeras filas porque no necesito esos años
evolucion_del_pib_de_españa <- evolucion_del_pib_de_españa[-1,] # elimino las 4 primeras filas porque no necesito esos años
evolucion_del_pib_de_españa <- evolucion_del_pib_de_españa[-1,] # elimino las 4 primeras filas porque no necesito esos años
evolucion_del_pib_de_españa <- evolucion_del_pib_de_españa[-1,] # elimino las 4 primeras filas porque no necesito esos años


names(evolucion_del_pib_de_españa)[names(evolucion_del_pib_de_españa) == "Tasa de variación interanual del PIB"] <- "evolucion_PIB" # cambiamos el nomnbre a la columna 
names(evolucion_del_pib_de_españa)[names(evolucion_del_pib_de_españa) == "Año"] <- "año" # cambiamos el nomnbre a la columna 


evolucion_del_pib_de_españa$evolucion_PIB<- as.numeric(gsub(",", ".", evolucion_del_pib_de_españa$evolucion_PIB)) # cambiamos la , por . a los decimales

class(evolucion_del_pib_de_españa$año) # tipo de variable chr o num

if (!any(is.na(as.numeric(evolucion_del_pib_de_españa$año)))) {
  evolucion_del_pib_de_españa$año <- as.numeric(evolucion_del_pib_de_españa$año)
}


ggplot(data = evolucion_del_pib_de_españa, aes(x = año, y = evolucion_PIB)) + 
  geom_line(color = "navy") + 
  xlab("Año") + 
  ylab("Evolución del PIB (%)") + 
  ggtitle("Evolución del PIB en España (2000-2021)") +
  theme(plot.title = element_text(hjust = 0.5))




# 8. Salario medio en España ----------------------------------------------

## objetivo de cálculo: PESO SALARIAL SOBRE EL PIB

salario_medio_anual <- read_delim("~/Documents/TFG/datos/salariomedioespaña.csv", 
                                  ";", escape_double = FALSE, trim_ws = TRUE)
View(salario_medio_anual)

salario_medio_anual <- na.omit(salario_medio_anual) # quitamos los valores NA

salario_medio_anual <- salario_medio_anual %>% # borramos columnas innecesarias
  select(-Periodo)

names(salario_medio_anual)[names(salario_medio_anual) == "Año"] <- "año" # cambiamos el nomnbre a la columna 
names(salario_medio_anual)[names(salario_medio_anual) == "Salarios"] <- "salario_medio" # cambiamos el nomnbre a la columna 

class(salario_medio_anual$año) # el tipo es chr, lo cambiamos a num

if (!any(is.na(as.numeric(salario_medio_anual$año)))) {
  salario_medio_anual$año <- as.numeric(salario_medio_anual$año)
}

class(salario_medio_anual$salario_medio)


ggplot(data = salario_medio_anual, aes(x = año, y = salario_medio)) + 
  geom_line(color = "navy") + 
  xlab("Año") + 
  ylab("Salario medio (euros)") + 
  ggtitle("Salario medio en España (2000-2021)") +
  theme(plot.title = element_text(hjust = 0.5))


# 9. Tasa de natalidad ----------------------------------------------------

tasadenatalidad <- read_excel("~/Documents/TFG/datos/tasadenatalidad.xls")
View(tasadenatalidad)

names(tasadenatalidad)[names(tasadenatalidad) == "tasa de natalidad"] <- "tasa_natalidad" # cambiamos el nomnbre a la columna 

ggplot(data = tasadenatalidad, aes(x = año, y = tasa_natalidad)) + 
  geom_line(color = "navy") + 
  xlab("Año") + 
  ylab("Tasa de natalidad (por mil habitantes)") + 
  ggtitle("Tasa de natalidad en España (2000-2021)") +
  theme(plot.title = element_text(hjust = 0.5))



# 10. Tasa de mortalidad --------------------------------------------------

tasademortalidad <- read_excel("~/Documents/TFG/datos/tasademortalidad.xlsx")
View(tasademortalidad)

names(tasademortalidad)[names(tasademortalidad) == "tasa de mortalidad"] <- "tasa_mortalidad" # cambiamos el nomnbre a la columna 


ggplot(data = tasademortalidad, aes(x = año, y = tasa_mortalidad)) + 
  geom_line(color = "navy") + 
  xlab("Año") + 
  ylab("Tasa de mortalidad (por mil habitantes)") + 
  ggtitle("Tasa de mortalidad en España (2000-2021)") +
  theme(plot.title = element_text(hjust = 0.5))


# 11. Edad de jubilacion --------------------------------------------------




# 12. Inflacion  ----------------------------------------------------------

variacion_inflacion <- read_delim("~/Documents/TFG/datos/variacion_interanual_del_ipc_en_españa_desde_2003.csv", 
                                                                ";", escape_double = FALSE, trim_ws = TRUE)
View(variacion_inflacion)


variacion_inflacion <- na.omit(variacion_inflacion) # quitamos los valores NA

variacion_inflacion <- variacion_inflacion %>% # borramos columnas innecesarias
  select(-Periodo)

names(variacion_inflacion)

names(variacion_inflacion)[names(variacion_inflacion) == "Año"] <- "año" # cambiamos el nomnbre a la columna 
names(variacion_inflacion)[names(variacion_inflacion) == "IPC general"] <- "ipc_inflacion" # cambiamos el nomnbre a la columna 

class(variacion_inflacion$año)

if (!any(is.na(as.numeric(variacion_inflacion$año)))) {
  variacion_inflacion$año <- as.numeric(variacion_inflacion$año) # cambiamos de chr a num
}

class(variacion_inflacion$ipc_inflacion)

ggplot(data = variacion_inflacion, aes(x = año, y = ipc_inflacion)) + 
  geom_line(color = "navy") + 
  xlab("Año") + 
  ylab("Tasa de inflacion (IPC)") + 
  ggtitle("Variación de la tasa de inflación en España (2000-2023)") +
  theme(plot.title = element_text(hjust = 0.5))



# 13. Tasa de inmigración -------------------------------------------------

tasainmigracion <- read_excel("~/Documents/TFG/datos/tasainmigracion.xlsx")
View(tasainmigracion)

class(tasainmigracion$año)
class(tasainmigracion$tasa_inmigracion)

ggplot(data = tasainmigracion, aes(x = año, y = tasa_inmigracion)) + 
  geom_line(color = "navy") + 
  xlab("Año") + 
  ylab("Tasa de inmigración (por mil habitantes)") + 
  ggtitle("Tasa bruta de inmigración en España (2000-2021)") +
  theme(plot.title = element_text(hjust = 0.5))



# 14. Gasto Público en España ---------------------------------------------

gastopublicototal <- read_excel("~/Documents/TFG/datos/gastopublicototal.xlsx")
View(gastopublicototal)

gastopublicototal <- na.omit(gastopublicototal) # quitamos los valores NA

class(gastopublicototal$año) # cambiamos el tipo chr a num

if (!any(is.na(as.numeric(gastopublicototal$año)))) {
  gastopublicototal$año <- as.numeric(gastopublicototal$año) # cambiamos de chr a num
}

class(gastopublicototal$gasto_publico)

ggplot(data = gastopublicototal, aes(x = año, y = gasto_publico)) + 
  geom_line(color = "navy") + 
  xlab("Año") + 
  ylab("Gasto público total (en millones de euros)") + 
  ggtitle("Gasto público total en España (2000-2021)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::comma)


# 15. Numero de pensionistas en España ------------------------------------

numeropensionistas <- read_excel("~/Documents/TFG/datos/tablacompletapensiones.xlsx")
View(numeropensionistas)

# elimino las columnas innecesarias

names(numeropensionistas)

numeropensionistas <- numeropensionistas %>% # borramos columnas innecesarias
  select(-"% del PIB")

numeropensionistas <- numeropensionistas %>% # borramos columnas innecesarias
  select(-"Total Gasto en Pensiones (en millones de euros)")


# cambiamos el nombre a las columnas

names(numeropensionistas)[names(numeropensionistas) == "Año"] <- "año"
names(numeropensionistas)[names(numeropensionistas) == "Número de Pensionistas (en miles)"] <- "numero_pensionistas"


# verificamos el tipo de clase de las variables

class(numeropensionistas$año)
class(numeropensionistas$numero_pensionistas)

ggplot(data = numeropensionistas, aes(x = año, y = numero_pensionistas)) + 
  geom_line(color = "navy") + 
  xlab("Año") + 
  ylab("Número de pensionistas (en millones)") + 
  ggtitle("Número de pensionistas en España (2000-2021)") +
  theme(plot.title = element_text(hjust = 0.5)) 







# DATAFRAME FINAL ---------------------------------------------------------

nuevo_dataframe <- cbind(gasto_pensiones,poblacionactiva$poblacion_activa)

view(nuevo_dataframe)




# VARIACIONES FORMATO VARIABLE ------------------------------------------------------------

## variacion interanual de la ocupacion (2012-2022)

variacion_interanual_de_la_ocupacion <- read_delim("~/Documents/TFG/datos/variacion_interanual_de_la_ocupacion.csv", 
                                                   ";", escape_double = FALSE, trim_ws = TRUE)
View(variacion_interanual_de_la_ocupacion)

variacion_interanual_de_la_ocupacion <- na.omit(variacion_interanual_de_la_ocupacion) # quitamos los valores NA


## variacion interanual del empleo (2004-2022)

variacion_interanual_empleo <- read_delim("~/Documents/TFG/datos/variacion_interanual_del_empleo_en_españa.csv", 
                                                        ";", escape_double = FALSE, trim_ws = TRUE)
View(variacion_interanual_empleo)

variacion_interanual_empleo <- na.omit(variacion_interanual_empleo) # quitamos los valores NA




