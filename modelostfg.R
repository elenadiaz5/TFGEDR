
# DATAFRAME pequeño CON TODAS LAS VARIABLES ---------------------------------

dataframe <- read_excel("~/Documents/TFG/datos/dataframe.xlsx")
View(dataframe)

names(dataframe)

names(dataframe)[which(names(dataframe) == "% del PIB")] <- "porcentaje_PIB"
names(dataframe)[which(names(dataframe) == "Año")] <- "año"
names(dataframe)[which(names(dataframe) == "Total Gasto en Pensiones (en millones de euros)")] <- "total_gasto_pensiones"
names(dataframe)[which(names(dataframe) == "Número de Pensionistas (en miles)")] <- "numero_pensionistas"
names(dataframe)[which(names(dataframe) == "Poblacion activa")] <- "poblacion_activa"
names(dataframe)[which(names(dataframe) == "Numero de parados")] <- "numero_parados"
names(dataframe)[which(names(dataframe) == "Poblacion ocupada")] <- "poblacion_ocupada"
names(dataframe)[which(names(dataframe) == "Poblacion total")] <- "poblacion_total"
names(dataframe)[which(names(dataframe) == "Esperanza vida")] <- "esperanza_vida"
names(dataframe)[which(names(dataframe) == "Evolucion del PIB")] <- "evolucion_PIB"
names(dataframe)[which(names(dataframe) == "Salario medio")] <- "salario_medio"


df <- na.omit(dataframe) #elimino el año 2022 por falta de datos en todas las variables
view(df)


# ESTUDIO DE CORRELACIONES ------------------------------------------------
y <- df$`Total Gasto en Pensiones (en millones de euros)` # nombro y a la variable Gasto en pensiones

## 1. correlacion de la variable independiente y (gasto en pensiones) con la variable Año

cor(df$Año, y)

# diagrama de dispersion (la relación entre cada variable independiente y la variable dependiente)

plot(df$Año, y, xlab = "Año", ylab = "Total Gasto en Pensiones (en millones de euros)")


## 2. correlacion de la variable independiente y (gasto en pensiones) con la variable % del PIB

cor(df$`% del PIB`, y)

# diagrama de dispersion (la relación entre cada variable independiente y la variable dependiente)

plot(df$`% del PIB`, y, xlab = "% del PIB", ylab = "Total Gasto en Pensiones (en millones de euros)")


## 3. correlacion de la variable independiente y (gasto en pensiones) con la variable Numero de pensionistas (en miles)

cor(df$`Número de Pensionistas (en miles)`, y)

# diagrama de dispersion (la relación entre cada variable independiente y la variable dependiente)

plot(df$`Número de Pensionistas (en miles)`, y, xlab = "Numero de pensionistas (en miles)", ylab = "Total Gasto en Pensiones (en millones de euros)")


## 4. correlacion de la variable independiente y (gasto en pensiones) con la variable Poblacion activa

cor(df$`Poblacion activa`, y)

# diagrama de dispersion (la relación entre cada variable independiente y la variable dependiente)

plot(df$`Poblacion activa`, y, xlab = "Poblacion activa", ylab = "Total Gasto en Pensiones (en millones de euros)")

## 5. correlacion de la variable independiente y (gasto en pensiones) con la variable Numero de parados

cor(df$`Numero de parados`, y)

# diagrama de dispersion (la relación entre cada variable independiente y la variable dependiente)

plot(df$`Numero de parados`, y, xlab = "Numero de parados", ylab = "Total Gasto en Pensiones (en millones de euros)")


## 6. correlacion de la variable independiente y (gasto en pensiones) con la variable Poblacion ocupada

cor(df$`Poblacion ocupada`, y)

# diagrama de dispersion (la relación entre cada variable independiente y la variable dependiente)

plot(df$`Poblacion ocupada`, y, xlab = "Poblacion ocupada", ylab = "Total Gasto en Pensiones (en millones de euros)")


## 7. correlacion de la variable independiente y (gasto en pensiones) con la variable Poblacion total

cor(df$`Poblacion total`, y)

# diagrama de dispersion (la relación entre cada variable independiente y la variable dependiente)

plot(df$`Poblacion total`, y, xlab = "Poblacion total", ylab = "Total Gasto en Pensiones (en millones de euros)")


## 8. correlacion de la variable independiente y (gasto en pensiones) con la variable Esperanza de vida

cor(df$`Esperanza vida`, y)

# diagrama de dispersion (la relación entre cada variable independiente y la variable dependiente)

plot(df$`Esperanza vida`, y, xlab = "Esperanza de vida", ylab = "Total Gasto en Pensiones (en millones de euros)")


## 9. correlacion de la variable independiente y (gasto en pensiones) con la variable Evolucion del PIB

cor(df$`Evolucion del PIB`, y)

# diagrama de dispersion (la relación entre cada variable independiente y la variable dependiente)

plot(df$`Evolucion del PIB`, y, xlab = "Evolucion del PIB", ylab = "Total Gasto en Pensiones (en millones de euros)")


## 10. correlacion de la variable independiente y (gasto en pensiones) con la variable Salario medio

cor(df$`Salario medio`, y)

# diagrama de dispersion (la relación entre cada variable independiente y la variable dependiente)

plot(df$`Salario medio`, y, xlab = "Salario medio", ylab = "Total Gasto en Pensiones (en millones de euros)")

library(GGally)

# Hacer un gráfico de dispersión para cada combinación de variables
ggpairs(df, columns = 1:3) 

# Matriz de correlación
ggcorr(df, label = TRUE, size = 3, hjust = 0.9, vjust = 0.9)



# MODELO REGRESION LINEAL MULTIPLE DATAFRAME PEQUEÑO ----------------------------------------

modeloregresion <- lm(`total_gasto_pensiones` ~ año + `porcentaje_PIB` + `numero_pensionistas` + `poblacion_activa` + `numero_parados` +
                        `poblacion_ocupada` + `poblacion_total` + `esperanza_vida` + `evolucion_PIB` + `salario_medio` , data = dataframe)

names(dataframe)

summary (modeloregresion)


# Crear un data frame con las observaciones y los valores ajustados del modelo
datos_grafico <- data.frame(Observado = modeloregresion$model$total_gasto_pensiones,
                            Ajustado = predict(modeloregresion))

# Crear un gráfico de dispersión con los valores observados y los valores ajustados
ggplot(datos_grafico, aes(x = Observado, y = Ajustado)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "Valores observados", y = "Valores ajustados", title = "Valores observados vs. Valores ajustados")


# METRICAS DE ERROR

resumen_modelo <- summary(modeloregresion)

# Mostrar el MSE
mse <- mean(resumen_modelo$residuals^2)
cat("MSE: ", mse, "\n")

# Mostrar el R2
r2 <- resumen_modelo$r.squared
cat("R2: ", r2, "\n")



plot(modeloregresion$residuales, type = "p")
plot(resid(modeloregresion) ~ dataframe$Año)
shapiro.test(modeloregresion$residuales)

library(lmtest)
bptest(modeloregresion) # bien


# GRAFICO DE RESIDUOS

# Generar los residuos del modelo de regresión
residuos <- residuals(modeloregresion)

# Graficar los residuos contra los valores ajustados
plot(modeloregresion$fitted.values, residuos, 
     xlab = "Valores ajustados", ylab = "Residuos",
     main = "Gráfico de residuos")

# Agregar una línea horizontal en y = 0 para visualizar el valor cero
abline(h = 0, col = "red")

## histograma

# Generar los residuos del modelo de regresión

# Graficar un histograma de los residuos
hist(residuos, main = "Histograma de los residuos", 
     xlab = "Residuos", ylab = "Frecuencia")



# PREDICCIONES

nuevos.datos <- data.frame(año = 2024, `porcentaje_PIB` = 0.156, `numero_pensionistas` = 12100, 
                           `poblacion_activa` = 24000, `numero_parados` = 3014800, `poblacion_ocupada` = 20490650,
                           `poblacion_total` = 47623964, `esperanza_vida` = 84, `evolucion_PIB` = 3,
                           `salario_medio` = 25497.00)
names(dataframe)

view(nuevos.datos)


prediccion <- predict(modeloregresion, nuevos.datos)








# prueba de un grafico

ggplot(modeloregresion$model, aes(x = ingreso_percapita, y = gasto_pensiones)) +
  geom_point(aes(color = "ingreso per cápita", shape = "ingreso per cápita"), size = 3) +
  geom_point(aes(x = porcentaje_poblacion_mayor_65, color = "porcentaje de población mayor de 65 años", shape = "porcentaje de población mayor de 65 años"), size = 3) +
  geom_point(aes(x = esperanza_vida, color = "esperanza de vida", shape = "esperanza de vida"), size = 3) +
  scale_color_manual(name = "", values = c("ingreso per cápita" = "red", "porcentaje de población mayor de 65 años" = "blue", "esperanza de vida" = "green")) +
  scale_shape_manual(name = "", values = c("ingreso per cápita" = 1, "porcentaje de población mayor de 65 años" = 2, "esperanza de vida" = 3)) +
  labs(x = "Ingreso per cápita", y = "Gasto en pensiones", color = "", shape = "") +
  theme_classic()









# ARBOLES DE REGRESION ----------------------------------------------------


arbol1 <- rpart(`Total Gasto en Pensiones (en millones de euros)` ~ Año + `% del PIB` + `Número de Pensionistas (en miles)` + `Poblacion activa` + `Numero de parados` +
                  `Poblacion ocupada` + `Poblacion total` + `Esperanza vida` + `Evolucion del PIB` + `Salario medio`,
                data = dataframe,
                control = rpart.control(maxdepth = 5,
                                        cp=0.01, minbucket = 1))

rpart.plot(arbol1)
rpart.plot(arbol1, digits=5, snip=TRUE, extra=1) # intento de quitar numero e
summary(arbol1)
pred1 <- predict(arbol1, newdata = dataframe)



# MODELO REGRESION DATAFRAME GRANDE ---------------------------------------

dfinal <- read_excel("~/Documents/TFG/datos/dataframe final.xlsx")
View(dfinal)

names(dfinal)

names(dfinal)[which(names(dfinal) == "% del PIB")] <- "porcentaje_PIB"
names(dfinal)[which(names(dfinal) == "Año")] <- "año"
names(dfinal)[which(names(dfinal) == "Total Gasto en Pensiones (en millones de euros)")] <- "total_gasto_pensiones"
names(dfinal)[which(names(dfinal) == "Número de Pensionistas (en millones)")] <- "numero_pensionistas"
names(dfinal)[which(names(dfinal) == "Poblacion activa")] <- "poblacion_activa"
names(dfinal)[which(names(dfinal) == "Numero de parados")] <- "numero_parados"
names(dfinal)[which(names(dfinal) == "Poblacion ocupada")] <- "poblacion_ocupada"
names(dfinal)[which(names(dfinal) == "Poblacion total")] <- "poblacion_total"
names(dfinal)[which(names(dfinal) == "Esperanza vida")] <- "esperanza_vida"
names(dfinal)[which(names(dfinal) == "Evolucion del PIB")] <- "evolucion_PIB"
names(dfinal)[which(names(dfinal) == "Salario medio")] <- "salario_medio"
names(dfinal)[which(names(dfinal) == "Tasa de mortalidad")] <- "tasa_mortalidad"
names(dfinal)[which(names(dfinal) == "Tasa de natalidad")] <- "tasa_natalidad"
names(dfinal)[which(names(dfinal) == "Edad de jubilacion")] <- "edad_jubilacion"
names(dfinal)[which(names(dfinal) == "Tasa de inmigración (por cada 1.000 habitantes)")] <- "tasa_inmigracion"
names(dfinal)[which(names(dfinal) == "Tasa de desempleo")] <- "tasa_desempleo"
names(dfinal)[which(names(dfinal) == "Gasto público (miles de millones de euros)")] <- "gasto_publico"



# cambio de tipo chr a num
class(dfinal$numero_pensionistas)

if (!any(is.na(as.numeric(dfinal$numero_pensionistas)))) {
  dfinal$numero_pensionistas <- as.numeric(dfinal$numero_pensionistas)
}

class(dfinal$evolucion_PIB)

if (!any(is.na(as.numeric(dfinal$evolucion_PIB)))) {
  dfinal$evolucion_PIB <- as.numeric(dfinal$evolucion_PIB)
}

class(dfinal$tasa_desempleo)

if (!any(is.na(as.numeric(dfinal$tasa_desempleo)))) {
  dfinal$tasa_desempleo <- as.numeric(dfinal$tasa_desempleo)
}


class(dfinal$Inflacion)

if (!any(is.na(as.numeric(dfinal$Inflacion)))) {
  dfinal$Inflacion <- as.numeric(dfinal$Inflacion)
}

class(dfinal$tasa_desempleo)



## modelo de regresion 2

# Analisis de la relacion entre las variables
round(cor(x = dfinal, method = "pearson"), 3) 

library(corrplot)

## calcular la matriz de correlación
correlation_matrix <- cor(dfinal)

## crear el gráfico de matriz de correlación
corrplot(correlation_matrix, method = "color", type = "lower", tl.col = "black",
         tl.srt = 45, diag = FALSE)



# Generar el modelo
names(dfinal)

modeloregresion2 <- lm(total_gasto_pensiones ~ año + porcentaje_PIB + poblacion_activa + numero_parados +
                         poblacion_ocupada + poblacion_total + esperanza_vida + evolucion_PIB + salario_medio + 
                         tasa_natalidad + tasa_mortalidad + edad_jubilacion + Inflacion + tasa_inmigracion + 
                         gasto_publico + numero_pensionistas, data = dfinal)


summary (modeloregresion2)


# Seleccion de los mejores predictores

step(object = modeloregresion2, direction = "both", trace = 1)

## vemos que hay que eliminar algunas variables que no aportan al modelo y queda el siguiente:

modelo_final <- lm(total_gasto_pensiones ~ año + poblacion_activa + numero_parados + poblacion_total + esperanza_vida + evolucion_PIB + salario_medio + tasa_natalidad + Inflacion + tasa_inmigracion + gasto_publico + numero_pensionistas, data = dfinal)

summary(modelo_final)$coefficients

summary(modelo_final)






# Crear un data frame con las observaciones y los valores ajustados del modelo
datos_grafico2 <- data.frame(Observado = modeloregresion2$model$total_gasto_pensiones,
                            Ajustado = predict(modeloregresion2))

# Crear un gráfico de dispersión con los valores observados y los valores ajustados
ggplot(datos_grafico2, aes(x = Observado, y = Ajustado)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "Valores observados", y = "Valores ajustados", title = "Valores observados vs. Valores ajustados")





# METRICAS DE ERROR

resumen_modelo2 <- summary(modeloregresion2)

# Mostrar el MSE
mse2 <- mean(resumen_modelo2$residuals^2)
cat("MSE (Mean Squared Error): ", mse, "\n")

# Mostrar el R2
r22 <- resumen_modelo2$r.squared
cat("R2 (Coeficiente de determinación): ", r22, "\n")



