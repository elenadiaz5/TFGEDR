# MODELO REGRESION DATAFRAME GRANDE ---------------------------------------

library(readxl)

dfinal <- read_excel("~/Documents/TFG/datos/dataframe final.xlsx")
View(dfinal)

names(dfinal)

names(dfinal)[which(names(dfinal) == "Año")] <- "año"
names(dfinal)[which(names(dfinal) == "Total Gasto en Pensiones (en millones de euros)")] <- "total_gasto_pensiones"
names(dfinal)[which(names(dfinal) == "% del PIB")] <- "porcentaje_PIB"
names(dfinal)[which(names(dfinal) == "Poblacion activa")] <- "poblacion_activa"
names(dfinal)[which(names(dfinal) == "Numero de parados")] <- "numero_parados"
names(dfinal)[which(names(dfinal) == "Poblacion ocupada")] <- "poblacion_ocupada"
names(dfinal)[which(names(dfinal) == "Poblacion total")] <- "poblacion_total"
names(dfinal)[which(names(dfinal) == "Esperanza vida")] <- "esperanza_vida"
names(dfinal)[which(names(dfinal) == "Evolucion del PIB")] <- "evolucion_PIB"
names(dfinal)[which(names(dfinal) == "Salario medio")] <- "salario_medio"
names(dfinal)[which(names(dfinal) == "Tasa de natalidad")] <- "tasa_natalidad"
names(dfinal)[which(names(dfinal) == "Tasa de mortalidad")] <- "tasa_mortalidad"
names(dfinal)[which(names(dfinal) == "Edad de jubilacion")] <- "edad_jubilacion"
names(dfinal)[which(names(dfinal) == "Inflacion")] <- "inflacion"
names(dfinal)[which(names(dfinal) == "Tasa de inmigración (por cada 1.000 habitantes)")] <- "tasa_inmigracion"
names(dfinal)[which(names(dfinal) == "Gasto público (miles de millones de euros)")] <- "gasto_publico"
names(dfinal)[which(names(dfinal) == "Número de Pensionistas (en millones)")] <- "numero_pensionistas"



# cambio de tipo chr a num
class(dfinal$numero_pensionistas)

if (!any(is.na(as.numeric(dfinal$numero_pensionistas)))) {
  dfinal$numero_pensionistas <- as.numeric(dfinal$numero_pensionistas)
}

class(dfinal$evolucion_PIB)

if (!any(is.na(as.numeric(dfinal$evolucion_PIB)))) {
  dfinal$evolucion_PIB <- as.numeric(dfinal$evolucion_PIB)
}

class(dfinal$inflacion)

if (!any(is.na(as.numeric(dfinal$inflacion)))) {
  dfinal$inflacion <- as.numeric(dfinal$inflacion)
}



# CORRELACIONES -----------------------------------------------------------

# Analisis de la relacion entre las variables
round(cor(x = dfinal, method = "pearson"), 3) 

library(corrplot)

## calcular la matriz de correlación
correlation_matrix <- cor(dfinal)

## crear el gráfico de matriz de correlación
corrplot(correlation_matrix, method = "color", type = "lower", tl.col = "black",
         tl.srt = 45, diag = FALSE)



# MODELO 1 ----------------------------------------------------------------

# * este modelo es el completo sin quitar ninguna variable

# Generar el modelo
names(dfinal)

modelocompleto <- lm(total_gasto_pensiones ~ año + porcentaje_PIB + poblacion_activa + numero_parados +
                         poblacion_ocupada + poblacion_total + esperanza_vida + evolucion_PIB + salario_medio + 
                         tasa_natalidad + tasa_mortalidad + edad_jubilacion + inflacion + tasa_inmigracion + 
                         gasto_publico + numero_pensionistas, data = dfinal)


summary (modelocompleto)



# Crear un data frame con las observaciones y los valores ajustados del modelo
datos_grafico2 <- data.frame(Observado = modeloregresion2$model$total_gasto_pensiones,
                             Ajustado = predict(modeloregresion2))

# Crear un gráfico de dispersión con los valores observados y los valores ajustados
ggplot(datos_grafico2, aes(x = Observado, y = Ajustado)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "Valores observados", y = "Valores ajustados", title = "Valores observados vs. Valores ajustados")




# Obtén las predicciones del modelo
predicciones <- predict(modelocompleto, newdata = dfinal)

# Calcula las diferencias entre las predicciones y los valores reales
diferencias <- predicciones - dfinal$total_gasto_pensiones

# Calcula los errores cuadrados
errores_cuadrados <- diferencias^2

# Calcula el promedio de los errores cuadrados
promedio_errores_cuadrados <- mean(errores_cuadrados)

# Calcula el RMSE
rmse <- sqrt(promedio_errores_cuadrados)

rmse



# MODELO 2 ----------------------------------------------------------------

# * en este modelo se eliminar las variables que no aportan al modelo

# Seleccion de los mejores predictores

step(object = modelocompleto, direction = "both", trace = 1)

## vemos que hay que eliminar algunas variables que no aportan al modelo y queda el siguiente:

# estas son las variables significativas que quedan del modelo 


modelosinvar<- lm(total_gasto_pensiones ~ año + poblacion_activa + numero_parados + 
                    poblacion_total + esperanza_vida + evolucion_PIB + salario_medio + 
                   tasa_natalidad + inflacion + tasa_inmigracion + gasto_publico + 
                    numero_pensionistas, data = dfinal)


summary(modelosinvar)$coefficients

summary(modelosinvar)



# METRICAS DE VALORACION DEL MODELO ---------------------------------------

# Obtener el coeficiente de determinación (R-cuadrado)
r_squared <- summary(modelosinvar)$r.squared

# Obtener el error estándar de la estimación (SEE)
see <- summary(modelosinvar)$sigma

# Realizar la prueba de significancia de los coeficientes
coef_test <- coef(summary(modelosinvar))

# Obtener los p-valores de los coeficientes individuales
p_values <- coef_test[, "Pr(>|t|)"]

# Obtener el valor p de la prueba F (para evaluar la significancia conjunta de los coeficientes)
f_p_value <- coef_test["Residuals", "Pr(>|t|)"]

# Imprimir los resultados
cat("Coeficiente de determinación (R-cuadrado):", r_squared, "\n")
cat("Error estándar de la estimación (SEE):", see, "\n")
cat("P-valores de los coeficientes:\n")
print(p_values)
cat("Valor p de la prueba F:", f_p_value, "\n")



# Crear un data frame con las observaciones y los valores ajustados del modelo
datos_grafico <- data.frame(Observado = modelosinvar$model$total_gasto_pensiones,
                             Ajustado = predict(modelosinvar))

# Crear un gráfico de dispersión con los valores observados y los valores ajustados
ggplot(datos_grafico, aes(x = Observado, y = Ajustado)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x = "Valores observados", y = "Valores ajustados", title = "Valores observados vs. Valores ajustados")




# Realizar predicciones con el modelo
predicciones2 <- predict(modelosinvar, newdata = dfinal)

# Calcular los residuos
residuos2 <- dfinal$total_gasto_pensiones - predicciones2

# Calcular el RMSE
rmse2 <- sqrt(mean(residuos2^2))

rmse2  # Mostrar el valor del RMSE


# PRUEBA DE NORMALIDAD DE LOS RESIDUOS
# Calcular los residuos
residuos <- resid(modelosinvar)

# Realizar la prueba de normalidad de Shapiro-Wilk
resultado_normalidad <- shapiro.test(residuos)

# Imprimir el resultado
print(resultado_normalidad)

# Crear un histograma de los residuos
hist(residuos, breaks = 10, col = "steelblue", xlab = "Residuos", main = "Histograma de los residuos")

# Crear un gráfico QQ de los residuos
qqnorm(residuos, col = "steelblue", main = "Gráfico de probabilidad normal (QQ-plot)")
qqline(residuos, col = "red")


# PRUEBA DE AUTOCORRELACION DE LOS RESIDUOS

# Calcular los residuos
residuos <- resid(modelosinvar)

# Calcular y graficar la función de autocorrelación de los residuos
acf(residuos)




# PREDICCION A FUTURO -----------------------------------------------------


modelosinvar<- lm(total_gasto_pensiones ~ año + poblacion_activa + numero_parados + 
                    poblacion_total + esperanza_vida + evolucion_PIB + salario_medio + 
                    tasa_natalidad + inflacion + tasa_inmigracion + gasto_publico + 
                    numero_pensionistas, data = dfinal)

nuevos.datos <- data.frame(año = 2025, 
                           `poblacion_activa` = 24000, 
                           `numero_parados` = 3100000, 
                           `poblacion_total` = 47400000,
                           `esperanza_vida` = 84,
                           `evolucion_PIB` = 3,
                           `salario_medio` = 25300.00,
                           `tasa_natalidad` = 7, 
                           `inflacion` = 2.3, 
                           `tasa_inmigracion` = 12, 
                           `gasto_publico` = 620000, 
                           `numero_pensionistas` = 10.7)


names(dfinal)

view(nuevos.datos)


prediccion <- predict(modelosinvar, nuevos.datos)

print(prediccion)

