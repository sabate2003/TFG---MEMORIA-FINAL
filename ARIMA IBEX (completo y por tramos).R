install.packages("zoo")
# Cargar librerías necesarias
library(readxl)
library(forecast)
library(tseries)
library(ggplot2)
library(zoo)
library(readxl)
library(forecast)
library(tseries)
library(lmtest)


# 1. Cargar los datos desde Excel
archivo <- "BASE DE DATOS ! .xlsx"  # Asegúrate de que el nombre y la ruta estén bien
df_ibex <- read_excel(archivo, sheet = "IBEX 35")

# 2. Convertir la columna de fecha
df_ibex$Fecha <- as.Date(df_ibex$`Exchange Date`)  # Ajusta si la columna se llama distinto

# 3. Crear la serie temporal con zoo y fechas reales
ibex_zoo <- zoo(df_ibex$Close, order.by = df_ibex$Fecha)

# 4. Visualizar la serie temporal original
plot(ibex_zoo, main = "Serie IBEX 35", ylab = "Cierre", xlab = "Fecha")

# 5. Elegir las fechas de corte para dividir en train y test
fecha_corte <- as.Date("2023-01-01")

# 6. Dividir en train y test
ibex_train <- window(ibex_zoo, end = fecha_corte - 1)
ibex_test <- window(ibex_zoo, start = fecha_corte)

# 7. Visualizar train y test
plot(ibex_train, main = "Train - IBEX 35", ylab = "Cierre", xlab = "Fecha")
plot(ibex_test, main = "Test - IBEX 35", ylab = "Cierre", xlab = "Fecha")

# 8. Eliminar los valores faltantes antes de aplicar acf
ibex_train_clean <- na.omit(ibex_train)

# 9. Convertir ibex_train_clean a una serie temporal (ts) adecuada para análisis
# Aquí estoy asumiendo que los datos son diarios. Ajusta 'start' y 'frequency' según tus datos.
ibex_train_ts <- ts(ibex_train_clean, start = c(2012, 1), frequency = 365)  # Ajusta el 'start' según el año y la frecuencia

# 10. Visualizar la serie temporal limpiada
plot(ibex_train_ts, main = "Serie Temporal Limpiada - IBEX 35", ylab = "Cierre", xlab = "Fecha")

# 11. Correlograma para identificar el modelo: ACF y PACF
par(mfrow = c(1,2))  # Configurar la ventana de gráficos
acf(ibex_train_ts, main = 'Serie de rendimiento de IBEX 35', xlab = 'Nº Retrasos')
pacf(ibex_train_ts, main = 'Serie de rendimiento de IBEX 35', xlab = 'Nº Retrasos')

#Comprobamos nuevamente la estacionariedad de la serie con una diferencia.
adf.test(ibex_train_ts)
#Aceptamos la H0, lo que significa que la serie es no estacionaria.

# 12. Aplicar diferencia para intentar hacer estacionaria la serie
ibex_train_diff <- diff(ibex_train_ts)

# 13. Visualizar la serie diferenciada
plot(ibex_train_diff, main = "Serie Temporal Diferenciada - IBEX 35", ylab = "Cierre Diferenciado", xlab = "Fecha")

# 14. Comprobar nuevamente la estacionariedad de la serie diferenciada
adf.test(ibex_train_diff)

#Para tener una guía comenzaremos aplicando el auto.arima, para que nos medio indique cual podría ser el mejor ARIMA.
auto_arima_IBEX35 <- auto.arima(ibex_train_ts)
auto_arima_IBEX35
coeftest(auto.arima(ibex_train_ts))
#DOS PARAMETROS SIGNIFICATIVOS  arima(4,1,0)
#AIC=41291.4
#Podemos observar que tiene un error tanto del AIC como del BIC enorme, trataremos de encontrar unos modelos que errores más bajos, si existe. 
#Comenzaremos a probar distintos modelos para observar cual podría ser el mejor.
#Comenzaremos con los modelos básicos. 
MA1_IBEX35 <- arima(ibex_train_ts, order = c(0,0,1))
MA1_IBEX35
#Realizamos un método formal para observar resultados.
coeftest(MA1_IBEX35)
#UN PARAMETRO SIGNIFICATIVO
#AIC = 51970.47

ARMA_IBEX35 <- arima(ibex_train_ts, order = c(1,0,1))
ARMA_IBEX35
coeftest(ARMA_IBEX35)
#DOS PARAMETROS MUY MUY SIGNIFICATIVOS 
#AIC = 51970.47

AR1_ibex35 <- arima(ibex_train_ts, order = c(1,0,0))
AR1_ibex35
coeftest(AR1_ibex35)
#UN PARAMETRO MUY SIGNIFICATIVO 
#AIC = 41278.31
#Basándonos en los gráficos de la FAC y FACP, comenzaremos con el modelo ARIMA (19,0,1)

ARMA191_IBEX35 <- arima (ibex_train_ts, order = c(19,0,1))
ARMA191_IBEX35
coeftest(ARMA191_IBEX35)
# 1 PARAMETROS SIGNIFICATIVOS,
# AIC =  41282.75

#Observando lo significativos que son los valores vamos a probar un AR(10)
AR10_IBEX35 <- arima(ibex_train_ts, order = c(10,0,0))
AR10_IBEX35
coeftest(AR10_IBEX35)
# 3 PARAMETROS SIGNIFICATIVOS, CON 1 MUY MUY SIGNIFICATIVOS
#AIC = 41274.64

# o un AR(2) 
AR2_IBEX35 <- arima(ibex_train_ts, order = c(2,0,0))
AR2_IBEX35
coeftest(AR2_IBEX35)
#1 PARAMETROS SIGNIFICATIVOS
#AIC =  41275.34

#Vamos a probar modelos MA para observar si funcionan mejor.
MA2_IBEX35 <- arima(ibex_train_ts, order = c(0,0,2))
MA2_IBEX35
coeftest(MA2_IBEX35)
#DOS PARAMETROS MUY SIGNIFIVATIVOS.
#AIC = 49038.92


#Vamos a probar modelos AR un poco más elevados.
AR3_IBEX35 <- arima(ibex_train_ts, order = c(3,0,0))
AR3_IBEX35
coeftest(AR3_IBEX35)
#1 PARAMETROS SIGNIFICATIVOS
#AIC =  41276.44

#Incluso mucho más elevadosç
AR19_IBEX35 <- arima(ibex_train_ts, order = c(19,0,0))
AR19_IBEX35
coeftest(AR19_IBEX35)
#2 PARAMETROS SIGNIFICATIVOS, 
#AIC = 41281.02

#Una última prueba con valores más altos aún.
AR25_IBEX35 <- arima(ibex_train_ts, order = c(25,0,0))
AR25_IBEX35
coeftest(AR25_IBEX35)
#14 PARAMETROS SIGNIFICATIVOS, 8 MUY MUY SIGNIFICATIVOS
#AIC = 41290.49

#
AR22_IBEX35 <- arima(ibex_train_ts, order = c(2,1,2))
AR22_IBEX35
coeftest(AR22_IBEX35)
#4 MUY MUY SIGNIFICATIVOS 
#AIC = 41259.73

#Observando que cuanto más bajo el AR, menos error existe, vamos a seguir probando un modelo con un AR mucho más elevado.
AR42_IBEX35 <- arima(ibex_train_ts, order = c(4,1,2))
AR42_IBEX35
coeftest(AR42_IBEX35)
#5 MUY MUY SIGNIFICATIVOS DE 6
#AIC = 41261.17

#Observando que cuanto más bajo el AR, menos error existe, vamos a seguir probando un modelo con un AR mucho más elevado.
AR32_IBEX35 <- arima(ibex_train_ts, order = c(3,1,2))
AR32_IBEX35
coeftest(AR32_IBEX35)
#5 MUY MUY SIGNIFICATIVOS DE 5
#AIC = 41259.96

#Observando que cuanto más bajo el AR, menos error existe, vamos a seguir probando un modelo con un AR mucho más elevado.
AR33_IBEX35 <- arima(ibex_train_ts, order = c(3,1,3))
AR33_IBEX35
coeftest(AR33_IBEX35)
#5 MUY MUY SIGNIFICATIVOS DE 6
#AIC =41273.87

#Observando que cuanto más bajo el AR, menos error existe, vamos a seguir probando un modelo con un AR mucho más elevado.
AR25_IBEX35 <- arima(ibex_train_ts, order = c(5,1,2))
AR25_IBEX35
coeftest(AR25_IBEX35)
#2 SIGNIFICATIVOS 1  MUY SIGNIFICATIVOS DE 7
#AIC =41262.01


#Observando que cuanto más bajo el AR, menos error existe, vamos a seguir probando un modelo con un AR mucho más elevado.
AR22_IBEX35 <- arima(ibex_train_ts, order = c(2,1,2))
AR22_IBEX35
coeftest(AR22_IBEX35)
#4 MUY MUY SIGNIFICATIVOS 
#AIC = 41259.73

# Crear un vector vacío para almacenar los AIC de cada modelo
AIC_values <- c()

# Modelo AR(2,0,2)
AR22_IBEX35 <- arima(ibex_train_ts, order = c(2,1,2))
AIC_values["AR(2,0,2)"] <- AIC(AR22_IBEX35)

# Modelo AR(4,0,2)
AR42_IBEX35 <- arima(ibex_train_ts, order = c(4,1,2))
AIC_values["AR(4,0,2)"] <- AIC(AR42_IBEX35)

# Modelo AR(3,0,2)
AR32_IBEX35 <- arima(ibex_train_ts, order = c(3,1,2))
AIC_values["AR(3,0,2)"] <- AIC(AR32_IBEX35)

# Modelo AR(3,0,3)
AR33_IBEX35 <- arima(ibex_train_ts, order = c(3,1,3))
AIC_values["AR(3,0,3)"] <- AIC(AR33_IBEX35)

# Modelo AR(5,0,2)
AR25_IBEX35 <- arima(ibex_train_ts, order = c(5,1,2))
AIC_values["AR(5,0,2)"] <- AIC(AR25_IBEX35)

# Mostrar todos los AIC
print(AIC_values)


# Crear un vector para almacenar los RMSE
RMSE_values <- c()

# Calcular RMSE para cada modelo

# AR(2,0,2)
residuos_AR22 <- AR22_IBEX35$residuals
RMSE_AR22 <- sqrt(mean(residuos_AR22^2))
RMSE_values["AR(2,0,2)"] <- RMSE_AR22
RMSE_AR22

# AR(4,0,2)
residuos_AR42 <- AR42_IBEX35$residuals
RMSE_AR42 <- sqrt(mean(residuos_AR42^2))
RMSE_values["AR(4,0,2)"] <- RMSE_AR42

# AR(3,0,2)
residuos_AR32 <- AR32_IBEX35$residuals
RMSE_AR32 <- sqrt(mean(residuos_AR32^2))
RMSE_values["AR(3,0,2)"] <- RMSE_AR32

# AR(3,0,3)
residuos_AR33 <- AR33_IBEX35$residuals
RMSE_AR33 <- sqrt(mean(residuos_AR33^2))
RMSE_values["AR(3,0,3)"] <- RMSE_AR33

# AR(5,0,2)
residuos_AR25 <- AR25_IBEX35$residuals
RMSE_AR25 <- sqrt(mean(residuos_AR25^2))
RMSE_values["AR(5,0,2)"] <- RMSE_AR25


# Mostrar los RMSE de todos los modelos
print(RMSE_values)


# Comparar los modelos seleccionando el que tiene el menor AIC y RMSE
best_model_by_AIC <- names(AIC_values)[which.min(AIC_values)]
best_model_by_RMSE <- names(RMSE_values)[which.min(RMSE_values)]

cat("Mejor modelo según AIC: ", best_model_by_AIC, "\n")
cat("Mejor modelo según RMSE: ", best_model_by_RMSE, "\n")


# Residuos del modelo AR(2,0,2) (ejemplo)
residuos_AR22 <- AR22_IBEX35$residuals

# Correlograma de los residuos (ACF y PACF)
par(mfrow = c(1, 2))
acf(residuos_AR22, main = "ACF de los residuos AR(2,0,2)")
pacf(residuos_AR22, main = "PACF de los residuos AR(2,0,2)")

# Test de normalidad de los residuos
jarque.bera.test(residuos_AR42)

# Histograma de los residuos
hist(residuos_AR22, main = "Histograma de los residuos AR(2,0,2)", xlab = "Residuos")

mean(residuos_AR22)
#Media igual a cero, puede ser ruido blanco.
# Test de independencia de los residuos (Box-Pierce Test)
Box.test(residuos_AR22, type = c("Box-Pierce"))





# Predicción del modelo AR(4,0,2)
pAR22_IBEX35 <- forecast(AR22_IBEX35, h = 5)
pAR22_IBEX35
prediccion_forecast_AR22 <- pAR22_IBEX35$mean

prediccion_AR22_IBEX35 <- predict(AR22_IBEX35, n.ahead = 5)
prediccion_AR22_IBEX35
pred_value_AR22 <- prediccion_AR42_IBEX35$pred

# Si la serie está diferenciada, revertimos la diferencia para obtener la serie original
ultimo_valor <- tail(ibex_train_ts, 1)  # O el valor más reciente de tu serie original
AR22_IBEX35_Revertida_pred <- cumsum(c(ultimo_valor, pred_value_AR42))[-1]
print(AR22_IBEX35_Revertida_pred)

AR22_IBEX35_Revertida_forecast <- cumsum(c(ultimo_valor, prediccion_forecast_AR22))[-1]
print(AR22_IBEX35_Revertida_forecast)

# Gráfico de las predicciones
par(mfrow = c(1, 2))
plot(AR22_IBEX35_Revertida_forecast, main = "Predicción AR(2,0,2) - Forecast")
plot(AR22_IBEX35_Revertida_pred, main = "Predicción AR(2,0,2) - Predict")

# Ahora calcularemos el RMSE de las predicciones
library(Metrics)
# Suponiendo que 'serie_real' es la serie de valores reales (últimos 5 datos reales de la serie)
serie_real <- tail(ibex_train_ts, 5)  # Últimos 5 valores reales de la serie

# Calcular el RMSE
rmse_final_AR22 <- rmse(serie_real, AR22_IBEX35_Revertida_pred)
print(paste("RMSE = ", rmse_final_AR22))

# Ahora calculamos R^2
SSE_AR22 <- sum((serie_real - AR22_IBEX35_Revertida_pred)^2)  # Suma de los errores al cuadrado
SST_AR22 <- sum((serie_real - mean(serie_real))^2)  # Suma total de los cuadrados

# Calcular R^2
R_squared_AR22 <- 1 - (SSE_AR22 / SST_AR22)
print(paste("R^2 = ", R_squared_AR22))

# Crear el dataframe con los valores reales y predichos
datos <- data.frame(
  Fecha = seq(as.Date("2024-12-27"), as.Date("2024-12-31"), by = "day"),  # Fechas de los últimos 5 días
  Reales = as.numeric(serie_real),
  Predichos_AR22 = as.numeric(AR22_IBEX35_Revertida_pred)
)
datos
# Graficar los valores reales vs las predicciones
plot(datos$Fecha, datos$Reales, type="o", col="green", pch=16, lty=1, 
     ylim=range(c(datos$Reales, datos$Predichos_AR22)),
     xlab="Fecha", ylab="Precio IBEX 35", main="Valores Reales vs Predicción AR(2,0,2)")

lines(datos$Fecha, datos$Predichos_AR42, type="o", col="blue", pch=15, lty=2)

# Agregar leyenda
legend("topleft", legend=c("Valores Reales", "Predicción AR(4,0,2)"), 
       col=c("green", "blue"), pch=c(16, 15), lty=c(1, 2), bty="n")




# Ajustar el modelo AR(4,0,2) sobre el conjunto de entrenamiento
AR22_IBEX35 <- arima(ibex_train_ts, order = c(2,0,2))

# Realizar la predicción sobre el conjunto de test (5 pasos hacia adelante)
pAR22_IBEX35 <- forecast(AR22_IBEX35, h = length(ibex_test))

# Predicciones
predicciones <- pAR22_IBEX35$mean

# Comparar las predicciones con los valores reales del conjunto de test
real_vs_prediccion <- data.frame(
  Fecha = index(ibex_test),
  Real = as.numeric(ibex_test),
  Predicho = as.numeric(predicciones)
)

print(real_vs_prediccion)

# Cargar ggplot2
library(ggplot2)

# Crear un dataframe con las fechas del test, los valores reales y las predicciones
real_vs_prediccion <- data.frame(
  Fecha = index(ibex_test),
  Real = as.numeric(ibex_test),
  Predicho = as.numeric(predicciones)
)

# Graficar con ggplot2
ggplot(real_vs_prediccion, aes(x = Fecha)) +
  # Gráfico de los valores reales (en negro)
  geom_line(aes(y = Real, color = "Valores Reales"), size = 1.2) + 
  # Gráfico de las predicciones (en rojo)
  geom_line(aes(y = Predicho, color = "Predicción ARMA(2,0,2)"), size = 1.2, linetype = "dashed") + 
  # Personalización del gráfico
  labs(title = "Valores Reales vs Predicción AR(2,0,2)",
       x = "Fecha",
       y = "Cierre IBEX 35") +
  scale_color_manual(values = c("Valores Reales" = "black", "Predicción AR(2,0,2)" = "red")) + # Cambiar colores aquí
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  ) +
  # Leyenda personalizada
  scale_x_date(date_labels = "%b %d, %Y", date_breaks = "3 month")  # Ajusta el formato de fechas


# Cargar las librerías necesarias
library(ggplot2)
library(zoo)

# Definir las fechas de corte para train y test
fecha_corte <- as.Date("2023-01-01")

# Crear la serie temporal completa
ibex_zoo <- zoo(df_ibex$Close, order.by = df_ibex$Fecha)

# Dividir en train y test
ibex_train <- window(ibex_zoo, end = fecha_corte - 1)
ibex_test <- window(ibex_zoo, start = fecha_corte)

# Crear el gráfico con ggplot2
ggplot() +
  # Parte de train (azul) completa
  geom_line(aes(x = index(ibex_train), y = as.numeric(ibex_train), color = "Train"), size = 1.2) +
  
  # Parte de test (rojo)
  geom_line(aes(x = index(ibex_test), y = as.numeric(ibex_test), color = "Test"), size = 1.2) +
  
  # Resaltar la parte de train correspondiente al test en azul
  geom_line(aes(x = index(ibex_test), y = as.numeric(ibex_train)[length(ibex_train) - length(ibex_test) + 1:length(ibex_test)], color = "Train (Test area)"), size = 1.2, linetype = "dashed") +
  
  # Personalización del gráfico
  labs(title = "Comparación de Train y Test en la Serie Temporal IBEX 35",
       x = "Fecha", y = "Valor Cierre") +
  scale_color_manual(name = "Serie", values = c("Train" = "BLACK", "Test" = "BLACK", "Train (Test area)" = "RED")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )


library(ggplot2)

# Crear gráfico con ggplot2
ggplot() +
  # Parte del test en rojo
  geom_line(aes(x = index(ibex_test), y = as.numeric(ibex_test), color = "Test")) +
  
  # Parte del train correspondiente a test en azul
  geom_line(aes(x = index(ibex_train)[(length(ibex_train) - length(ibex_test) + 1):length(ibex_train)], 
                y = as.numeric(ibex_train)[(length(ibex_train) - length(ibex_test) + 1):length(ibex_train)], 
                color = "Train")) +
  
  # Configurar título, etiquetas y leyenda
  labs(title = "Train y Test comparados", x = "Fecha", y = "Valor IBEX 35") +
  scale_color_manual(name = "Serie", values = c("Train" = "blue", "Test" = "red")) +
  theme_minimal()

library(ggplot2)

# Crear gráfico con ggplot2
ggplot() +
  # Parte del test en rojo
  geom_line(aes(x = index(ibex_test), y = as.numeric(ibex_test), color = "Test")) +
  
  # Parte del train en azul
  geom_line(aes(x = index(ibex_train), y = as.numeric(ibex_train), color = "Train")) +
  
  # Configurar título, etiquetas y leyenda
  labs(title = "Comparación entre Train y Test", x = "Fecha", y = "Valor IBEX 35") +
  scale_color_manual(name = "Serie", values = c("Train" = "blue", "Test" = "red")) +
  theme_minimal()
library(ggplot2)

# Unir el conjunto de train y test para tener todo el rango de 2010 a 2025
serie_completa <- c(ibex_train, ibex_test)

# Crear gráfico con ggplot2
ggplot() +
  # Graficar el conjunto de entrenamiento (train) en azul
  geom_line(aes(x = index(ibex_train), y = as.numeric(ibex_train), color = "Train")) +
  
  # Graficar el conjunto de test (test) en rojo
  geom_line(aes(x = index(ibex_test), y = as.numeric(ibex_test), color = "Test")) +
  
  # Configurar título, etiquetas y leyenda
  labs(title = "Serie Completa: Train vs Test", x = "Fecha", y = "Valor IBEX 35") +
  scale_color_manual(name = "Serie", values = c("Train" = "blue", "Test" = "red")) +
  theme_minimal() +
  xlim(c(min(index(ibex_train)), max(index(ibex_test)))) # Definir el rango de fechas de 2010 a 2025

library(ggplot2)

# Graficar todo el conjunto de train y test comparado
ggplot() +
  # Parte de test en rojo
  geom_line(aes(x = index(ibex_test), y = as.numeric(ibex_test), color = "Test")) +
  
  # Parte del train correspondiente a test en azul
  geom_line(aes(x = index(ibex_test), y = as.numeric(ibex_train)[length(ibex_train) - length(ibex_test) + 1:length(ibex_test)], color = "Train")) +
  
  # Configurar título, etiquetas y leyenda
  labs(title = "Train y Test comparados", x = "Fecha", y = "Valor IBEX 35") +
  scale_color_manual(name = "Serie", values = c("Train" = "RED", "Test" = "BLACK")) +
  theme_minimal()



######por tramos#######



#--------------------------TRAMO 1-------------------------------

tramo1 <- window(ibex_zoo, start = as.Date("2012-01-01"), end = as.Date("2015-12-31"))

# División en train/test
tramo1_train1 <- window(tramo1, end = as.Date("2014-12-31"))
tramo1_test1 <- window(tramo1, start = as.Date("2015-01-01"))

#Visualizar train y test del Tramo 1
plot(tramo1_train1, main = "Train - IBEX 35 (Tramo 1)", ylab = "Cierre", xlab = "Fecha")
plot(tramo1_test1, main = "Test - IBEX 35 (Tramo 1)", ylab = "Cierre", xlab = "Fecha")

# Limpieza y conversión a ts
train1_clean <- na.omit(tramo1_train1)
tramo1_train1_ts <- ts(train1_clean, start = c(2012, 1), frequency = 365)

# 4. Visualizar serie temporal limpia
plot(tramo1_train1_ts, main = "Serie Temporal Limpiada - Tramo 1", ylab = "Cierre", xlab = "Fecha")

# 5. Correlogramas ACF y PACF
par(mfrow = c(1,2))
acf(tramo1_train1_ts, main = "ACF - Tramo 1", xlab = "Nº Retrasos")
pacf(tramo1_train1_ts, main = "PACF - Tramo 1", xlab = "Nº Retrasos")


# 6. Test de estacionariedad (ADF)
adf.test(tramo1_train1_ts)
# Nota: si el p-valor > 0.05 → NO es estacionaria. NO ES ESTACIONARIA

# Diferencia manual de orden 1
tramo1_diff1 <- diff(tramo1_train1_ts)

# Verificamos estacionariedad
adf.test(tramo1_diff1)  # Si ahora p < 0.05 → es estacionaria. ES ESTACIONARIA


# --- 4. Visualizamos la serie diferenciada ---
plot(tramo1_diff1, main = "Serie diferenciada tramo1", ylab = "Diferencia", xlab = "Fecha")

# --- 5. FAC y FACP ---
par(mfrow = c(1, 2))
acf(tramo1_diff1, main = "ACF - tramo1 diferenciado")
pacf(tramo1_diff1, main = "PACF - tramo1 diferenciado")
par(mfrow = c(1, 1))  # Restaurar ventana


# Luego puedes aplicar auto.arima sin que estime d automáticamente:
auto.arima(tramo1_diff1, d = 0)


# MA(1)
MA1_tramo1 <- arima(tramo1_diff1, order = c(0,0,1))
MA1_tramo1
coeftest(MA1_tramo1)
#AIC = 9451.27
#CERO PARAMETROS SIG


# ARMA(1,1)
ARMA11_tramo1 <- arima(tramo1_diff1, order = c(1,0,1))
ARMA11_tramo1
coeftest(ARMA11_tramo1)
#9448.82
#DOS PARAMETROS MUY SIG


# AR(1)
AR1_tramo1 <- arima(tramo1_diff1, order = c(1,0,0))
AR1_tramo1
coeftest(AR1_tramo1)
#9451.32
#CERO PARÁMETROS SIG

# ARIMA(19,0,1)
ARIMA191_tramo1 <- arima(tramo1_diff1, order = c(19,0,1))
ARIMA191_tramo1
coeftest(ARIMA191_tramo1)
#9460.92
#CERO

# AR(10)
AR10_tramo1 <- arima(tramo1_diff1, order = c(10,0,0))
AR10_tramo1
coeftest(AR10_tramo1)

# AR(2)
AR2_tramo1 <- arima(tramo1_diff1, order = c(2,0,0))
AR2_tramo1
coeftest(AR2_tramo1)
#CERO


# MA(2)
MA2_tramo1 <- arima(tramo1_diff1, order = c(0,0,2))
MA2_tramo1
coeftest(MA2_tramo1)
#CERO

# AR(3)
AR3_tramo1 <- arima(tramo1_diff1, order = c(3,0,0))
AR3_tramo1
coeftest(AR3_tramo1)
#CERO


# AR(19)
AR19_tramo1 <- arima(tramo1_diff1, order = c(19,0,0))
AR19_tramo1
coeftest(AR19_tramo1)

# AR(25)
AR25_tramo1 <- arima(tramo1_diff1, order = c(25,0,0))
AR25_tramo1
coeftest(AR25_tramo1)

# ARMA(2,2)
AR22_tramo1 <- arima(tramo1_diff1, order = c(2,0,2))
AR22_tramo1
coeftest(AR22_tramo1)
#TODOS MUY 


# ARMA(4,2)
AR42_tramo1 <- arima(tramo1_diff1, order = c(4,0,2))
AR42_tramo1
coeftest(AR42_tramo1)
#4 SIG, 2 MUCHO, 1 BIEN


# ARMA(3,2)
AR32_tramo1 <- arima(tramo1_diff1, order = c(3,0,2))
AR32_tramo1
coeftest(AR32_tramo1)

# ARMA(3,3)
AR33_tramo1 <- arima(tramo1_diff1, order = c(3,0,3))
AR33_tramo1
coeftest(AR33_tramo1)
#TODOS MUYYYYYYYY


# ARMA(5,2)
AR52_tramo1 <- arima(tramo1_diff1, order = c(5,0,2))
AR52_tramo1
coeftest(AR52_tramo1)
#ÑEH 



# Crear un vector vacío para almacenar los AIC de cada modelo
AIC_values <- c()

# Modelo ARMA(2,2)
AR22_tramo1 <- arima(tramo1_diff1, order = c(2,0,2))
AIC_values["ARMA(2,2)"] <- AIC(AR22_tramo1)

# Modelo ARMA(4,2)
AR42_tramo1 <- arima(tramo1_diff1, order = c(4,0,2))
AIC_values["ARMA(4,2)"] <- AIC(AR42_tramo1)

# Modelo ARMA(3,2)
AR32_tramo1 <- arima(tramo1_diff1, order = c(3,0,2))
AIC_values["ARMA(3,2)"] <- AIC(AR32_tramo1)

# Modelo ARMA(3,3)
AR33_tramo1 <- arima(tramo1_diff1, order = c(3,0,3))
AIC_values["ARMA(3,3)"] <- AIC(AR33_tramo1)

# Modelo ARMA(5,2)
AR52_tramo1 <- arima(tramo1_diff1, order = c(5,0,2))
AIC_values["ARMA(5,2)"] <- AIC(AR52_tramo1)

# Mostrar todos los AIC
print(AIC_values)

# --- Calcular RMSE para cada modelo ---
RMSE_values <- c()

# ARMA(2,2)
res_AR22 <- AR22_tramo1$residuals
RMSE_values["ARMA(2,2)"] <- sqrt(mean(res_AR22^2))

# ARMA(4,2)
res_AR42 <- AR42_tramo1$residuals
RMSE_values["ARMA(4,2)"] <- sqrt(mean(res_AR42^2))

# ARMA(3,2)
res_AR32 <- AR32_tramo1$residuals
RMSE_values["ARMA(3,2)"] <- sqrt(mean(res_AR32^2))

# ARMA(3,3)
res_AR33 <- AR33_tramo1$residuals
RMSE_values["ARMA(3,3)"] <- sqrt(mean(res_AR33^2))

# ARMA(5,2)
res_AR52 <- AR52_tramo1$residuals
RMSE_values["ARMA(5,2)"] <- sqrt(mean(res_AR52^2))

# Mostrar los RMSE de todos los modelos
print(RMSE_values)

# --- Comparar los modelos ---
best_model_by_AIC <- names(AIC_values)[which.min(AIC_values)]
best_model_by_RMSE <- names(RMSE_values)[which.min(RMSE_values)]

cat("✅ Mejor modelo según AIC: ", best_model_by_AIC, "\n")
cat("✅ Mejor modelo según RMSE:", best_model_by_RMSE, "\n")


# Residuos del modelo ARMA(3,3)
residuos_AR33 <- AR33_tramo1$residuals

# Correlograma de los residuos (ACF y PACF)
par(mfrow = c(1, 2))
acf(residuos_AR33, main = "ACF de los residuos ARMA(3,3)")
pacf(residuos_AR33, main = "PACF de los residuos ARMA(3,3)")

# Test de normalidad de los residuos. DISTRIBUCIÓN NORMAL. 
jarque.bera.test(residuos_AR33)
#P-VALUE=0.0006. NO SIGUEN DISTRIBUCIÓN NORMAL 

# Histograma de los residuos
hist(residuos_AR33, main = "Histograma de los residuos ARMA(3,3)", xlab = "Residuos")

# Media de los residuos
mean(residuos_AR33)

# Test de independencia de los residuos (Box-Pierce). RESIDUOS AUTOCORRELACIONADOS, SON RUIDO BLANCO
Box.test(residuos_AR33, type = "Box-Pierce")
#SI SON RUDIO BLANCO. No se rechaza la hipótesis nula de que los residuos son independientes (no hay autocorrelación significativa).


# Predicción con forecast() (5 pasos adelante)
pAR33 <- forecast(AR33_tramo1, h = 5)
prediccion_forecast_AR33 <- pAR33$mean

# Predicción con predict()
prediccion_AR33 <- predict(AR33_tramo1, n.ahead = 5)
pred_value_AR33 <- prediccion_AR33$pred
print(prediccion_AR33)
print(pred_value_AR33)
# Usar el último valor real del tramo1 original (antes de diferenciar)
ultimo_valor <- tail(tramo1_train1_ts, 1)

# Revertir diferencia con cumsum
AR33_Revertida_pred <- cumsum(c(ultimo_valor, pred_value_AR33))[-1]
AR33_Revertida_forecast <- cumsum(c(ultimo_valor, prediccion_forecast_AR33))[-1]

# Mostrar
print(AR33_Revertida_pred)
print(AR33_Revertida_forecast)


# Graficar ambas predicciones
par(mfrow = c(1, 2))
plot(AR33_Revertida_forecast, main = "Forecast - ARMA(3,3)", type = "o", col = "blue")
plot(AR33_Revertida_pred, main = "Predict - ARMA(3,3)", type = "o", col = "darkgreen")


library(Metrics)

# Supongamos que estos son los últimos 5 datos reales de la serie original
serie_real <- tail(tramo1_train1_ts, 5)

# RMSE
rmse_final_AR33 <- rmse(serie_real, AR33_Revertida_pred)
print(paste("RMSE =", rmse_final_AR33))

# R²
SSE_AR33 <- sum((serie_real - AR33_Revertida_pred)^2)
SST_AR33 <- sum((serie_real - mean(serie_real))^2)
R_squared_AR33 <- 1 - (SSE_AR33 / SST_AR33)
print(paste("R^2 =", R_squared_AR33))


# Crear dataframe con fechas (ajustar según tu dataset real)
datos_AR33 <- data.frame(
  Fecha = seq(as.Date("2015-12-27"), as.Date("2015-12-31"), by = "day"),
  Reales = as.numeric(serie_real),
  Predichos_AR33 = as.numeric(AR33_Revertida_pred)
)
print(datos_AR33)
# Graficar comparación
plot(datos_AR33$Fecha, datos_AR33$Reales, type = "o", col = "green", pch = 16, lty = 1,
     ylim = range(c(datos_AR33$Reales, datos_AR33$Predichos_AR33)),
     xlab = "Fecha", ylab = "Valores", main = "Valores reales vs Predicción ARMA(3,3)")
lines(datos_AR33$Fecha, datos_AR33$Predichos_AR33, type = "o", col = "blue", pch = 15, lty = 2)

legend("topleft", legend = c("Reales", "Predicción ARMA(3,3)"),
       col = c("green", "blue"), pch = c(16, 15), lty = c(1, 2), bty = "n")



library(ggplot2)

# Supongamos que tienes tramo1_test1 ya definido y formateado como ts o zoo
# Ajustar modelo a training y predecir sobre test
AR33_tramo1 <- arima(tramo1_train1_ts, order = c(3,0,3))
pAR33_test <- forecast(AR33_tramo1, h = length(tramo1_test1))

# Comparar
real_vs_prediccion <- data.frame(
  Fecha = index(tramo1_test1),
  Real = as.numeric(tramo1_test1),
  Predicho = as.numeric(pAR33_test$mean)
)
print(real_vs_prediccion)
# Plot
ggplot(real_vs_prediccion, aes(x = Fecha)) +
  geom_line(aes(y = Real, color = "Valores Reales"), size = 1.2) +
  geom_line(aes(y = Predicho, color = "Predicción ARMA(3,3)"), size = 1.2, linetype = "dashed") +
  labs(title = "Valores Reales vs Predicción ARMA(3,3)",
       x = "Fecha", y = "Valor") +
  scale_color_manual(values = c("Valores Reales" = "black", "Predicción ARMA(3,3)" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Supongamos que tu modelo entrenado es AR33_tramo1 y tienes tramo1_test1
library(forecast)

# Predicción sobre el conjunto de test
predicciones_AR33 <- forecast(AR33_tramo1, h = length(tramo1_test1))

# Extraer los valores predichos
valores_predichos <- predicciones_AR33$mean

# Crear un data frame con reales y predichos
df_comparacion <- data.frame(
  Fecha = index(tramo1_test1),
  Real = as.numeric(tramo1_test1),
  Predicho = as.numeric(valores_predichos)
)

# Graficar con ggplot2
library(ggplot2)

ggplot(df_comparacion, aes(x = Fecha)) +
  geom_line(aes(y = Real, color = "Real")) +
  geom_line(aes(y = Predicho, color = "Predicho")) +
  labs(title = "Valores Reales vs Predicción ARMA(3,3)",
       x = "Fecha", y = "Valor") +
  scale_color_manual(values = c("Real" = "black", "Predicho" = "blue")) +
  theme_minimal()
SSE <- sum((tramo1_test1 - valores_predichos)^2)
SST <- sum((tramo1_test1 - mean(tramo1_test1))^2)
R_squared <- 1 - (SSE/SST)
print(paste("R² en el test:", round(R_squared, 4)))

SSE <- sum((tramo1_test1 - valores_predichos)^2)
SST <- sum((tramo1_test1 - mean(tramo1_test1))^2)
R_squared <- 1 - (SSE/SST)
print(paste("R² en el test:", round(R_squared, 4)))

SSE <- sum((tramo1_test1 - valores_predichos)^2)
SST <- sum((tramo1_test1 - mean(tramo1_test1))^2)
R_squared <- 1 - (SSE/SST)
print(paste("R² en el test:", round(R_squared, 4)))

residuos_test <- tramo1_test1 - valores_predichos
acf(residuos_test, main = "ACF de residuos del test")
hist(residuos_test, main = "Histograma de residuos del test", xlab = "Error")

# Graficar el conjunto de entrenamiento y test
plot(tramo1_train1, main = "Train vs Test", col = "blue", xlim = range(c(index(tramo1_train1), index(tramo1_test1))))
lines(tramo1_test1, col = "red")
legend("topleft", legend = c("Train", "Test"), col = c("blue", "red"), lty = 1)


# Media y varianza
media_train <- mean(tramo1_train1)
media_test <- mean(tramo1_test1)

var_train <- var(tramo1_train1)
var_test <- var(tramo1_test1)

cat("Media Train:", media_train, "\n")
cat("Media Test:", media_test, "\n")
cat("Varianza Train:", var_train, "\n")
cat("Varianza Test:", var_test, "\n")

library(tseries)

# Prueba ADF para estacionariedad
adf_train <- adf.test(tramo1_train1)
adf_test <- adf.test(tramo1_test1)

cat("p-value ADF Train:", adf_train$p.value, "\n")
cat("p-value ADF Test:", adf_test$p.value, "\n")

# Correlación entre train y test
correlacion <- cor(tramo1_train1, tramo1_test1)
cat("Correlación entre train y test:", correlacion, "\n")

# Descomposición
descomp_train <- decompose(tramo1_train1)
descomp_test <- decompose(tramo1_test1)

# Graficar descomposición
par(mfrow = c(2, 1))
plot(descomp_train)
plot(descomp_test)




# Usar ggplot2 para graficar
library(ggplot2)

# Crear gráfico con ggplot2
ggplot() +
  geom_line(aes(x = index(tramo1_train1), y = as.numeric(tramo1_train1), color = "Train")) +
  geom_line(aes(x = index(tramo1_test1), y = as.numeric(tramo1_test1), color = "Test")) +
  labs(title = "Train vs Test", x = "Fecha", y = "Valor") +
  scale_color_manual(name = "Serie", values = c("Train" = "blue", "Test" = "red")) +
  theme_minimal()


# Crear gráfico con ggplot2
ggplot() +
  # Parte de test en rojo
  geom_line(aes(x = index(tramo1_test1), y = as.numeric(tramo1_test1), color = "Test")) +
  
  # Parte del train correspondiente a test en azul
  geom_line(aes(x = index(tramo1_test1), y = as.numeric(tramo1_train1)[length(tramo1_train1) - length(tramo1_test1) + 1:length(tramo1_test1)], color = "Train")) +
  
  # Configurar título, etiquetas y leyenda
  labs(title = "Train y Test comparados Tramo 1", x = "Fecha", y = "Valor") +
  scale_color_manual(name = "Serie", values = c("Train" = "Black", "Test" = "red")) +
  theme_minimal()




#---------------------------TRAMO 2--------------------------------------------------
# Definir Tramo 2
tramo2 <- window(ibex_zoo, start = as.Date("2015-09-01"), end = as.Date("2017-11-24"))

# División en Train y Test
tramo2_train1 <- window(tramo2, end = as.Date("2017-06-30"))
tramo2_test1 <- window(tramo2, start = as.Date("2017-07-01"))

# Visualizar Train y Test del Tramo 2
plot(tramo2_train1, main = "Train - IBEX 35 (Tramo 2)", ylab = "Cierre", xlab = "Fecha")
plot(tramo2_test1, main = "Test - IBEX 35 (Tramo 2)", ylab = "Cierre", xlab = "Fecha")

# Limpieza y conversión a ts
train2_clean <- na.omit(tramo2_train1)
tramo2_train1_ts <- ts(train2_clean, start = c(2015, 9), frequency = 365)

# Visualizar serie temporal limpia
plot(tramo2_train1_ts, main = "Serie Temporal Limpiada - Tramo 2", ylab = "Cierre", xlab = "Fecha")

# Correlogramas ACF y PACF
par(mfrow = c(1, 2))
acf(tramo2_train1_ts, main = "ACF - Tramo 2", xlab = "Nº Retrasos")
pacf(tramo2_train1_ts, main = "PACF - Tramo 2", xlab = "Nº Retrasos")
par(mfrow = c(1, 1))  # Restaurar ventana

# Test de estacionariedad (ADF)
adf.test(tramo2_train1_ts)
# Si el p-valor > 0.05 → NO es estacionaria. NO ES ESTACIONARIA

# Diferencia manual de orden 1
tramo2_diff1 <- diff(tramo2_train1_ts)

# Verificamos estacionariedad
adf.test(tramo2_diff1)  # Si ahora p < 0.05 → es estacionaria. ES ESTACIONARIA

# Visualizamos la serie diferenciada
plot(tramo2_diff1, main = "Serie diferenciada Tramo 2", ylab = "Diferencia", xlab = "Fecha")

# ACF y PACF para la serie diferenciada
par(mfrow = c(1, 2))
acf(tramo2_diff1, main = "ACF - Tramo 2 diferenciado")
pacf(tramo2_diff1, main = "PACF - Tramo 2 diferenciado")
par(mfrow = c(1, 1))


# ARMA(2,2)
AR22_tramo2 <- arima(tramo2_diff1, order = c(2,0,2))
AR22_tramo2
coeftest(AR22_tramo2)

# ARMA(4,2)
AR42_tramo2 <- arima(tramo2_diff1, order = c(4,0,2))
AR42_tramo2
coeftest(AR42_tramo2)

# ARMA(3,2)
AR32_tramo2 <- arima(tramo2_diff1, order = c(3,0,2))
AR32_tramo2
coeftest(AR32_tramo2)

# ARMA(3,3)
AR33_tramo2 <- arima(tramo2_diff1, order = c(3,0,3))
AR33_tramo2
coeftest(AR33_tramo2)

# ARMA(5,2)
AR52_tramo2 <- arima(tramo2_diff1, order = c(5,0,2))
AR52_tramo2
coeftest(AR52_tramo2)

# Crear un vector vacío para almacenar los AIC de cada modelo
AIC_values <- c()

# Modelo ARMA(2,2)
AIC_values["ARMA(2,2)"] <- AIC(AR22_tramo2)

# Modelo ARMA(4,2)
AIC_values["ARMA(4,2)"] <- AIC(AR42_tramo2)

# Modelo ARMA(3,2)
AIC_values["ARMA(3,2)"] <- AIC(AR32_tramo2)

# Modelo ARMA(3,3)
AIC_values["ARMA(3,3)"] <- AIC(AR33_tramo2)

# Modelo ARMA(5,2)
AIC_values["ARMA(5,2)"] <- AIC(AR52_tramo2)

# Mostrar todos los AIC
print(AIC_values)

# --- Calcular RMSE para cada modelo ---
RMSE_values <- c()

# ARMA(2,2)
res_AR22 <- AR22_tramo2$residuals
RMSE_values["ARMA(2,2)"] <- sqrt(mean(res_AR22^2))

# ARMA(4,2)
res_AR42 <- AR42_tramo2$residuals
RMSE_values["ARMA(4,2)"] <- sqrt(mean(res_AR42^2))

# ARMA(3,2)
res_AR32 <- AR32_tramo2$residuals
RMSE_values["ARMA(3,2)"] <- sqrt(mean(res_AR32^2))

# ARMA(3,3)
res_AR33 <- AR33_tramo2$residuals
RMSE_values["ARMA(3,3)"] <- sqrt(mean(res_AR33^2))

# ARMA(5,2)
res_AR52 <- AR52_tramo2$residuals
RMSE_values["ARMA(5,2)"] <- sqrt(mean(res_AR52^2))

# Mostrar los RMSE de todos los modelos
print(RMSE_values)

# --- Comparar los modelos ---
best_model_by_AIC <- names(AIC_values)[which.min(AIC_values)]
best_model_by_RMSE <- names(RMSE_values)[which.min(RMSE_values)]

cat("✅ Mejor modelo según AIC: ", best_model_by_AIC, "\n")
cat("✅ Mejor modelo según RMSE:", best_model_by_RMSE, "\n")


# Estimación del modelo ARMA(3,3)
AR33_tramo2 <- arima(tramo2_diff1, order = c(3, 0, 3))
summary(AR33_tramo2)
coeftest(AR33_tramo2)

# Residuos del modelo ARMA(3,3)
residuos_AR33_tramo2 <- AR33_tramo2$residuals

# Correlograma de los residuos (ACF y PACF)
par(mfrow = c(1, 2))
acf(residuos_AR33_tramo2, main = "ACF de los residuos ARMA(3,3) - Tramo 2")
pacf(residuos_AR33_tramo2, main = "PACF de los residuos ARMA(3,3) - Tramo 2")

# Test de normalidad de los residuos
jarque.bera.test(residuos_AR33_tramo2)

# Histograma de los residuos
hist(residuos_AR33_tramo2, main = "Histograma de los residuos ARMA(3,3) - Tramo 2", xlab = "Residuos")

# Media de los residuos
mean(residuos_AR33_tramo2)

# Test de independencia de los residuos (Box-Pierce)
Box.test(residuos_AR33_tramo2, type = "Box-Pierce")

# Predicción con forecast() (5 pasos adelante)
pAR33_tramo2 <- forecast(AR33_tramo2, h = 5)
prediccion_forecast_AR33_tramo2 <- pAR33_tramo2$mean

# Predicción con predict()
prediccion_AR33_tramo2 <- predict(AR33_tramo2, n.ahead = 5)
pred_value_AR33_tramo2 <- prediccion_AR33_tramo2$pred

# Usar el último valor real del tramo2 original (antes de diferenciar)
ultimo_valor_tramo2 <- tail(tramo2_train1_ts, 1)

# Revertir diferencia con cumsum
AR33_Revertida_pred_tramo2 <- cumsum(c(ultimo_valor_tramo2, pred_value_AR33_tramo2))[-1]
AR33_Revertida_forecast_tramo2 <- cumsum(c(ultimo_valor_tramo2, prediccion_forecast_AR33_tramo2))[-1]

# Mostrar las predicciones
print(AR33_Revertida_pred_tramo2)
print(AR33_Revertida_forecast_tramo2)

# Graficar ambas predicciones
par(mfrow = c(1, 2))
plot(AR33_Revertida_forecast_tramo2, main = "Forecast - ARMA(3,3) - Tramo 2", type = "o", col = "blue")
plot(AR33_Revertida_pred_tramo2, main = "Predict - ARMA(3,3) - Tramo 2", type = "o", col = "darkgreen")

# Supongamos que estos son los últimos 5 datos reales de la serie original
serie_real_tramo2 <- tail(tramo2_train1_ts, 5)

# Calcular RMSE
rmse_final_AR33_tramo2 <- rmse(serie_real_tramo2, AR33_Revertida_pred_tramo2)
print(paste("RMSE =", rmse_final_AR33_tramo2))

# Calcular R²
SSE_AR33_tramo2 <- sum((serie_real_tramo2 - AR33_Revertida_pred_tramo2)^2)
SST_AR33_tramo2 <- sum((serie_real_tramo2 - mean(serie_real_tramo2))^2)
R_squared_AR33_tramo2 <- 1 - (SSE_AR33_tramo2 / SST_AR33_tramo2)
print(paste("R^2 =", R_squared_AR33_tramo2))


# Ajustar modelo a training y predecir sobre test
AR33_tramo2 <- arima(tramo2_train1_ts, order = c(3, 0, 3))
pAR33_test_tramo2 <- forecast(AR33_tramo2, h = length(tramo2_test1))

# Comparar predicción y valores reales
real_vs_prediccion_tramo2 <- data.frame(
  Fecha = index(tramo2_test1),
  Real = as.numeric(tramo2_test1),
  Predicho = as.numeric(pAR33_test_tramo2$mean)
)
print(real_vs_prediccion_tramo2)
# Graficar comparación
ggplot(real_vs_prediccion_tramo2, aes(x = Fecha)) +
  geom_line(aes(y = Real, color = "Valores Reales"), size = 1.2) +
  geom_line(aes(y = Predicho, color = "Predicción ARMA(3,3)"), size = 1.2, linetype = "dashed") +
  labs(title = "Valores Reales vs Predicción ARMA(3,3) - Tramo 2",
       x = "Fecha", y = "Valor") +
  scale_color_manual(values = c("Valores Reales" = "black", "Predicción ARMA(3,3)" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))







#-------------------------------TRAMO 3------------------------------------------------
# Definir Tramo 3
tramo3 <- window(ibex_zoo, start = as.Date("2017-11-24"), end = as.Date("2019-06-20"))

# División en Train y Test
tramo3_train1 <- window(tramo3, end = as.Date("2019-01-01"))
tramo3_test1 <- window(tramo3, start = as.Date("2019-01-02"))

# Visualizar Train y Test del Tramo 3
plot(tramo3_train1, main = "Train - IBEX 35 (Tramo 3)", ylab = "Cierre", xlab = "Fecha")
plot(tramo3_test1, main = "Test - IBEX 35 (Tramo 3)", ylab = "Cierre", xlab = "Fecha")

# Limpieza y conversión a ts
train3_clean <- na.omit(tramo3_train1)
tramo3_train1_ts <- ts(train3_clean, start = c(2017, 11), frequency = 365)

# Visualizar serie temporal limpia
plot(tramo3_train1_ts, main = "Serie Temporal Limpiada - Tramo 3", ylab = "Cierre", xlab = "Fecha")

# Correlogramas ACF y PACF
par(mfrow = c(1, 2))
acf(tramo3_train1_ts, main = "ACF - Tramo 3", xlab = "Nº Retrasos")
pacf(tramo3_train1_ts, main = "PACF - Tramo 3", xlab = "Nº Retrasos")
par(mfrow = c(1, 1))  # Restaurar ventana

# Test de estacionariedad (ADF)
adf.test(tramo3_train1_ts)
# Si el p-valor > 0.05 → NO es estacionaria

# Diferencia manual de orden 1
tramo3_diff1 <- diff(tramo3_train1_ts)

# Verificamos estacionariedad
adf.test(tramo3_diff1)  # Si p < 0.05 → es estacionaria

# Visualizamos la serie diferenciada
plot(tramo3_diff1, main = "Serie diferenciada Tramo 3", ylab = "Diferencia", xlab = "Fecha")

# ACF y PACF para la serie diferenciada
par(mfrow = c(1, 2))
acf(tramo3_diff1, main = "ACF - Tramo 3 diferenciado")
pacf(tramo3_diff1, main = "PACF - Tramo 3 diferenciado")
par(mfrow = c(1, 1))

# Ajuste de modelos ARMA
AR22_tramo3 <- arima(tramo3_diff1, order = c(2,0,2))
coeftest(AR22_tramo3)
#4 MUY

AR42_tramo3 <- arima(tramo3_diff1, order = c(4,0,2))
coeftest(AR42_tramo3)

AR32_tramo3 <- arima(tramo3_diff1, order = c(3,0,2))
coeftest(AR32_tramo3)

AR33_tramo3 <- arima(tramo3_diff1, order = c(3,0,3))
coeftest(AR33_tramo3)

AR52_tramo3 <- arima(tramo3_diff1, order = c(5,0,2))
coeftest(AR52_tramo3)

# Comparar AIC
AIC_values <- c()
AIC_values["ARMA(2,2)"] <- AIC(AR22_tramo3)
AIC_values["ARMA(4,2)"] <- AIC(AR42_tramo3)
AIC_values["ARMA(3,2)"] <- AIC(AR32_tramo3)
AIC_values["ARMA(3,3)"] <- AIC(AR33_tramo3)
AIC_values["ARMA(5,2)"] <- AIC(AR52_tramo3)
print(AIC_values)

# Comparar RMSE
RMSE_values <- c()
RMSE_values["ARMA(2,2)"] <- sqrt(mean(AR22_tramo3$residuals^2))
RMSE_values["ARMA(4,2)"] <- sqrt(mean(AR42_tramo3$residuals^2))
RMSE_values["ARMA(3,2)"] <- sqrt(mean(AR32_tramo3$residuals^2))
RMSE_values["ARMA(3,3)"] <- sqrt(mean(AR33_tramo3$residuals^2))
RMSE_values["ARMA(5,2)"] <- sqrt(mean(AR52_tramo3$residuals^2))
print(RMSE_values)

# Selección de los mejores modelos
best_model_by_AIC <- names(AIC_values)[which.min(AIC_values)]
best_model_by_RMSE <- names(RMSE_values)[which.min(RMSE_values)]

cat("✅ Mejor modelo según AIC: ", best_model_by_AIC, "\n")
cat("✅ Mejor modelo según RMSE:", best_model_by_RMSE, "\n")

# --- Estimación del modelo ARMA(2,2) ---
AR22_tramo3 <- arima(tramo3_diff1, order = c(2, 0, 2))
summary(AR22_tramo3)
coeftest(AR22_tramo3)

# --- Residuos del modelo ---
residuos_AR22_tramo3 <- AR22_tramo3$residuals

# --- Correlograma ACF y PACF de los residuos ---
par(mfrow = c(1, 2))
acf(residuos_AR22_tramo3, main = "ACF Residuos ARMA(2,2) - Tramo 3")
pacf(residuos_AR22_tramo3, main = "PACF Residuos ARMA(2,2) - Tramo 3")
par(mfrow = c(1, 1))

# --- Test de normalidad ---
jarque.bera.test(residuos_AR22_tramo3)

# --- Histograma de los residuos ---
hist(residuos_AR22_tramo3, main = "Histograma Residuos ARMA(2,2) - Tramo 3", xlab = "Residuos")

# --- Media de los residuos ---
mean(residuos_AR22_tramo3)

# --- Test de independencia de residuos (Box-Pierce) ---
Box.test(residuos_AR22_tramo3, type = "Box-Pierce")

# --- Predicción con forecast() (5 pasos adelante) ---
pAR22_tramo3 <- forecast(AR22_tramo3, h = 5)
pred_forecast_AR22_tramo3 <- pAR22_tramo3$mean

# --- Predicción con predict() (5 pasos) ---
pred_AR22_tramo3 <- predict(AR22_tramo3, n.ahead = 5)
pred_value_AR22_tramo3 <- pred_AR22_tramo3$pred

# --- Revertir la diferencia (predicciones en escala original) ---
ultimo_valor_tramo3 <- tail(tramo3_train1_ts, 1)

AR22_Revertida_pred_tramo3 <- cumsum(c(ultimo_valor_tramo3, pred_value_AR22_tramo3))[-1]
AR22_Revertida_forecast_tramo3 <- cumsum(c(ultimo_valor_tramo3, pred_forecast_AR22_tramo3))[-1]

# --- Mostrar predicciones ---
print(AR22_Revertida_pred_tramo3)
print(AR22_Revertida_forecast_tramo3)

# --- Gráficas de ambas predicciones ---
par(mfrow = c(1, 2))
plot(AR22_Revertida_forecast_tramo3, main = "Forecast - ARMA(2,2) - Tramo 3", type = "o", col = "blue")
plot(AR22_Revertida_pred_tramo3, main = "Predict - ARMA(2,2) - Tramo 3", type = "o", col = "darkgreen")
par(mfrow = c(1, 1))

# --- Últimos 5 datos reales del tramo 3 ---
serie_real_tramo3 <- tail(tramo3_train1_ts, 5)

# --- RMSE ---
rmse_final_AR22_tramo3 <- rmse(serie_real_tramo3, AR22_Revertida_pred_tramo3)
print(paste("RMSE =", rmse_final_AR22_tramo3))

# --- R² ---
SSE_AR22_tramo3 <- sum((serie_real_tramo3 - AR22_Revertida_pred_tramo3)^2)
SST_AR22_tramo3 <- sum((serie_real_tramo3 - mean(serie_real_tramo3))^2)
R_squared_AR22_tramo3 <- 1 - (SSE_AR22_tramo3 / SST_AR22_tramo3)
print(paste("R^2 =", R_squared_AR22_tramo3))

# --- Ajustar modelo a training y predecir sobre test ---
AR22_tramo3 <- arima(tramo3_train1_ts, order = c(2, 0, 2))
pAR22_test_tramo3 <- forecast(AR22_tramo3, h = length(tramo3_test1))

# --- Comparar predicción vs test ---
real_vs_prediccion_tramo3 <- data.frame(
  Fecha = index(tramo3_test1),
  Real = as.numeric(tramo3_test1),
  Predicho = as.numeric(pAR22_test_tramo3$mean)
)
print(real_vs_prediccion_tramo3)

# --- Gráfica de comparación usando ggplot2 ---
library(ggplot2)

ggplot(real_vs_prediccion_tramo3, aes(x = Fecha)) +
  geom_line(aes(y = Real, color = "Valores Reales"), size = 1.2) +
  geom_line(aes(y = Predicho, color = "Predicción ARMA(2,2)"), size = 1.2, linetype = "dashed") +
  labs(title = "Valores Reales vs Predicción ARMA(2,2) - Tramo 3",
       x = "Fecha", y = "Valor") +
  scale_color_manual(values = c("Valores Reales" = "black", "Predicción ARMA(2,2)" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# --- Visualización general Train vs Test ---
ggplot() +
  geom_line(aes(x = index(tramo3_train1), y = as.numeric(tramo3_train1), color = "Train")) +
  geom_line(aes(x = index(tramo3_test1), y = as.numeric(tramo3_test1), color = "Test")) +
  labs(title = "Train vs Test Tramo 3", x = "Fecha", y = "Valor") +
  scale_color_manual(name = "Serie", values = c("Train" = "blue", "Test" = "red")) +
  theme_minimal()

# --- Resaltando Test en la serie total ---
ggplot() +
  geom_line(aes(x = index(tramo3_train1), y = as.numeric(tramo3_train1), color = "Train"), size = 1) + 
  geom_line(aes(x = index(tramo3_test1), y = as.numeric(tramo3_test1), color = "Test"), size = 1, linetype = "dashed") +
  labs(title = "Serie completa con Test Destacado - Tramo 3", x = "Fecha", y = "Valor") +
  scale_color_manual(name = "Serie", values = c("Train" = "blue", "Test" = "red")) +
  theme_minimal() +
  scale_x_date(limits = c(as.Date("2017-11-24"), as.Date("2019-06-20")), date_labels = "%Y-%m-%d", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

# Graficar la serie completa con el Test destacado
ggplot() +
  geom_line(aes(x = index(tramo3_train1), y = as.numeric(tramo3_train1), color = "Train"), size = 1) + 
  geom_line(aes(x = index(tramo3_test1), y = as.numeric(tramo3_test1), color = "Test"), size = 1, linetype = "dashed") +
  labs(title = "Serie completa con Test Destacado - Tramo 3", x = "Fecha", y = "Valor") +
  scale_color_manual(name = "Serie", values = c("Train" = "blue", "Test" = "red")) +
  theme_minimal() +
  scale_x_date(limits = c(as.Date("2017-11-24"), as.Date("2019-06-20")), 
               date_labels = "%Y-%m-%d", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

# Ajustar el modelo ARIMA (2, 0, 2)
AR22_tramo3 <- arima(tramo3_train1_ts, order = c(2, 0, 2))

# Ver la salida del modelo
summary(AR22_tramo3)

# Predicciones para el conjunto Test
predicciones_test <- predict(AR22_tramo3, n.ahead = length(tramo3_test1))$pred

# Verifica que las predicciones no sean 0
print(predicciones_test)

# Crear un dataframe para comparar los valores reales vs predicciones
real_vs_prediccion_tramo3 <- data.frame(
  Fecha = index(tramo3_test1),
  Real = as.numeric(tramo3_test1),
  Predicho = predicciones_test
)

# Ver los primeros registros del dataframe para asegurarte que las fechas y predicciones estén alineadas
head(real_vs_prediccion_tramo3)

# Gráfico de los datos reales vs las predicciones
library(ggplot2)

ggplot(real_vs_prediccion_tramo3, aes(x = Fecha)) +
  geom_line(aes(y = Real, color = "Real"), size = 1) +   # Línea para los datos reales
  geom_line(aes(y = Predicho, color = "Predicción"), size = 1, linetype = "dashed") +  # Línea para las predicciones
  labs(title = "Comparación de Valores Reales vs Predicciones ARIMA - Tramo 3", 
       x = "Fecha", y = "Valor") +
  scale_color_manual(values = c("Real" = "blue", "Predicción" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
# Calcular RMSE entre los valores reales y las predicciones
rmse_final <- sqrt(mean((real_vs_prediccion_tramo3$Real - real_vs_prediccion_tramo3$Predicho)^2))
print(paste("RMSE =", rmse_final))

# Calcular R²
SSE <- sum((real_vs_prediccion_tramo3$Real - real_vs_prediccion_tramo3$Predicho)^2)
SST <- sum((real_vs_prediccion_tramo3$Real - mean(real_vs_prediccion_tramo3$Real))^2)
R_squared <- 1 - (SSE / SST)
print(paste("R² =", R_squared))



# Crear gráfico con ggplot2
ggplot() +
  # Parte de test en rojo
  geom_line(aes(x = index(tramo3_test1), y = as.numeric(tramo3_test1), color = "Test")) +
  
  # Parte del train correspondiente a test en azul
  geom_line(aes(x = index(tramo3_test1), y = as.numeric(tramo3_train1)[length(tramo3_train1) - length(tramo3_test1) + 1:length(tramo3_test1)], color = "Train")) +
  
  # Configurar título, etiquetas y leyenda
  labs(title = "Train y Test comparados Tramo 3", x = "Fecha", y = "Valor") +
  scale_color_manual(name = "Serie", values = c("Train" = "Black", "Test" = "red")) +
  theme_minimal()


# Crear gráfico con ggplot2
ggplot() +
  # Parte de test en rojo
  geom_line(aes(x = index(tramo3_test1), y = as.numeric(tramo3_test1), color = "Test")) +
  
  # Parte del train correspondiente a test en azul
  geom_line(aes(x = index(tramo3_test1), y = as.numeric(tramo3_train1)[length(tramo3_train1) - length(tramo3_test1) + 1:length(tramo3_test1)], color = "Train")) +
  
  # Configurar título, etiquetas y leyenda
  labs(title = "Train y Test comparados Tramo 3", 
       x = "Fecha", y = "Valor") +
  
  # Definir el formato de la fecha para que se muestre el año y otros detalles
  scale_x_date(labels = scales::date_format("%Y-%m-%d"), 
               breaks = "1 months") +
  
  scale_color_manual(name = "Serie", values = c("Train" = "bLack", "Test" = "red")) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Para mejorar la legibilidad de las fechas


#---------------TRAMO 4---------------------------------------------


# Definir Tramo 4
tramo4 <- window(ibex_zoo, start = as.Date("2019-06-20"), end = as.Date("2021-09-17"))

# División en Train y Test
tramo4_train1 <- window(tramo4, end = as.Date("2020-12-31"))
tramo4_test1 <- window(tramo4, start = as.Date("2021-01-01"))

plot(tramo4_train1, main = "Train - IBEX 35 (Tramo 4)", ylab = "Cierre", xlab = "Fecha")
plot(tramo4_test1, main = "Test - IBEX 35 (Tramo 4)", ylab = "Cierre", xlab = "Fecha")

train4_clean <- na.omit(tramo4_train1)
tramo4_train1_ts <- ts(train4_clean, start = c(2019, 6), frequency = 365)
plot(tramo4_train1_ts, main = "Serie Temporal Limpiada - Tramo 4", ylab = "Cierre", xlab = "Fecha")

par(mfrow = c(1, 2))
acf(tramo4_train1_ts, main = "ACF - Tramo 4", xlab = "Nº Retrasos")
pacf(tramo4_train1_ts, main = "PACF - Tramo 4", xlab = "Nº Retrasos")
par(mfrow = c(1, 1))

library(tseries)
adf.test(tramo4_train1_ts)
tramo4_diff1 <- diff(tramo4_train1_ts)
adf.test(tramo4_diff1)
plot(tramo4_diff1, main = "Serie diferenciada Tramo 4", ylab = "Diferencia", xlab = "Fecha")
adf.test(tramo4_diff1)
par(mfrow = c(1, 2))
acf(tramo4_diff1, main = "ACF - Tramo 4 diferenciado")
pacf(tramo4_diff1, main = "PACF - Tramo 4 diferenciado")
par(mfrow = c(1, 1))

AR22_tramo4 <- arima(tramo4_diff1, order = c(2,0,2))
coeftest(AR22_tramo4)
AR42_tramo4 <- arima(tramo4_diff1, order = c(4,0,2))
coeftest(AR42_tramo4)
AR32_tramo4 <- arima(tramo4_diff1, order = c(3,0,2))
coeftest(AR32_tramo4)
AR33_tramo4 <- arima(tramo4_diff1, order = c(3,0,3))
coeftest(AR33_tramo4)
AR52_tramo4 <- arima(tramo4_diff1, order = c(5,0,2))
coeftest(AR52_tramo4)

AIC_values <- c(
  "ARMA(2,2)" = AIC(AR22_tramo4),
  "ARMA(4,2)" = AIC(AR42_tramo4),
  "ARMA(3,2)" = AIC(AR32_tramo4),
  "ARMA(3,3)" = AIC(AR33_tramo4),
  "ARMA(5,2)" = AIC(AR52_tramo4)
)
print(AIC_values)

RMSE_values <- c(
  "ARMA(2,2)" = sqrt(mean(AR22_tramo4$residuals^2)),
  "ARMA(4,2)" = sqrt(mean(AR42_tramo4$residuals^2)),
  "ARMA(3,2)" = sqrt(mean(AR32_tramo4$residuals^2)),
  "ARMA(3,3)" = sqrt(mean(AR33_tramo4$residuals^2)),
  "ARMA(5,2)" = sqrt(mean(AR52_tramo4$residuals^2))
)
print(RMSE_values)

cat("✅ Mejor modelo según AIC:", names(AIC_values)[which.min(AIC_values)], "\n")
cat("✅ Mejor modelo según RMSE:", names(RMSE_values)[which.min(RMSE_values)], "\n")


# Ajustamos modelo elegido 
AR52_tramo4 <- arima(tramo4_diff1, order = c(2, 0, 2))

# Residuos
residuos_AR52_tramo4 <- AR52_tramo4$residuals

# Correlogramas de residuos
par(mfrow = c(1, 2))
acf(residuos_AR52_tramo4, main = "ACF Residuos ARMA(5,2) - Tramo 4")
pacf(residuos_AR52_tramo4, main = "PACF Residuos ARMA(5,2) - Tramo 4")
par(mfrow = c(1, 1))

# Test de normalidad
jarque.bera.test(residuos_AR52_tramo4)

# Histograma residuos
hist(residuos_AR52_tramo4, main = "Histograma Residuos ARMA(5,2) - Tramo 4", xlab = "Residuos")

# Media de los residuos
mean(residuos_AR52_tramo4)

# Test independencia (Box-Pierce)
Box.test(residuos_AR52_tramo4, type = "Box-Pierce")

# Predicciones con forecast() y predict()
library(forecast)
pAR52_tramo4 <- forecast(AR52_tramo4, h = 5)
pred_forecast_AR52_tramo4 <- pAR52_tramo4$mean

pred_AR52_tramo4 <- predict(AR52_tramo4, n.ahead = 5)
pred_value_AR52_tramo4 <- pred_AR52_tramo4$pred

# Revertir diferencia (predicciones en escala original)
ultimo_valor_tramo4 <- tail(tramo4_train1_ts, 1)

AR52_Revertida_pred_tramo4 <- cumsum(c(ultimo_valor_tramo4, pred_value_AR52_tramo4))[-1]
AR52_Revertida_forecast_tramo4 <- cumsum(c(ultimo_valor_tramo4, pred_forecast_AR52_tramo4))[-1]

# Mostrar predicciones
print(AR52_Revertida_pred_tramo4)
print(AR52_Revertida_forecast_tramo4)

# Gráficas de predicciones
par(mfrow = c(1, 2))
plot(AR52_Revertida_forecast_tramo4, main = "Forecast - ARMA(5,2) - Tramo 4", type = "o", col = "blue")
plot(AR52_Revertida_pred_tramo4, main = "Predict - ARMA(5,2) - Tramo 4", type = "o", col = "darkgreen")
par(mfrow = c(1, 1))

# Últimos 5 valores reales
serie_real_tramo4 <- tail(tramo4_train1_ts, 5)

# RMSE
rmse_final_AR52_tramo4 <- sqrt(mean((serie_real_tramo4 - AR52_Revertida_pred_tramo4)^2))
print(paste("RMSE =", rmse_final_AR52_tramo4))

# R²
SSE <- sum((serie_real_tramo4 - AR52_Revertida_pred_tramo4)^2)
SST <- sum((serie_real_tramo4 - mean(serie_real_tramo4))^2)
R_squared <- 1 - (SSE / SST)
print(paste("R² =", R_squared))

# Ajustar modelo final completo y predecir sobre test
AR52_tramo4_completo <- arima(tramo4_train1_ts, order = c(5, 0, 2))
pAR52_test_tramo4 <- forecast(AR52_tramo4_completo, h = length(tramo4_test1))

# Comparación real vs predicción
real_vs_prediccion_AR52_tramo4 <- data.frame(
  Fecha = index(tramo4_test1),
  Real = as.numeric(tramo4_test1),
  Predicho = as.numeric(pAR52_test_tramo4$mean)
)

# Gráfico comparativo
library(ggplot2)
ggplot(real_vs_prediccion_AR52_tramo4, aes(x = Fecha)) +
  geom_line(aes(y = Real, color = "Valores Reales"), size = 1.2) +
  geom_line(aes(y = Predicho, color = "Predicción ARMA(5,2)"), size = 1.2, linetype = "dashed") +
  labs(title = "Valores Reales vs Predicción ARMA(5,2) - Tramo 4",
       x = "Fecha", y = "Valor") +
  scale_color_manual(values = c("Valores Reales" = "black", "Predicción ARMA(5,2)" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Crear gráfico comparativo Train vs Test para Tramo 4
library(ggplot2)

ggplot() +
  # Parte del test en rojo
  geom_line(aes(x = index(tramo4_test1), y = as.numeric(tramo4_test1), color = "Test")) +
  
  # Parte del train correspondiente en azul
  geom_line(aes(x = index(tramo4_test1), 
                y = tail(as.numeric(tramo4_train1), length(tramo4_test1)), 
                color = "Train")) +
  
  # Configurar título, etiquetas y leyenda
  labs(title = "Train y Test comparados - Tramo 4", 
       x = "Fecha", y = "Valor") +
  
  # Formato de fechas
  scale_x_date(labels = scales::date_format("%Y-%m-%d"), 
               breaks = "1 months") +
  
  # Colores personalizados
  scale_color_manual(name = "Serie", 
                     values = c("Train" = "black", "Test" = "red")) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))
# Crear tabla comparativa: valores reales vs predichos (modelo ARMA(5,2))
comparacion_tramo4 <- data.frame(
  Fecha = index(tramo4_test1),
  Real = as.numeric(tramo4_test1),
  Predicho = as.numeric(pAR52_test_tramo4$mean)  # Asegúrate de haber corrido forecast(AR52_tramo4, h = ...)
)

print(comparacion_tramo4)

library(ggplot2)

ggplot(comparacion_tramo4, aes(x = Fecha)) +
  geom_line(aes(y = Real, color = "Real"), size = 1.2) +
  geom_line(aes(y = Predicho, color = "Predicho"), size = 1.2, linetype = "dashed") +
  labs(title = "Valores Reales vs Predicción ARMA(5,2) - Tramo 4",
       x = "Fecha", y = "Valor") +
  scale_color_manual(values = c("Real" = "black", "Predicho" = "blue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# RMSE
rmse_AR52_tramo4 <- sqrt(mean((comparacion_tramo4$Real - comparacion_tramo4$Predicho)^2))
print(paste("RMSE =", round(rmse_AR52_tramo4, 4)))
# MAE
mae_AR52_tramo4 <- mean(abs(comparacion_tramo4$Real - comparacion_tramo4$Predicho))
print(paste("MAE =", round(mae_AR52_tramo4, 4)))
# R²
SSE <- sum((comparacion_tramo4$Real - comparacion_tramo4$Predicho)^2)
SST <- sum((comparacion_tramo4$Real - mean(comparacion_tramo4$Real))^2)
R2_AR52_tramo4 <- 1 - SSE/SST
print(paste("R² =", round(R2_AR52_tramo4, 4)))

plot(comparacion_tramo4$Real - comparacion_tramo4$Predicho, type = "o", col = "red",
     main = "Errores de Predicción", ylab = "Error", xlab = "Índice")
abline(h = 0, col = "blue", lty = 2)
LO 



# Crear gráfico con ggplot2
ggplot() +
  # Parte de test en rojo
  geom_line(aes(x = index(tramo4_test1), y = as.numeric(tramo4_test1), color = "Test")) +
  
  # Parte del train correspondiente a test en azul
  geom_line(aes(x = index(tramo4_test1), y = as.numeric(tramo4_train1)[length(tramo4_train1) - length(tramo4_test1) + 1:length(tramo4_test1)], color = "Train")) +
  
  # Configurar título, etiquetas y leyenda
  labs(title = "Train y Test comparados Tramo 4", 
       x = "Fecha", y = "Valor") +
  
  # Definir el formato de la fecha para que se muestre el año y otros detalles
  scale_x_date(labels = scales::date_format("%Y-%m-%d"), 
               breaks = "1 months") +
  
  scale_color_manual(name = "Serie", values = c("Train" = "bLack", "Test" = "red")) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Para mejorar la legibilidad de las fechas


#---------------------------------tramo 4.2----------------------------------------------------------------------------------------



# --- Estimación del modelo ARMA(2,2) ---
AR52_tramo4 <- arima(tramo4_diff1, order = c(5, 0, 2))
summary(AR52_tramo4)
coeftest(AR52_tramo4)

# --- Residuos del modelo ---
residuos_AR52_tramo4 <- AR52_tramo4$residuals

# --- Correlograma ACF y PACF de los residuos ---
par(mfrow = c(1, 2))
acf(residuos_AR52_tramo4, main = "ACF Residuos ARMA(2,2) - Tramo 4")
pacf(residuos_AR52_tramo4, main = "PACF Residuos ARMA(2,2) - Tramo 4")
par(mfrow = c(1, 1))

# --- Test de normalidad ---
jarque.bera.test(residuos_AR52_tramo4)

# --- Histograma de los residuos ---
hist(residuos_AR52_tramo4, main = "Histograma Residuos ARMA(2,2) - Tramo 4", xlab = "Residuos")

# --- Media de los residuos ---
mean(residuos_AR52_tramo4)

# --- Test de independencia de residuos (Box-Pierce) ---
Box.test(residuos_AR52_tramo4, type = "Box-Pierce")

# --- Predicción con forecast() (5 pasos adelante) ---
pAR52_tramo4 <- forecast(AR52_tramo4, h = 5)
pred_forecast_AR52_tramo4 <- pAR52_tramo4$mean

# --- Predicción con predict() (5 pasos) ---
pred_AR52_tramo4 <- predict(AR52_tramo4, n.ahead = 5)
pred_value_AR52_tramo4 <- pred_AR52_tramo4$pred

# --- Revertir la diferencia (predicciones en escala original) ---
ultimo_valor_tramo4 <- tail(tramo4_train1_ts, 1)

AR52_Revertida_pred_tramo4 <- cumsum(c(ultimo_valor_tramo4, pred_value_AR52_tramo4))[-1]
AR52_Revertida_forecast_tramo4 <- cumsum(c(ultimo_valor_tramo4, pred_forecast_AR52_tramo4))[-1]

# --- Mostrar predicciones ---
print(AR52_Revertida_pred_tramo4)
print(AR52_Revertida_forecast_tramo4)

# --- Gráficas de ambas predicciones ---
par(mfrow = c(1, 2))
plot(AR52_Revertida_forecast_tramo4, main = "Forecast - ARMA(2,2) - Tramo 4", type = "o", col = "blue")
plot(AR52_Revertida_pred_tramo4, main = "Predict - ARMA(2,2) - Tramo 4", type = "o", col = "darkgreen")
par(mfrow = c(1, 1))

# --- Últimos 5 datos reales del tramo 4 ---
serie_real_tramo4 <- tail(tramo4_train1_ts, 5)

# --- RMSE ---
rmse_final_AR52_tramo4 <- rmse(serie_real_tramo4, AR52_Revertida_pred_tramo4)
print(paste("RMSE =", rmse_final_AR52_tramo4))

# --- R² ---
SSE_AR52_tramo4 <- sum((serie_real_tramo4 - AR52_Revertida_pred_tramo4)^2)
SST_AR52_tramo4 <- sum((serie_real_tramo4 - mean(serie_real_tramo4))^2)
R_squared_AR52_tramo4 <- 1 - (SSE_AR52_tramo4 / SST_AR52_tramo4)
print(paste("R^2 =", R_squared_AR52_tramo4))

# --- Ajustar modelo a training y predecir sobre test ---
AR52_tramo4 <- arima(tramo4_train1_ts, order = c(2, 0, 2))
pAR52_test_tramo4 <- forecast(AR52_tramo4, h = length(tramo4_test1))

# --- Comparar predicción vs test ---
real_vs_prediccion_tramo4 <- data.frame(
  Fecha = index(tramo4_test1),
  Real = as.numeric(tramo4_test1),
  Predicho = as.numeric(pAR52_test_tramo4$mean)
)

# --- Gráfica de comparación usando ggplot2 ---
library(ggplot2)

ggplot(real_vs_prediccion_tramo4, aes(x = Fecha)) +
  geom_line(aes(y = Real, color = "Valores Reales"), size = 1.2) +
  geom_line(aes(y = Predicho, color = "Predicción ARMA(2,2)"), size = 1.2, linetype = "dashed") +
  labs(title = "Valores Reales vs Predicción ARMA(2,2) - Tramo 4",
       x = "Fecha", y = "Valor") +
  scale_color_manual(values = c("Valores Reales" = "black", "Predicción ARMA(2,2)" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# --- Visualización general Train vs Test ---
ggplot() +
  geom_line(aes(x = index(tramo4_train1), y = as.numeric(tramo4_train1), color = "Train")) +
  geom_line(aes(x = index(tramo4_test1), y = as.numeric(tramo4_test1), color = "Test")) +
  labs(title = "Train vs Test Tramo 4", x = "Fecha", y = "Valor") +
  scale_color_manual(name = "Serie", values = c("Train" = "blue", "Test" = "red")) +
  theme_minimal()

# --- Resaltando Test en la serie total ---
ggplot() +
  geom_line(aes(x = index(tramo4_train1), y = as.numeric(tramo4_train1), color = "Train"), size = 1) + 
  geom_line(aes(x = index(tramo4_test1), y = as.numeric(tramo4_test1), color = "Test"), size = 1, linetype = "dashed") +
  labs(title = "Serie completa con Test Destacado - Tramo 4", x = "Fecha", y = "Valor") +
  scale_color_manual(name = "Serie", values = c("Train" = "blue", "Test" = "red")) +
  theme_minimal() +
  scale_x_date(limits = c(as.Date("2017-11-24"), as.Date("2019-06-20")), date_labels = "%Y-%m-%d", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

# Graficar la serie completa con el Test destacado
ggplot() +
  geom_line(aes(x = index(tramo4_train1), y = as.numeric(tramo4_train1), color = "Train"), size = 1) + 
  geom_line(aes(x = index(tramo4_test1), y = as.numeric(tramo4_test1), color = "Test"), size = 1, linetype = "dashed") +
  labs(title = "Serie completa con Test Destacado - Tramo 4", x = "Fecha", y = "Valor") +
  scale_color_manual(name = "Serie", values = c("Train" = "blue", "Test" = "red")) +
  theme_minimal() +
  scale_x_date(limits = c(as.Date("2017-11-24"), as.Date("2019-06-20")), 
               date_labels = "%Y-%m-%d", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

# Ajustar el modelo ARIMA (2, 0, 2)
AR52_tramo4 <- arima(tramo4_train1_ts, order = c(2, 0, 2))

# Ver la salida del modelo
summary(AR52_tramo4)

# Predicciones para el conjunto Test
predicciones_test <- predict(AR52_tramo4, n.ahead = length(tramo4_test1))$pred

# Verifica que las predicciones no sean 0
print(predicciones_test)

# Crear un dataframe para comparar los valores reales vs predicciones
real_vs_prediccion_tramo4 <- data.frame(
  Fecha = index(tramo4_test1),
  Real = as.numeric(tramo4_test1),
  Predicho = predicciones_test
)

# Ver los primeros registros del dataframe para asegurarte que las fechas y predicciones estén alineadas
head(real_vs_prediccion_tramo4)

# Gráfico de los datos reales vs las predicciones
ggplot(real_vs_prediccion_tramo4, aes(x = Fecha)) +
  geom_line(aes(y = Real, color = "Real"), size = 1) +
  geom_line(aes(y = Predicho, color = "Predicción"), size = 1, linetype = "dashed") +
  labs(title = "Comparación de Valores Reales vs Predicciones ARIMA - Tramo 4", 
       x = "Fecha", y = "Valor") +
  scale_color_manual(values = c("Real" = "blue", "Predicción" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Calcular RMSE entre los valores reales y las predicciones
rmse_final <- sqrt(mean((real_vs_prediccion_tramo4$Real - real_vs_prediccion_tramo4$Predicho)^2))
print(paste("RMSE =", rmse_final))

# Calcular R²
SSE <- sum((real_vs_prediccion_tramo4$Real - real_vs_prediccion_tramo4$Predicho)^2)
SST <- sum((real_vs_prediccion_tramo4$Real - mean(real_vs_prediccion_tramo4$Real))^2)
R_squared <- 1 - (SSE / SST)
print(paste("R² =", R_squared))

# Crear gráfico con ggplot2
ggplot() +
  geom_line(aes(x = index(tramo4_test1), y = as.numeric(tramo4_test1), color = "Test")) +
  geom_line(aes(x = index(tramo4_test1), y = as.numeric(tramo4_train1)[length(tramo4_train1) - length(tramo4_test1) + 1:length(tramo4_test1)], color = "Train")) +
  labs(title = "Train y Test comparados Tramo 4", x = "Fecha", y = "Valor") +
  scale_color_manual(name = "Serie", values = c("Train" = "Black", "Test" = "red")) +
  theme_minimal()

ggplot() +
  geom_line(aes(x = index(tramo4_test1), y = as.numeric(tramo4_test1), color = "Test")) +
  geom_line(aes(x = index(tramo4_test1), y = as.numeric(tramo4_train1)[length(tramo4_train1) - length(tramo4_test1) + 1:length(tramo4_test1)], color = "Train")) +
  labs(title = "Train y Test comparados Tramo 4", x = "Fecha", y = "Valor") +
  scale_x_date(labels = scales::date_format("%Y-%m-%d"), breaks = "1 months") +
  scale_color_manual(name = "Serie", values = c("Train" = "bLack", "Test" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#---------------------TRAMO 5--------------------------------------


# 1. Datos de precios
tramo5 <- window(ibex_zoo, start = as.Date("2021-09-20"), end = as.Date("2025-02-01"))

# 2. División Train/Test
tramo5_train1 <- window(tramo5, end = as.Date("2023-06-30"))
tramo5_test1  <- window(tramo5, start = as.Date("2025-01-01"))

# 3. Limpieza y conversión a ts
train5_clean <- na.omit(tramo5_train1)
tramo5_train1_ts <- ts(train5_clean, frequency = 365)

par(mfrow = c(1, 2))
acf(tramo4_train1_ts, main = "ACF - Tramo 4", xlab = "Nº Retrasos")
pacf(tramo4_train1_ts, main = "PACF - Tramo 4", xlab = "Nº Retrasos")
par(mfrow = c(1, 1))



# 4. Ajustar ARMA(3,2)
library(forecast)
AR32_tramo5 <- arima(tramo5_train1_ts, order = c(3, 0, 2))

# 5. Predicción sobre el test
pAR32_test_tramo5 <- forecast(AR32_tramo5, h = length(tramo5_test1))

# Tabla con valores reales y predichos
comparacion_tramo5 <- data.frame(
  Fecha = index(tramo5_test1),
  Real = as.numeric(tramo5_test1),
  Predicho = as.numeric(pAR32_test_tramo5$mean)
)

library(ggplot2)

ggplot(comparacion_tramo5, aes(x = Fecha)) +
  geom_line(aes(y = Real, color = "Real"), size = 1.2) +
  geom_line(aes(y = Predicho, color = "Predicho"), size = 1.2, linetype = "dashed") +
  labs(title = "Valores Reales vs Predicción ARMA(3,2) - Tramo 5",
       x = "Fecha", y = "Valor") +
  scale_color_manual(values = c("Real" = "black", "Predicho" = "blue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))


ggplot() +
  geom_line(aes(x = index(tramo5_test1), y = as.numeric(tramo5_test1), color = "Test")) +
  geom_line(aes(x = index(tramo5_test1), 
                y = tail(as.numeric(tramo5_train1), length(tramo5_test1)), 
                color = "Train")) +
  labs(title = "Train y Test comparados - Tramo 5", 
       x = "Fecha", y = "Valor") +
  scale_x_date(labels = scales::date_format("%Y-%m-%d"), breaks = "1 month") +
  scale_color_manual(name = "Serie", values = c("Train" = "black", "Test" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))
# Crear gráfico con ggplot2
ggplot() +
  # Parte de test en rojo
  geom_line(aes(x = index(tramo5_test1), y = as.numeric(tramo5_test1), color = "Test")) +
  
  # Parte del train correspondiente a test en azul
  geom_line(aes(x = index(tramo5_test1), y = as.numeric(tramo5_train1)[length(tramo5_train1) - length(tramo5_test1) + 1:length(tramo5_test1)], color = "Train")) +
  
  # Configurar título, etiquetas y leyenda
  labs(title = "Train y Test comparados Tramo 5", 
       x = "Fecha", y = "Valor") +
  
  # Definir el formato de la fecha para que se muestre el año y otros detalles
  scale_x_date(labels = scales::date_format("%Y-%m-%d"), 
               breaks = "1 months") +
  
  scale_color_manual(name = "Serie", values = c("Train" = "bLack", "Test" = "red")) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Para mejorar la legibilidad de las fechas


#--------------tramo 5.1---------
# Definir Tramo 5
tramo5 <- window(ibex_zoo, start = as.Date("2021-09-20"), end = as.Date("2025-01-01"))
plot(tramo5)
# División en Train y Test
tramo5_train <- window(tramo5, end = as.Date("2023-06-30"))
tramo5_test  <- window(tramo5, start = as.Date("2023-07-01"))

# Visualizar Train y Test del Tramo 5
plot(tramo5_train, main = "Train - IBEX 35 (Tramo 5)", ylab = "Cierre", xlab = "Fecha")
plot(tramo5_test, main = "Test - IBEX 35 (Tramo 5)", ylab = "Cierre", xlab = "Fecha")

# Limpieza y conversión a ts
train5_clean <- na.omit(tramo5_train)
tramo5_train_ts <- ts(train5_clean, start = c(2021, 9), frequency = 365)

# Visualizar serie temporal limpia
plot(tramo5_train_ts, main = "Serie Temporal Limpiada - Tramo 5", ylab = "Cierre", xlab = "Fecha")

# Correlogramas ACF y PACF
par(mfrow = c(1, 2))
acf(tramo5_train_ts, main = "ACF - Tramo 5", xlab = "Nº Retrasos")
pacf(tramo5_train_ts, main = "PACF - Tramo 5", xlab = "Nº Retrasos")
par(mfrow = c(1, 1))  # Restaurar ventana

# Test de estacionariedad (ADF)
adf.test(tramo5_train_ts)
# Si el p-valor > 0.05 → NO es estacionaria

# Diferencia manual de orden 1
tramo5_diff1 <- diff(tramo5_train_ts)

# Verificamos estacionariedad
adf.test(tramo5_diff1)  # Si p < 0.05 → es estacionaria

# Visualizamos la serie diferenciada
plot(tramo5_diff1, main = "Serie diferenciada Tramo 5", ylab = "Diferencia", xlab = "Fecha")

# ACF y PACF para la serie diferenciada
par(mfrow = c(1, 2))
acf(tramo5_diff1, main = "ACF - Tramo 5 diferenciado")
pacf(tramo5_diff1, main = "PACF - Tramo 5 diferenciado")
par(mfrow = c(1, 1))

# Ajuste de modelos ARMA
AR22_tramo5 <- arima(tramo5_diff1, order = c(2,0,2))
coeftest(AR22_tramo5)
#4 MUY

AR42_tramo5 <- arima(tramo5_diff1, order = c(4,0,2))
coeftest(AR42_tramo5)

AR32_tramo5 <- arima(tramo5_diff1, order = c(3,0,2))
coeftest(AR32_tramo5)

AR33_tramo5 <- arima(tramo5_diff1, order = c(3,0,3))
coeftest(AR33_tramo5)

AR52_tramo5 <- arima(tramo5_diff1, order = c(5,0,2))
coeftest(AR52_tramo5)

# Comparar AIC
AIC_values <- c()
AIC_values["ARMA(2,2)"] <- AIC(AR22_tramo5)
AIC_values["ARMA(4,2)"] <- AIC(AR42_tramo5)
AIC_values["ARMA(3,2)"] <- AIC(AR32_tramo5)
AIC_values["ARMA(3,3)"] <- AIC(AR33_tramo5)
AIC_values["ARMA(5,2)"] <- AIC(AR52_tramo5)
print(AIC_values)

# Comparar RMSE
RMSE_values <- c()
RMSE_values["ARMA(2,2)"] <- sqrt(mean(AR22_tramo5$residuals^2))
RMSE_values["ARMA(4,2)"] <- sqrt(mean(AR42_tramo5$residuals^2))
RMSE_values["ARMA(3,2)"] <- sqrt(mean(AR32_tramo5$residuals^2))
RMSE_values["ARMA(3,3)"] <- sqrt(mean(AR33_tramo5$residuals^2))
RMSE_values["ARMA(5,2)"] <- sqrt(mean(AR52_tramo5$residuals^2))
print(RMSE_values)

# Selección de los mejores modelos
best_model_by_AIC <- names(AIC_values)[which.min(AIC_values)]
best_model_by_RMSE <- names(RMSE_values)[which.min(RMSE_values)]

cat("✅ Mejor modelo según AIC: ", best_model_by_AIC, "\n")
cat("✅ Mejor modelo según RMSE:", best_model_by_RMSE, "\n")



library(forecast)
library(lmtest)
library(tseries)
library(ggplot2)
library(Metrics)

# --- Modelo ARMA(3,3) sobre la serie diferenciada del TRAIN ---
modelo_AR33 <- arima(tramo5_diff1, order = c(3, 0, 3))
summary(modelo_AR33)
coeftest(modelo_AR33)

# --- Diagnóstico de residuos ---
residuos <- residuals(modelo_AR33)

par(mfrow = c(1, 2))
acf(residuos, main = "ACF Residuos ARMA(3,3)")
pacf(residuos, main = "PACF Residuos ARMA(3,3)")
par(mfrow = c(1, 1))

jarque.bera.test(residuos)
Box.test(residuos, type = "Box-Pierce")
hist(residuos, main = "Histograma de Residuos", xlab = "Residuos")
mean(residuos)

# --- Predicción del modelo: mismos pasos que TEST ---
n_test <- length(tramo5_test)
predicciones_diff <- forecast(modelo_AR33, h = n_test)

# --- Revertimos la diferencia para volver a escala original ---
ultimo_train <- tail(tramo5_train_ts, 1)
predicciones_nivel <- cumsum(c(ultimo_train, predicciones_diff$mean))[-1]

# --- Comparar predicciones con valores reales ---
real_vs_pred <- data.frame(
  Fecha = as.Date(time(tramo5_test)),
  Real = as.numeric(tramo5_test),
  Predicho = as.numeric(predicciones_nivel)
)

# --- Gráfica comparativa ---
ggplot(real_vs_pred, aes(x = Fecha)) +
  geom_line(aes(y = Real, color = "Real"), size = 1.2) +
  geom_line(aes(y = Predicho, color = "Predicho"), size = 1.2, linetype = "dashed") +
  scale_color_manual(name = "Serie", values = c("Real" = "black", "Predicho" = "red")) +
  labs(title = "Predicción ARMA(3,3) vs Valores Reales - Tramo 5",
       x = "Fecha", y = "Cierre IBEX 35") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# --- Evaluación del modelo ---
rmse_modelo <- rmse(real_vs_pred$Real, real_vs_pred$Predicho)
cat("📉 RMSE sobre Test =", rmse_modelo, "\n")

SSE <- sum((real_vs_pred$Real - real_vs_pred$Predicho)^2)
SST <- sum((real_vs_pred$Real - mean(real_vs_pred$Real))^2)
R2 <- 1 - (SSE / SST)
cat("📈 R^2 sobre Test =", R2, "\n")

par(mfrow = c(1, 2))
acf(residuos, main = "ACF Residuos ARMA(3,3)", col = "blue", lwd = 2)
pacf(residuos, main = "PACF Residuos ARMA(3,3)", col = "darkgreen", lwd = 2)
par(mfrow = c(1, 1))


hist(residuos, main = "Histograma de Residuos ARMA(3,3)", 
     col = "lightgray", border = "black", xlab = "Residuos", breaks = 20)


ggplot(real_vs_pred, aes(x = Fecha)) +
  geom_line(aes(y = Real, color = "Real"), size = 1.2) +
  geom_line(aes(y = Predicho, color = "Predicho"), size = 1.2, linetype = "dashed") +
  scale_color_manual(name = "Serie", values = c("Real" = "black", "Predicho" = "red")) +
  labs(title = "Predicción ARMA(3,3) vs Valores Reales - Tramo 5",
       x = "Fecha", y = "Cierre IBEX 35") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "top")


ggplot() +
  geom_line(aes(x = index(tramo5_train), y = as.numeric(tramo5_train), color = "Train"), size = 1) +
  geom_line(aes(x = index(tramo5_test), y = as.numeric(tramo5_test), color = "Test"), size = 1, linetype = "dashed") +
  scale_color_manual(name = "Serie", values = c("Train" = "blue", "Test" = "red")) +
  labs(title = "Serie completa - Tramo 5 (Train y Test)", x = "Fecha", y = "Cierre") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "top")




ggplot() +
  geom_line(aes(x = index(tramo5_train1), y = as.numeric(tramo5_train1), color = "Train"), size = 1) + 
  geom_line(aes(x = index(tramo5_test1), y = as.numeric(tramo5_test1), color = "Test"), size = 1, linetype = "dashed") +
  labs(title = "Serie completa con Test Destacado - Tramo 5", x = "Fecha", y = "Valor") +
  scale_color_manual(name = "Serie", values = c("Train" = "blue", "Test" = "red")) +
  scale_x_date(limits = c(as.Date("2021-09-20"), as.Date("2025-01-01")), 
               date_labels = "%Y-%m-%d", date_breaks = "3 months") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "top")



library(ggplot2)

# Crear dataframe combinado para graficar todo junto
df_completo <- data.frame(
  Fecha = c(index(tramo5_train), index(tramo5_test), index(tramo5_test)),
  Valor = c(as.numeric(tramo5_train), as.numeric(tramo5_test), real_vs_pred$Predicho),
  Tipo = c(rep("Train", length(tramo5_train)),
           rep("Test Real", length(tramo5_test)),
           rep("Predicción", length(real_vs_pred$Predicho)))
)

# Gráfico
ggplot(df_completo, aes(x = Fecha, y = Valor, color = Tipo)) +
  geom_line(size = 1.2, aes(linetype = Tipo)) +
  scale_color_manual(values = c("Train" = "blue", "Test Real" = "black", "Predicción" = "red")) +
  scale_linetype_manual(values = c("Train" = "solid", "Test Real" = "solid", "Predicción" = "dashed")) +
  labs(title = "Train vs Test vs Predicción ARMA(3,3) - Tramo 5",
       x = "Fecha", y = "Cierre IBEX 35") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months")

library(ggplot2)

# Obtener últimos valores del train para contexto (ej: últimos 30 días)
dias_contexto <- 30  # puedes ajustar este número
fechas_train_tail <- tail(index(tramo5_train), dias_contexto)
valores_train_tail <- tail(as.numeric(tramo5_train), dias_contexto)

# Construir dataframes para cada parte
df_train_tail <- data.frame(
  Fecha = fechas_train_tail,
  Valor = valores_train_tail,
  Tipo = "Train"
)

df_test_real <- data.frame(
  Fecha = index(tramo5_test),
  Valor = as.numeric(tramo5_test),
  Tipo = "Test Real"
)

df_prediccion <- data.frame(
  Fecha = real_vs_pred$Fecha,
  Valor = real_vs_pred$Predicho,
  Tipo = "Predicción"
)

# Unir todo
df_comparacion <- rbind(df_train_tail, df_test_real, df_prediccion)

# Gráfico
ggplot(df_comparacion, aes(x = Fecha, y = Valor, color = Tipo)) +
  geom_line(aes(linetype = Tipo), size = 1.2) +
  scale_color_manual(values = c("Train" = "blue", "Test Real" = "black", "Predicción" = "red")) +
  scale_linetype_manual(values = c("Train" = "solid", "Test Real" = "solid", "Predicción" = "dashed")) +
  labs(title = "Comparación Local: Final del Train vs Test y Predicción",
       x = "Fecha", y = "Cierre IBEX 35") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "2 weeks")


library(ggplot2)

# Construir el dataframe para los valores reales del Test y las predicciones
df_test_real <- data.frame(
  Fecha = index(tramo5_test),
  Valor = as.numeric(tramo5_test),
  Tipo = "Test Real"
)

df_prediccion <- data.frame(
  Fecha = real_vs_pred$Fecha,
  Valor = as.numeric(real_vs_pred$Predicho),
  Tipo = "Predicción"
)

# Unir ambos dataframes
df_comparacion <- rbind(df_test_real, df_prediccion)

# Gráfico
ggplot(df_comparacion, aes(x = Fecha, y = Valor, color = Tipo)) +
  geom_line(aes(linetype = Tipo), size = 1.2) + 
  geom_point(aes(color = Tipo), size = 2) +  # Agregar puntos para hacer la predicción más visual
  scale_color_manual(values = c("Test Real" = "black", "Predicción" = "red")) +
  scale_linetype_manual(values = c("Test Real" = "solid", "Predicción" = "dashed")) +
  labs(title = "Predicción ARMA(3,3) vs Valores Reales - Tramo 5 (Test)",
       x = "Fecha", y = "Cierre IBEX 35") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month")


# Gráfico de comparación entre Train y Test para Tramo 5
ggplot() +
  geom_line(aes(x = index(tramo5_test), y = as.numeric(tramo5_test), color = "Test")) +
  geom_line(aes(x = index(tramo5_test), y = as.numeric(tramo5_train)[length(tramo5_train) - length(tramo5_test) + 1:length(tramo5_test)], color = "Train")) +
  labs(title = "Train y Test comparados Tramo 5", x = "Fecha", y = "Valor") +
  scale_color_manual(name = "Serie", values = c("Train" = "black", "Test" = "red")) +
  theme_minimal()

# Gráfico con fechas formateadas para Tramo 5
ggplot() +
  geom_line(aes(x = index(tramo5_test), y = as.numeric(tramo5_test), color = "Test")) +
  geom_line(aes(x = index(tramo5_test), y = as.numeric(tramo5_train)[length(tramo5_train) - length(tramo5_test) + 1:length(tramo5_test)], color = "Train")) +
  labs(title = "Train y Test comparados Tramo 5", x = "Fecha", y = "Valor") +
  scale_x_date(labels = scales::date_format("%Y-%m-%d"), breaks = "1 month") +
  scale_color_manual(name = "Serie", values = c("Train" = "black", "Test" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# --- Estimación del modelo ARMA(3,3) ---
AR33_tramo5 <- arima(tramo5_diff1, order = c(3, 0, 3))
summary(AR33_tramo5)
coeftest(AR33_tramo5)

# --- Residuos del modelo ---
residuos_AR33_tramo5 <- AR33_tramo5$residuals

# --- Correlograma ACF y PACF de los residuos ---
par(mfrow = c(1, 2))
acf(residuos_AR33_tramo5, main = "ACF Residuos ARMA(3,3) - Tramo 5")
pacf(residuos_AR33_tramo5, main = "PACF Residuos ARMA(3,3) - Tramo 5")
par(mfrow = c(1, 1))

# --- Test de normalidad ---
jarque.bera.test(residuos_AR33_tramo5)

# --- Histograma de los residuos ---
hist(residuos_AR33_tramo5, main = "Histograma Residuos ARMA(3,3) - Tramo 5", xlab = "Residuos")

# --- Media de los residuos ---
mean(residuos_AR33_tramo5)

# --- Test de independencia de residuos (Box-Pierce) ---
Box.test(residuos_AR33_tramo5, type = "Box-Pierce")

# --- Predicción con forecast() (5 pasos adelante) ---
pAR33_tramo5 <- forecast(AR33_tramo5, h = 5)
pred_forecast_AR33_tramo5 <- pAR33_tramo5$mean

# --- Predicción con predict() (5 pasos) ---
pred_AR33_tramo5 <- predict(AR33_tramo5, n.ahead = 5)
pred_value_AR33_tramo5 <- pred_AR33_tramo5$pred

# --- Revertir la diferencia (predicciones en escala original) ---
ultimo_valor_tramo5 <- tail(tramo5_train1_ts, 1)

AR33_Revertida_pred_tramo5 <- cumsum(c(ultimo_valor_tramo5, pred_value_AR33_tramo5))[-1]
AR33_Revertida_forecast_tramo5 <- cumsum(c(ultimo_valor_tramo5, pred_forecast_AR33_tramo5))[-1]

# --- Mostrar predicciones ---
print(AR33_Revertida_pred_tramo5)
print(AR33_Revertida_forecast_tramo5)

# --- Gráficas de ambas predicciones ---
par(mfrow = c(1, 2))
plot(AR33_Revertida_forecast_tramo5, main = "Forecast - ARMA(3,3) - Tramo 5", type = "o", col = "blue")
plot(AR33_Revertida_pred_tramo5, main = "Predict - ARMA(3,3) - Tramo 5", type = "o", col = "darkgreen")
par(mfrow = c(1, 1))

# --- Últimos 5 datos reales del tramo 5 ---
serie_real_tramo5 <- tail(tramo5_train1_ts, 5)

# --- RMSE ---
rmse_final_AR33_tramo5 <- rmse(serie_real_tramo5, AR33_Revertida_pred_tramo5)
print(paste("RMSE =", rmse_final_AR33_tramo5))

# --- R² ---
SSE_AR33_tramo5 <- sum((serie_real_tramo5 - AR33_Revertida_pred_tramo5)^2)
SST_AR33_tramo5 <- sum((serie_real_tramo5 - mean(serie_real_tramo5))^2)
R_squared_AR33_tramo5 <- 1 - (SSE_AR33_tramo5 / SST_AR33_tramo5)
print(paste("R^2 =", R_squared_AR33_tramo5))

# --- Ajustar modelo a training y predecir sobre test ---
AR33_tramo5 <- arima(tramo5_train1_ts, order = c(3, 0, 3))
pAR33_test_tramo5 <- forecast(AR33_tramo5, h = length(tramo5_test1))

# --- Comparar predicción vs test ---
real_vs_prediccion_tramo5 <- data.frame(
  Fecha = index(tramo5_test1),
  Real = as.numeric(tramo5_test1),
  Predicho = as.numeric(pAR33_test_tramo5$mean)
)

# --- Gráfica de comparación usando ggplot2 ---
library(ggplot2)

ggplot(real_vs_prediccion_tramo5, aes(x = Fecha)) +
  geom_line(aes(y = Real, color = "Valores Reales"), size = 1.2) +
  geom_line(aes(y = Predicho, color = "Predicción ARMA(3,3)"), size = 1.2, linetype = "dashed") +
  labs(title = "Valores Reales vs Predicción ARMA(3,3) - Tramo 5",
       x = "Fecha", y = "Valor") +
  scale_color_manual(values = c("Valores Reales" = "black", "Predicción ARMA(3,3)" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# --- Visualización general Train vs Test ---
ggplot() +
  geom_line(aes(x = index(tramo5_train1), y = as.numeric(tramo5_train1), color = "Train")) +
  geom_line(aes(x = index(tramo5_test1), y = as.numeric(tramo5_test1), color = "Test")) +
  labs(title = "Train vs Test Tramo 5", x = "Fecha", y = "Valor") +
  scale_color_manual(name = "Serie", values = c("Train" = "blue", "Test" = "red")) +
  theme_minimal()

# --- Resaltando Test en la serie total ---
ggplot() +
  geom_line(aes(x = index(tramo5_train1), y = as.numeric(tramo5_train1), color = "Train"), size = 1) + 
  geom_line(aes(x = index(tramo5_test1), y = as.numeric(tramo5_test1), color = "Test"), size = 1, linetype = "dashed") +
  labs(title = "Serie completa con Test Destacado - Tramo 5", x = "Fecha", y = "Valor") +
  scale_color_manual(name = "Serie", values = c("Train" = "blue", "Test" = "red")) +
  theme_minimal() +
  scale_x_date(limits = c(as.Date("2021-09-20"), as.Date("2025-01-01")), date_labels = "%Y-%m-%d", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

# Ajustar el modelo ARIMA (3, 0, 3)
AR33_tramo5 <- arima(tramo5_train1_ts, order = c(3, 0, 3))

# Ver la salida del modelo
summary(AR33_tramo5)

# Predicciones para el conjunto Test
predicciones_test <- predict(AR33_tramo5, n.ahead = length(tramo5_test1))$pred

# Verifica que las predicciones no sean 0
print(predicciones_test)

# Crear un dataframe para comparar los valores reales vs predicciones
real_vs_prediccion_tramo5 <- data.frame(
  Fecha = index(tramo5_test1),
  Real = as.numeric(tramo5_test1),
  Predicho = predicciones_test
)

# Ver los primeros registros del dataframe para asegurarte que las fechas y predicciones estén alineadas
head(real_vs_prediccion_tramo5)

# Gráfico de los datos reales vs las predicciones
ggplot(real_vs_prediccion_tramo5, aes(x = Fecha)) +
  geom_line(aes(y = Real, color = "Real"), size = 1) +
  geom_line(aes(y = Predicho, color = "Predicción"), size = 1, linetype = "dashed") +
  labs(title = "Comparación de Valores Reales vs Predicciones ARIMA - Tramo 5", 
       x = "Fecha", y = "Valor") +
  scale_color_manual(values = c("Real" = "blue", "Predicción" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Calcular RMSE entre los valores reales y las predicciones
rmse_final <- sqrt(mean((real_vs_prediccion_tramo5$Real - real_vs_prediccion_tramo5$Predicho)^2))
print(paste("RMSE =", rmse_final))

# Calcular R²
SSE <- sum((real_vs_prediccion_tramo5$Real - real_vs_prediccion_tramo5$Predicho)^2)
SST <- sum((real_vs_prediccion_tramo5$Real - mean(real_vs_prediccion_tramo5$Real))^2)
R_squared <- 1 - (SSE / SST)
print(paste("R² =", R_squared))

# Crear gráfico con ggplot2
ggplot() +
  geom_line(aes(x = index(tramo5_test1), y = as.numeric(tramo5_test1), color = "Test")) +
  geom_line(aes(x = index(tramo5_test1), y = as.numeric(tramo5_train1)[length(tramo5_train1) - length(tramo5_test1) + 1:length(tramo5_test1)], color = "Train")) +
  labs(title = "Train y Test comparados Tramo 5", x = "Fecha", y = "Valor") +
  scale_color_manual(name = "Serie", values = c("Train" = "Black", "Test" = "red")) +
  theme_minimal()

ggplot() +
  geom_line(aes(x = index(tramo5_test1), y = as.numeric(tramo5_test1), color = "Test")) +
  geom_line(aes(x = index(tramo5_test1), y = as.numeric(tramo5_train1)[length(tramo5_train1) - length(tramo5_test1) + 1:length(tramo5_test1)], color = "Train")) +
  labs(title = "Train y Test comparados Tramo 5", x = "Fecha", y = "Valor") +
  scale_x_date(labels = scales::date_format("%Y-%m-%d"), breaks = "1 months") +
  scale_color_manual(name = "Serie", values = c("Train" = "bLack", "Test" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




