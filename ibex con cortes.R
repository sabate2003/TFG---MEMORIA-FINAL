library(tseries) # adf.test, kpss.test, bds.test, get.hist.quote, portfolio.optim, surrogate, arma, garch
library(forecast) #Autoarima, Acf, pacf
library(ggplot2)
library(strucchange) #Sirve para detectar cambios estructurales en la serie.
library(splus2R) #para encontrar el máximo relativo  
library(readxl)

source("BDS-recursive(v3) y str change IBEX-35.R")

# Instalar y cargar el paquete readxl

library(readxl)

# Cargar los datos desde la hoja "IBEX 35"
datos <- read_excel("BASE DE DATOS ! .xlsx", sheet = "IBEX 35")

# Ver los nombres de las columnas para asegurarnos de los nombres correctos
colnames(datos)

# Convertir la columna de fechas a formato Date (si no está en ese formato)
datos$`Exchange Date` <- as.Date(datos$`Exchange Date`)

# Seleccionar la serie de precios (ajusta el nombre de la columna si es diferente)
y0 <- as.numeric(datos$Close)  # Asegúrate de que es numérico

# Graficar la serie original con fechas
plot(datos$`Exchange Date`, y0, type='l', main="Serie Original IBEX 35", 
     ylab="Precio de Cierre", xlab="Fecha")

# Determinar el número de diferenciaciones necesarias
ndiff <- ndiffs(y0)

# Diferenciar la serie
y <- diff(y0, lag=1, differences=ndiff)

# Ajustar el vector de fechas (pierde `ndiff` observaciones)
fechas <- datos$`Exchange Date`[-(1:ndiff)]

# Graficar la serie diferenciada con fechas en el eje X
plot(fechas, y, type='l', main="Serie Diferenciada IBEX 35", 
     ylab="Diferencias", xlab="Fecha")

m = 6      # Dimensión de inserción
N0 = 20    # Tamaño inicial de la muestra para el test BDS
nstd = 0.5 # Multiplicador de la desviación estándar para el radio de la bola
lapso = 1  # Cada cuántas observaciones se calcula BDS

bdsout <- bds_recurV3(y, nstd=0.5, N0=20, m=6, lapso=1, tracebar=TRUE)

Maxm = 5
bdsout$bdsM <- rowMeans(scale(bdsout[, c(4:(min(Maxm, m+1)))]))


g_bds <- bdsout %>% 
  ggplot(aes(x=indice)) +
  geom_line(aes(y=scale(m3)), col=3) +
  geom_line(aes(y=scale(m4)), col=4) +
  geom_line(aes(y=scale(m5)), col=5) +
  geom_line(aes(y=bdsM), col=1, lwd=1.05) +
  ylab("normalized bds test")

print(g_bds)


modelo <- bdsM ~ indice
modelo.lm <- lm(modelo, data=bdsout, na.action=NULL)
summary(modelo.lm)


bdsout <- cbind.data.frame(bdsout, predict.lm(modelo.lm, interval="confidence", level=0.95))


#####

h_0 = 0.05
testestbr <- efp(modelo, type="OLS-MOSUM", data=bdsout, h=h_0)
bandas <- boundary(testestbr, type="level", level=0.05)
T_inicio <- start(bandas)[2]
T_final <- end(bandas)[2]

plot(bdsout[T_inicio:T_final,1], testestbr$process, type="l", xlab="tiempo", ylab="test")
abline(h=0, col=1, lty=3)
lines(bdsout[T_inicio:T_final, 1], bandas, col=2, type="l", lty=2)
lines(bdsout[T_inicio:T_final, 1], -bandas, col=2, type="l", lty=2)


rowmax <- abs(testestbr$process)
maximos <- peaks(rowmax, span=nrow(bdsout)*0.10, strict=TRUE, endbehavior=0)
bdsout[T_inicio:T_final,1][maximos]

g_bds <- g_bds +
  geom_vline(xintercept=c(bdsout[T_inicio:T_final,1][maximos]), col="red", lty=3)
print(g_bds)

# Obtener las fechas correspondientes a los valores detectados
fechas_detectadas <- fechas[bdsout[T_inicio:T_final, 1][maximos]]

# Mostrar las fechas detectadas
print(fechas_detectadas)

plot(fechas, y, type='l', main="Serie Diferenciada IBEX 35 con Cambios Detectados", 
     ylab="Diferencias", xlab="Fecha")

# Agregar líneas verticales en las fechas detectadas
abline(v=fechas_detectadas, col="red", lty=3)

fechas_detectadas_original <- datos$`Exchange Date`[bdsout[T_inicio:T_final, 1][maximos]]

plot(datos$`Exchange Date`, y0, type='l', main="Serie Original IBEX 35 con Cambios Detectados", 
     ylab="Precio de Cierre", xlab="Fecha")

# Agregar líneas verticales en las fechas detectadas
abline(v=fechas_detectadas_original, col="red", lty=3)







###ACCIONA-------------

# Cargar los datos desde la hoja "Acciona" del archivo Excel
datos_acciona <- read_excel("BASE DE DATOS ! .xlsx", sheet = "Acciona ")

# Ver los nombres de las columnas para asegurarnos de los nombres correctos
colnames(datos_acciona)

# Convertir la columna de fechas a formato Date (si no está en ese formato)
datos_acciona$`Exchange Date` <- as.Date(datos_acciona$`Exchange Date`)

# Seleccionar la serie de precios (ajusta el nombre de la columna si es diferente)
y0_acciona <- as.numeric(datos_acciona$Close)  # Asegúrate de que es numérico

# Graficar la serie original con fechas
plot(datos_acciona$`Exchange Date`, y0_acciona, type='l', main="Serie Original Acciona", 
     ylab="Precio de Cierre", xlab="Fecha")

# Determinar el número de diferenciaciones necesarias
ndiff_acciona <- ndiffs(y0_acciona)

# Diferenciar la serie
y_acciona <- diff(y0_acciona, lag=1, differences=ndiff_acciona)

# Ajustar el vector de fechas (pierde `ndiff` observaciones)
fechas_acciona <- datos_acciona$`Exchange Date`[-(1:ndiff_acciona)]

# Graficar la serie diferenciada con fechas en el eje X
plot(fechas_acciona, y_acciona, type='l', main="Serie Diferenciada Acciona", 
     ylab="Diferencias", xlab="Fecha")

# Parámetros para el test BDS
m = 6      # Dimensión de inserción
N0 = 20    # Tamaño inicial de la muestra para el test BDS
nstd = 0.5 # Multiplicador de la desviación estándar para el radio de la bola
lapso = 1  # Cada cuántas observaciones se calcula BDS

# Realizar el test BDS para detectar cambios estructurales
bdsout_acciona <- bds_recurV3(y_acciona, nstd=0.5, N0=20, m=6, lapso=1, tracebar=TRUE)

Maxm = 5
bdsout_acciona$bdsM <- rowMeans(scale(bdsout_acciona[, c(4:(min(Maxm, m+1)))]))

# Gráfico del test BDS
g_bds_acciona <- bdsout_acciona %>% 
  ggplot(aes(x=indice)) +
  geom_line(aes(y=scale(m3)), col=3) +
  geom_line(aes(y=scale(m4)), col=4) +
  geom_line(aes(y=scale(m5)), col=5) +
  geom_line(aes(y=bdsM), col=1, lwd=1.05) +
  ylab("normalized bds test")

print(g_bds_acciona)

# Modelo para predicciones
modelo_acciona <- bdsM ~ indice
modelo.lm_acciona <- lm(modelo_acciona, data=bdsout_acciona, na.action=NULL)
summary(modelo.lm_acciona)

bdsout_acciona <- cbind.data.frame(bdsout_acciona, predict.lm(modelo.lm_acciona, interval="confidence", level=0.95))

#####

# Realizar el test OLS-MOSUM
h_0 = 0.05
testestbr_acciona <- efp(modelo_acciona, type="OLS-MOSUM", data=bdsout_acciona, h=h_0)
bandas_acciona <- boundary(testestbr_acciona, type="level", level=0.05)
T_inicio_acciona <- start(bandas_acciona)[2]
T_final_acciona <- end(bandas_acciona)[2]

# Graficar el proceso
plot(bdsout_acciona[T_inicio_acciona:T_final_acciona, 1], testestbr_acciona$process, type="l", xlab="tiempo", ylab="test")
abline(h=0, col=1, lty=3)
lines(bdsout_acciona[T_inicio_acciona:T_final_acciona, 1], bandas_acciona, col=2, type="l", lty=2)
lines(bdsout_acciona[T_inicio_acciona:T_final_acciona, 1], -bandas_acciona, col=2, type="l", lty=2)

# Detectar los máximos en el proceso
rowmax_acciona <- abs(testestbr_acciona$process)
maximos_acciona <- peaks(rowmax_acciona, span=nrow(bdsout_acciona)*0.10, strict=TRUE, endbehavior=0)

# Fechas correspondientes a los máximos detectados
bdsout_acciona[T_inicio_acciona:T_final_acciona, 1][maximos_acciona]

# Graficar los máximos detectados en la serie BDS
g_bds_acciona <- g_bds_acciona +
  geom_vline(xintercept=c(bdsout_acciona[T_inicio_acciona:T_final_acciona, 1][maximos_acciona]), col="red", lty=3)

print(g_bds_acciona)

# Obtener las fechas correspondientes a los valores detectados
fechas_detectadas_acciona <- fechas_acciona[bdsout_acciona[T_inicio_acciona:T_final_acciona, 1][maximos_acciona]]

# Mostrar las fechas detectadas
print(fechas_detectadas_acciona)

# Graficar la serie diferenciada con las fechas detectadas
plot(fechas_acciona, y_acciona, type='l', main="Serie Diferenciada Acciona con Cambios Detectados", 
     ylab="Diferencias", xlab="Fecha")

# Agregar líneas verticales en las fechas detectadas
abline(v=fechas_detectadas_acciona, col="red", lty=3)

# Obtener las fechas detectadas en la serie original
fechas_detectadas_original_acciona <- datos_acciona$`Exchange Date`[bdsout_acciona[T_inicio_acciona:T_final_acciona, 1][maximos_acciona]]

# Graficar la serie original con las fechas detectadas
plot(datos_acciona$`Exchange Date`, y0_acciona, type='l', main="Serie Original Acciona con Cambios Detectados", 
     ylab="Precio de Cierre", xlab="Fecha")

# Agregar líneas verticales en las fechas detectadas
abline(v=fechas_detectadas_original_acciona, col="red", lty=3)


