
library(tseries) 
library(forecast) 
library(ggplot2)
library(strucchange) 
library(splus2R)
library(readxl)
library(dplyr)


# Establecer tamaño global de los gráficos
options(repr.plot.width = 10, repr.plot.height = 6)
# Definir la lista de acciones
acciones <- c('IBEX 35', 'Acciona ', 'Acerinox ', 'ACS ', 'ArcelorMittal SA', 'Banco Sabadell',
              'Banco Santander', 'Bankinter', 'BBVA ', 'Caixabank ', ' Colonial', 'Enagas',
              'Endesa ', 'Ferrovial ', 'Fluidra', 'Grifols ', 'Iberdrola ', 'Inditex', 'Indra ',
              'Mapfre', 'Naturgy ', 'Redeia', 'Sacyr', 'Solaria ', 'Telefonica')

# Función para realizar el análisis para cada acción
analizar_accion <- function(accion) {
  # Cargar los datos desde la hoja correspondiente
  datos <- read_excel("BASE DE DATOS ! .xlsx", sheet = accion)
  
  # Ver los nombres de las columnas para asegurarnos de los nombres correctos
  colnames(datos)
  
  # Convertir la columna de fechas a formato Date (si no está en ese formato)
  datos$`Exchange Date` <- as.Date(datos$`Exchange Date`)
  
  # Seleccionar la serie de precios (ajusta el nombre de la columna si es diferente)
  y0 <- as.numeric(datos$Close)  # Asegúrate de que es numérico
  
  # Graficar la serie original con fechas y añadir el nombre de la acción al título
  plot(datos$`Exchange Date`, y0, type='l', main=paste("Serie Original -", accion), 
       ylab="Precio de Cierre", xlab="Fecha")
  
  # Determinar el número de diferenciaciones necesarias
  ndiff <- ndiffs(y0)
  
  # Diferenciar la serie
  y <- diff(y0, lag=1, differences=ndiff)
  
  # Ajustar el vector de fechas (pierde `ndiff` observaciones)
  fechas <- datos$`Exchange Date`[-(1:ndiff)]
  
  # Graficar la serie diferenciada con fechas en el eje X
  plot(fechas, y, type='l', main=paste("Serie Diferenciada -", accion), 
       ylab="Diferencias", xlab="Fecha")
  
  m = 6      # Dimensión de inserción
  N0 = 20    # Tamaño inicial de la muestra para el test BDS
  nstd = 0.5 # Multiplicador de la desviación estándar para el radio de la bola
  lapso = 1  # Cada cuántas observaciones se calcula BDS
  
  # Realizar el test BDS para detectar cambios estructurales
  bdsout <- bds_recurV3(y, nstd=0.5, N0=20, m=6, lapso=1, tracebar=TRUE)
  
  Maxm = 5
  bdsout$bdsM <- rowMeans(scale(bdsout[, c(4:(min(Maxm, m+1)))]))
  
  # Gráfico del test BDS con el nombre de la acción
  g_bds <- bdsout %>% 
    ggplot(aes(x=indice)) +
    geom_line(aes(y=scale(m3)), col=3) +
    geom_line(aes(y=scale(m4)), col=4) +
    geom_line(aes(y=scale(m5)), col=5) +
    geom_line(aes(y=bdsM), col=1, lwd=1.05) +
    ylab("normalized bds test") +
    ggtitle(paste("BDS Test -", accion))
  
  print(g_bds)
  
  # Modelo para predicciones
  modelo <- bdsM ~ indice
  modelo.lm <- lm(modelo, data=bdsout, na.action=NULL)
  summary(modelo.lm)
  
  bdsout <- cbind.data.frame(bdsout, predict.lm(modelo.lm, interval="confidence", level=0.95))
  
  #####
  
  # Realizar el test OLS-MOSUM
  h_0 = 0.05
  testestbr <- efp(modelo, type="OLS-MOSUM", data=bdsout, h=h_0)
  bandas <- boundary(testestbr, type="level", level=0.05)
  T_inicio <- start(bandas)[2]
  T_final <- end(bandas)[2]
  
  # Graficar el proceso con el nombre de la acción
  plot(bdsout[T_inicio:T_final, 1], testestbr$process, type="l", xlab="tiempo", ylab="test", main=paste("Proceso Test OLS-MOSUM -", accion))
  abline(h=0, col=1, lty=3)
  lines(bdsout[T_inicio:T_final, 1], bandas, col=2, type="l", lty=2)
  lines(bdsout[T_inicio:T_final, 1], -bandas, col=2, type="l", lty=2)
  
  # Detectar los máximos en el proceso
  rowmax <- abs(testestbr$process)
  maximos <- peaks(rowmax, span=nrow(bdsout)*0.10, strict=TRUE, endbehavior=0)
  
  # Fechas correspondientes a los máximos detectados
  bdsout[T_inicio:T_final, 1][maximos]
  
  # Gráfico de los máximos detectados en la serie BDS con el nombre de la acción
  g_bds <- g_bds +
    geom_vline(xintercept=c(bdsout[T_inicio:T_final, 1][maximos]), col="red", lty=3) +
    ggtitle(paste("BDS Test - Máximos Detectados -", accion))
  
  print(g_bds)
  
  # Obtener las fechas correspondientes a los valores detectados
  fechas_detectadas <- fechas[bdsout[T_inicio:T_final, 1][maximos]]
  
  # Mostrar las fechas detectadas
  cat("Fechas detectadas para la acción", accion, ":\n")
  print(fechas_detectadas)
  
  # Graficar la serie diferenciada con las fechas detectadas
  plot(fechas, y, type='l', main=paste("Serie Diferenciada -", accion, "con Cambios Detectados"), 
       ylab="Diferencias", xlab="Fecha")
  
  # Agregar líneas verticales en las fechas detectadas
  abline(v=fechas_detectadas, col="red", lty=3)
  
  # Obtener las fechas detectadas en la serie original
  fechas_detectadas_original <- datos$`Exchange Date`[bdsout[T_inicio:T_final, 1][maximos]]
  
  # Graficar la serie original con las fechas detectadas
  plot(datos$`Exchange Date`, y0, type='l', main=paste(accion), 
       ylab="Precio de Cierre", xlab="Fecha")
  
  # Agregar líneas verticales en las fechas detectadas
  abline(v=fechas_detectadas_original, col="red", lty=3)
}

analizar_accion('Banco Sabadell')

# Aplicar la función a todas las acciones
for (accion in acciones) {
  analizar_accion(accion)
}

# Crear un dataframe vacío para almacenar todas las series
datos_totales <- data.frame()

# Leer todas las series y unirlas en un solo dataframe
for (accion in acciones) {
  datos <- read_excel("BASE DE DATOS ! .xlsx", sheet = accion) %>%
    select(`Exchange Date`, Close) %>% 
    rename(Fecha = `Exchange Date`, Precio_Cierre = Close) %>%
    mutate(Accion = accion) # Añadir columna con el nombre de la acción
  
  datos$Fecha <- as.Date(datos$Fecha)  # Convertir fecha a formato Date
  datos_totales <- rbind(datos_totales, datos)  # Unir datos al dataframe total
}

# Graficar todas las acciones en un solo gráfico
ggplot(datos_totales, aes(x = Fecha, y = Precio_Cierre, color = Accion, group = Accion)) +
  geom_line(size = 1) +  
  labs(title = "Evolución del Precio de Cierre de Acciones",
       x = "Fecha",
       y = "Precio de Cierre",
       color = "Acción") +
  theme_minimal()





library(tseries) 
library(forecast) 
library(ggplot2)
library(strucchange) 
library(splus2R)
library(readxl)
library(dplyr)

# Definir la lista de acciones
acciones <- c('IBEX 35', 'Acciona ', 'Acerinox ', 'ACS ', 'ArcelorMittal SA', 'Banco Sabadell',
              'Banco Santander', 'Bankinter', 'BBVA ', 'Caixabank ', ' Colonial', 'Enagas',
              'Endesa ', 'Ferrovial ', 'Fluidra', 'Grifols ', 'Iberdrola ', 'Inditex', 'Indra ',
              'Mapfre', 'Naturgy ', 'Redeia', 'Sacyr', 'Solaria ', 'Telefonica')

# Crear un data.frame vacío para almacenar los datos BDS de todas las acciones
bds_data <- data.frame()

# Función para realizar el análisis de una acción y almacenar los datos
analizar_accion <- function(accion) {
  datos <- read_excel("BASE DE DATOS ! .xlsx", sheet = accion)
  
  # Convertir la columna de fechas a formato Date
  datos$`Exchange Date` <- as.Date(datos$`Exchange Date`)
  
  # Seleccionar la serie de precios
  y0 <- as.numeric(datos$Close)  
  
  # Diferenciar la serie
  ndiff <- ndiffs(y0)
  y <- diff(y0, lag=1, differences=ndiff)
  fechas <- datos$`Exchange Date`[-(1:ndiff)]
  
  # Test BDS
  bdsout <- bds_recurV3(y, nstd=0.5, N0=20, m=6, lapso=1, tracebar=TRUE)
  bdsout$bdsM <- rowMeans(scale(bdsout[, c(4:(min(5, 6+1)))]))  # Calcular media normalizada
  
  # Crear un data.frame con la acción y los valores BDS
  bds_temp <- data.frame(
    Fecha = fechas[1:nrow(bdsout)],  # Ajustar tamaño de fechas
    BDS = bdsout$bdsM,
    Accion = accion
  )
  
  # Agregar al data.frame global
  bds_data <<- rbind(bds_data, bds_temp)
}

# Aplicar la función a todas las acciones
for (accion in acciones) {
  analizar_accion(accion)
}

# Graficar todas las acciones en el mismo gráfico
ggplot(bds_data, aes(x = Fecha, y = BDS, color = Accion)) +
  geom_line() +
  labs(title = "Test BDS para Todas las Acciones", x = "Fecha", y = "BDS Normalizado") +
  theme_minimal() +
  coord_cartesian(ylim = c(-3, 3))


library(ggplot2)
library(RColorBrewer)

# Verificar que no haya valores faltantes en las series
bds_data <- na.omit(bds_data)

# Graficar todas las acciones con colores diferenciados y líneas más visibles
ggplot(bds_data, aes(x = Fecha, y = BDS, color = Accion)) +
  geom_line(size = 1.2) +  # Aumentar el grosor de las líneas para mayor visibilidad
  labs(title = "Test BDS para Todas las Acciones", x = "Fecha", y = "BDS Normalizado") +
  scale_color_brewer(palette = "Paired") +  # Usar la paleta "Paired" que tiene más colores
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(1, "cm"))  # Ajustar el tamaño de los cuadros en la leyenda


# Ver las acciones con valores extremos en las fechas más recientes
bds_data %>%
  filter(Fecha > as.Date("2024-01-01")) %>%  # Filtra las fechas recientes
  arrange(desc(abs(BDS))) %>%  # Ordena por valores extremos de BDS
  head(10)  # Muestra las 10 más extremas

acciones_anomalas <- bds_data %>%
  filter(Fecha > as.Date("2024-01-01")) %>%
  filter(abs(BDS) > 5) %>%  # Ajusta este umbral según lo observado
  pull(Accion) %>%
  unique()

ggplot(bds_data %>% filter(Accion %in% acciones_anomalas), aes(x = Fecha, y = BDS, color = Accion)) +
  geom_line() +
  labs(title = "Test BDS - Acciones con Anomalías", x = "Fecha", y = "BDS Normalizado") +
  theme_minimal() +
  coord_cartesian(ylim = c(-3, 3))

ggplot(bds_data %>% filter(Accion %in% c("Bankinter", "Caixabank", "IBEX 35")), 
       aes(x = Fecha, y = BDS, color = Accion)) +
  geom_line() +
  labs(title = "Test BDS - Acciones con Anomalías", x = "Fecha", y = "BDS Normalizado") +
  theme_minimal() +
  coord_cartesian(ylim = c(-3, 3))

bds_data %>% 
  filter(Accion %in% c("Bankinter", "Caixabank", "IBEX 35")) %>% 
  arrange(Accion, Fecha) %>% 
  tail(20)  # Muestra las últimas 20 observaciones
summary(bds_data$BDS)
boxplot(bds_data$BDS ~ bds_data$Accion, las=2, main="Distribución del Test BDS por Acción")

summary(bds_data$BDS)

boxplot(bds_data$BDS ~ bds_data$Accion, las=2, main="Distribución del Test BDS por Acción")

ggplot(bds_data %>% filter(!(Accion %in% c("IBEX 35", "Bankinter"))), 
       aes(x = Fecha, y = BDS, color = Accion)) +
  geom_line() +
  labs(title = "Test BDS - Acciones sin IBEX 35 y Bankinter", 
       x = "Fecha", 
       y = "BDS Normalizado") +
  theme_minimal()
ggplot(bds_data %>% filter(!(Accion %in% c("IBEX 35", "Bankinter", "Caixabank"))), 
       aes(x = Fecha, y = BDS, color = Accion)) +
  geom_line() +
  labs(title = "Test BDS - Acciones sin IBEX 35, Bankinter y Caixabank", 
       x = "Fecha", 
       y = "BDS Normalizado") +
  theme_minimal()
ggplot(bds_data %>% filter(!(Accion %in% c("IBEX 35", "Bankinter", "Caixabank"))), 
       aes(x = Fecha, y = BDS, color = Accion)) +
  geom_line() +
  labs(title = "Test BDS - Acciones sin IBEX 35, Bankinter y Caixabank", 
       x = "Fecha", 
       y = "BDS Normalizado") +
  theme_minimal() +
  coord_cartesian(ylim = c(-2.5, 2.5))

ggplot(bds_data, aes(x = Fecha, y = BDS, color = Accion)) +
  geom_line() +
  labs(title = "Test BDS - Todas las Acciones", 
       x = "Fecha", 
       y = "BDS Normalizado") +
  theme_minimal() +
  coord_cartesian(ylim = c(-3.5, 3.5))

# Filtrar datos por sector y graficar

# Energía
ggplot(bds_data %>% filter(Accion %in% c("Iberdrola", "Endesa", "Naturgy", "Enagas", "Redeia", "Solaria")), 
       aes(x = Fecha, y = BDS, color = Accion)) +
  geom_line() +
  labs(title = "Test BDS - Sector Energía", x = "Fecha", y = "BDS Normalizado") +
  theme_minimal()

# Finanzas
ggplot(bds_data %>% filter(Accion %in% c("BBVA", "Banco Santander", "Banco Sabadell", "Bankinter", "Caixabank", "Mapfre")), 
       aes(x = Fecha, y = BDS, color = Accion)) +
  geom_line() +
  labs(title = "Test BDS - Sector Finanzas", x = "Fecha", y = "BDS Normalizado") +
  theme_minimal() +
  coord_cartesian(ylim = c(-3, 3))

# Industria y Construcción
ggplot(bds_data %>% filter(Accion %in% c("Acciona ", "ACS ", "Ferrovial ", "Sacyr")), 
       aes(x = Fecha, y = BDS, color = Accion)) +
  geom_line() +
  labs(title = "Test BDS - Sector Industria y Construcción", x = "Fecha", y = "BDS Normalizado") +
  theme_minimal()

# Materiales y Metalurgia
ggplot(bds_data %>% filter(Accion %in% c("Acerinox ", "ArcelorMittal SA")), 
       aes(x = Fecha, y = BDS, color = Accion)) +
  geom_line() +
  labs(title = "Test BDS - Sector Materiales y Metalurgia", x = "Fecha", y = "BDS Normalizado") +
  theme_minimal()

# Tecnología y Telecomunicaciones
ggplot(bds_data %>% filter(Accion %in% c("Indra ", "Telefonica", "Inditex", "Fluidra")), 
       aes(x = Fecha, y = BDS, color = Accion)) +
  geom_line() +
  labs(title = "Test BDS - Sector Tecnología, Telecomunicaciones, Consumo y Retail", x = "Fecha", y = "BDS Normalizado") +
  theme_minimal()

# Salud e inmobiliario
ggplot(bds_data %>% filter(Accion %in% c("Grifols ", " Colonial")), 
       aes(x = Fecha, y = BDS, color = Accion)) +
  geom_line() +
  labs(title = "Test BDS - Sector Salud e Inmobiliario", x = "Fecha", y = "BDS Normalizado") +
  theme_minimal()

# Inmobiliario
ggplot(bds_data %>% filter(Accion %in% c(" Colonial")), 
       aes(x = Fecha, y = BDS, color = Accion)) +
  geom_line() +
  labs(title = "Test BDS - Sector Inmobiliario", x = "Fecha", y = "BDS Normalizado") +
  theme_minimal()

# Consumo y Retail
ggplot(bds_data %>% filter(Accion %in% c("Inditex", "Fluidra")), 
       aes(x = Fecha, y = BDS, color = Accion)) +
  geom_line() +
  labs(title = "Test BDS - Sector Consumo y Retail", x = "Fecha", y = "BDS Normalizado") +
  theme_minimal()



# Filtrar las fechas recientes y ordenar por los valores más extremos de BDS
bds_data %>%
  filter(Fecha > as.Date("2024-01-01")) %>%  # Filtra las fechas después del 1 de enero de 2024
  arrange(desc(abs(BDS))) %>%  # Ordena por el valor absoluto de BDS para identificar los extremos
  head(30)  # Muestra las 10 acciones más extremas








#-----graficar 
library(tseries) 
library(forecast) 
library(ggplot2)
library(strucchange) 
library(splus2R)
library(readxl)
library(dplyr)

# Definir la lista de acciones
acciones <- c('IBEX 35', 'Acciona ', 'Acerinox ', 'ACS ', 'ArcelorMittal SA', 'Banco Sabadell',
              'Banco Santander', 'Bankinter', 'BBVA ', 'Caixabank ', 'Colonial', 'Enagas',
              'Endesa ', 'Ferrovial ', 'Fluidra', 'Grifols ', 'Iberdrola ', 'Inditex', 'Indra ',
              'Mapfre', 'Naturgy ', 'Redeia', 'Sacyr', 'Solaria ', 'Telefonica')

# Función para realizar el análisis para cada acción
analizar_accion <- function(accion) {
  # Cargar los datos desde la hoja correspondiente
  datos <- read_excel("BASE DE DATOS ! .xlsx", sheet = accion)
  
  # Convertir la columna de fechas a formato Date
  datos$`Exchange Date` <- as.Date(datos$`Exchange Date`)
  
  # Seleccionar la serie de precios
  y0 <- as.numeric(datos$Close)
  
  # Determinar el número de diferenciaciones necesarias
  ndiff <- ndiffs(y0)
  
  # Diferenciar la serie
  y <- diff(y0, lag=1, differences=ndiff)
  
  # Ajustar el vector de fechas (pierde `ndiff` observaciones)
  fechas <- datos$`Exchange Date`[-(1:ndiff)]
  
  # Realizar el test BDS para detectar cambios estructurales
  bdsout <- bds_recurV3(y, nstd=0.5, N0=20, m=6, lapso=1, tracebar=TRUE)
  
  # Detectar los máximos en el proceso
  rowmax <- abs(bdsout$process)  
  maximos <- peaks(rowmax, span=nrow(bdsout)*0.10, strict=TRUE, endbehavior=0)
  
  # Fechas correspondientes a los máximos detectados
  fechas_detectadas <- fechas[bdsout[,"indice"][maximos]]
  
  # Mostrar las fechas detectadas
  cat("Fechas detectadas para la acción", accion, ":\n")
  print(fechas_detectadas)

  # Asegurar que cada gráfico se dibuje en una nueva ventana
  dev.new()  # Abre una nueva ventana gráfica
  par(mfrow=c(1,1))  # Restablecer el layout de gráficos

  # Graficar la serie diferenciada con cambios detectados
  plot(fechas, y, type='l', main=paste("Serie Diferenciada -", accion, "con Cambios Detectados"), 
       ylab="Diferencias", xlab="Fecha")
  abline(v=fechas_detectadas, col="red", lty=3)
  
  # Guardar el gráfico (opcional)
  ggsave(filename=paste0("Serie_Diferenciada_", accion, ".png"))

  # Graficar la serie original con cambios detectados
  dev.new()
  plot(datos$`Exchange Date`, y0, type='l', main=paste("Serie Original -", accion, "con Cambios Detectados"), 
       ylab="Precio de Cierre", xlab="Fecha")
  abline(v=fechas_detectadas, col="red", lty=3)

  # Guardar el gráfico (opcional)
  ggsave(filename=paste0("Serie_Original_", accion, ".png"))
}

# Aplicar la función a todas las acciones
for (accion in acciones) {
  analizar_accion(accion)
}

# Función para extraer solo las fechas detectadas
analizar_accion_fechas <- function(accion) {
  # Cargar los datos desde la hoja correspondiente
  datos <- read_excel("BASE DE DATOS ! .xlsx", sheet = accion)
  
  # Convertir la columna de fechas a formato Date
  datos$`Exchange Date` <- as.Date(datos$`Exchange Date`)
  
  # Seleccionar la serie de precios
  y0 <- as.numeric(datos$Close)
  
  # Determinar el número de diferenciaciones necesarias
  ndiff <- ndiffs(y0)
  
  # Diferenciar la serie
  y <- diff(y0, lag=1, differences=ndiff)
  
  # Ajustar el vector de fechas (pierde `ndiff` observaciones)
  fechas <- datos$`Exchange Date`[-(1:ndiff)]
  
  # Realizar el test BDS para detectar cambios estructurales
  bdsout <- bds_recurV3(y, nstd=0.5, N0=20, m=6, lapso=1, tracebar=TRUE)
  
  # Detectar los máximos en el proceso
  rowmax <- abs(bdsout$process)  
  maximos <- peaks(rowmax, span=nrow(bdsout)*0.10, strict=TRUE, endbehavior=0)
  
  # Fechas correspondientes a los máximos detectados
  fechas_detectadas <- fechas[bdsout[,"indice"][maximos]]
  
  # Mostrar las fechas detectadas
  cat("Fechas detectadas para la acción", accion, ":\n")
  print(fechas_detectadas)
  
  # Retornar las fechas detectadas
  return(fechas_detectadas)
}

# Aplicar la función a todas las acciones y guardar los resultados en una lista
resultados_fechas <- lapply(acciones, analizar_accion_fechas)
names(resultados_fechas) <- acciones

