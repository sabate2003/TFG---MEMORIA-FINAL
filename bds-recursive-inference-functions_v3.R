# En este script están programadas las funciones que vamos a utilizar en el script de ejemplos 
# Función que realiza el BDS test -tramos sistema generador estable-

set.seed(123)  # Para reproducibilidad
y <- rnorm(1000)  # Genera 1000 valores de una distribución normal
# --------------------------------

xserie=y[1:1000]          #Serie temporal a anlizar 
N0= 100                   # Tamaño inicial de la subserie 
m=6                       # dimensión para el test BDS 
nstd=0.50                 # número de veces la desviación estandar para definir el redio 
lapso=1                   # intervalo de crecimiento en cada iteración 
tracebar=TRUE             # nos muestra la barra de progreso 


# ---- BDS_RECURV3 ----
# con furrr que sustituye a purr pero paralelizando

bds_recurV3<-function(xserie,N0= 50, m=6, nstd=0.75 , lapso=5, tracebar=TRUE ){
  
  # nstd es el número de veces la desviación típica que se va a usar para definir el radio de la bola para buscar puntos próximos eps
  require(tseries)                           # para el bds.test
  require(zoo)                               # manejo de series temporales 
  #require(purrr)
  require(furrr)
  
#  require(future)
  # establece la forma de paralelizar         # Paralelización → ejecutar múltiples tareas al mismo tiempo en diferentes núcleos del procesador 
  no_cores <- availableCores() - 1            #número de núcleos disponibles menos 1 
#  plan(multicore, workers = no_cores)
  plan(multisession, workers = no_cores)      #detecta el nºnucleos disponibles de antes y los usa  
  
  #  require(xts)                                                      # Este código nos aseguraría que la seríe se convierta en un vector 
  #  if (is.data.frame(x) || is.xts(x)) {x<-as.vector(x[,1])}
  #  else x<-x
  
  NDimBds <- length(seq(from=N0,to=length(xserie),by = lapso))        # Esta variable representa el número de subconjuntos que se van a generar 
                                                                      # Desde N0 hasta el final de la serie con un intervalo = a valor de lapso 
  
  # creo la lista de subseries sobre la que tengo que aplicar el test BDS
  
  cat("\nPreparando datos... \n")
  
  subconjuntos <- future_map(seq(from=N0,to=length(xserie),by = lapso), ~ xserie[1:.x],.progress = tracebar)    #Almacena las subseries    
  
                                                                                                                    #En lugar de un for, utiliza future_map para aplicar 
                                                                                                                    # el test bds en paralelo sobre los subconjuntos de la serie
  # Ahora estimo el test bds sobre cada subconjunto
  
  cat("\nCalculadno BDS recursivo... \n")                 #Simplemente imprime los mensajes 
  
  bdsout <- future_map(subconjuntos, ~ bds.test(.x,m=m,eps=sd(.x)*nstd),.progress = tracebar)              #Almacena los resultados del test BDS 
  
  cat("\nCalculo BDS recursivo completado! ... preparando salida \n")
  
  
  bdsout <- as.data.frame(matrix(unlist(bdsout), nrow = length(bdsout), byrow = TRUE))                #Con este bloque lo que hacemos es crear el data frame final 
  bdsout <-bdsout[,c(1:((m-1)*2))]
  bdsout[] <- lapply(bdsout, as.numeric)
  names(bdsout)<-c(paste0("m",2:m), paste0("p-value",2:m))
  bdsout$indice <- seq(from=N0,to=length(xserie),by = lapso)
  bdsout$Fecha <- as.Date(index(xserie[seq(from=N0,to=length(xserie),by = lapso)]))
  bdsout <- bdsout[, c(((m-1)*2+1),((m-1)*2+2),c(1:((m-1)*2)))]
  
  
  
  
  plan(sequential)                   # Vuelve al modo normal (secuencial) para evitar interferencias en otras partes del código 
  # Mensaje final
  cat("\nProceso completado!\n")
  return(bdsout)
  #jpeg(file="bds_recur.jpeg")
  #plot(bdsout[,3]~bdsout[,1],typ="l",xlab = "n", ylab = "BDS values")
  #dev.off()
}

# ---- BDS_RECURV2 ----

#La principal diferencia respecto a la función anterior es que ejecuta todo en secuencia,
#por lo que esta es más lenta para un conjunto de datos grandes

# con purrr (sustituyo el for por purrr)
bds_recurV2<-function(xserie,N0= 50, m=6, nstd=0.75 , lapso=5, tracebar=TRUE ){
  
  # nstd es el número de veces la desviación típica que se va a usar para definir el radio de la bola para buscar puntos próximos eps
  require(tseries)
  require(zoo)
  
  require(purrr)
  
  #  require(xts)
  #  if (is.data.frame(x) || is.xts(x)) {x<-as.vector(x[,1])}
  #  else x<-x

  NDimBds <- length(seq(from=N0,to=length(xserie),by = lapso))

  # creo la lista de subseries sobre la que tengo que aplicar el test BDS

  cat("\nPreparando datos... \n")
  
    subconjuntos <- purrr::map(seq(from=N0,to=length(xserie),by = lapso), ~ xserie[1:.x],.progress = tracebar)           #purr::map() procesa cada subconjunto secuencialmente 
                                                                                                                         # No hay parelización 
  # Ahora estimo el test bds sobre cada subconjunto

  cat("\nCalculadno BDS recursivo... \n")
    
  bdsout <- purrr::map(subconjuntos, ~ bds.test(.x,m=m,eps=sd(.x)*nstd),.progress = tracebar)
  
  cat("\nCalculo BDS recursivo completado! ... preparando salida \n")
  
  
  bdsout <- as.data.frame(matrix(unlist(bdsout), nrow = length(bdsout), byrow = TRUE))
  bdsout <-bdsout[,c(1:((m-1)*2))]
  bdsout[] <- lapply(bdsout, as.numeric)
  names(bdsout)<-c(paste0("m",2:m), paste0("p-value",2:m))
  bdsout$indice=seq(from=N0,to=length(xserie),by = lapso)
  bdsout$Fecha=as.Date(index(xserie[seq(from=N0,to=length(xserie),by = lapso)]))
  bdsout <- bdsout[, c(((m-1)*2+1),((m-1)*2+2),c(1:((m-1)*2)))]
  
  
  # Mensaje final
  cat("\nProceso completado!\n")
  return(bdsout)
  #jpeg(file="bds_recur.jpeg")
  #plot(bdsout[,3]~bdsout[,1],typ="l",xlab = "n", ylab = "BDS values")
  #dev.off()
}


#---- BDS_RECURV1 ---- 

#Utiliza bucle for para ejecutarlo, mas lento que paralelización. 
#Utiliza una lista que luego se convierte en data.frame  

# con bucle, me permitirá posteriormente paralelizar
bds_recurV1<-function(x,N0= 50, m=6, nstd=0.75 , lapso=5, tracebar=TRUE, trace=FALSE, trac_step=5){
  
  # nstd es el número de veces la desviación típica que se va a usar para definir el radio de la bola para buscar puntos próximos eps
    require(tseries)
    require(zoo)
    require(progress)
    #  require(xts)
  #  if (is.data.frame(x) || is.xts(x)) {x<-as.vector(x[,1])}
  #  else x<-x

  # bdsout<-data.frame()
  bdsout<-list()                                                                 #esta variable almacena los resultados como una lista
  
  j=0                                                                            #contador de iteraciones 
  
  NDimBds <- length(seq(from=N0,to=length(x),by = lapso))                        # número de iteraciones 
  Ntrac=round(NDimBds/trac_step)                                                 # para controlar cuándo imprimir mensajes de progreso 
  # Crear la barra de progreso
    pb <- progress_bar$new(format = "  Procesando [:bar] :percent en :elapsed",
              total = NDimBds, clear = FALSE, width = 60) 
 
  
  #El siguiente bucle for recorre la serie en intervalos de tamaño "lapso", ejecutando el test bds en cada subserie creciente
  #Los resultados se guardas en el objeto bdsout que creamos anteriormente mientras que se muestran mensajes de progreso 
    
  for (i in seq(from=N0,to=length(x),by = lapso)){
    j=j+1
    if (trace==TRUE) {if (i%% (Ntrac*lapso)==0) cat(paste0(j, " de ", round((length(x)-N0)/lapso), " \n"))}
    xtemp<-x[1:i]
    xsigtemp<-sd(xtemp)*nstd
    bds<-bds.test(xtemp,m=m,eps=xsigtemp)
    bdsout[[j]]<-c(i, as.Date(index(x[i])),(bds$statistic[1:m-1]), (bds$p.value[1:m-1]))
    
    # Actualizar la barra de progreso
    if (tracebar==TRUE) pb$tick()
  }    
  bdsout <- as.data.frame(matrix(unlist(bdsout), nrow = length(bdsout), byrow = TRUE))
  names(bdsout)<-c("indice","fecha", paste0("m",2:m), paste0("p-value",2:m))
  
  # Mensaje final
  cat("\nProceso completado!\n")
  return(bdsout)
  #jpeg(file="bds_recur.jpeg")
  #plot(bdsout[,3]~bdsout[,1],typ="l",xlab = "n", ylab = "BDS values")
  #dev.off()
}


# ---- BDS_RECURV0---- 

#se utiliza un bucle for y rbind() - función para combinar las filas 
#Es la más básica y sin optimización 
# Es muy lento pues se agregan las filas una por una 

#Esta es la original
bds_recurV0<-function(x,N0= 50, m=6, nstd=0.75 , lapso=5){
  
  # nstd es el número de veces la desviación típica que se va a usar para definir el radio de la bola para buscar puntos próximos eps
  require(tseries)
  require(zoo)
  require(dplyr)
  #  require(xts)
  #  if (is.data.frame(x) || is.xts(x)) {x<-as.vector(x[,1])}
  #  else x<-x
  bdsout<-data.frame()                                                           # dataframe vacio para guardar resultados 
  j=0                                                                            # contador de iteraciones 

  NDimBds <- length(seq(from=N0,to=length(x),by = lapso))
  #Recorrela serie en intervalos "lapso", aumentando el tamaño de la subserie encada paso 
  #guarda los resultados con rbind() 
  for (i in seq(from=N0,to=length(x),by = lapso)){
    j=j+1
    if (i%% (5*lapso)==0) cat(paste0(j, " de ", round((length(x)-N0)/lapso), " \n"))
    xtemp<-x[1:i]
    xsigtemp<-sd(xtemp)*nstd
    bds<-bds.test(xtemp,m=m,eps=xsigtemp)
    bdsout<-rbind(bdsout, c(i, as.Date(index(x[i])),bds$statistic[1:m-1], bds$p.value[1:m-1]))
  }    
  names(bdsout)<-c("indice","fecha", paste0("m",2:m), paste0("p-value",2:m))    #renombramos las columnas del dataframe 
  return(bdsout)
  #jpeg(file="bds_recur.jpeg")
  #plot(bdsout[,3]~bdsout[,1],typ="l",xlab = "n", ylab = "BDS values")
  #dev.off()
}


# ---- RECUR_STCHG ---- 

#Extensión del test BDS
#Detecta cambios estructurales en una serie temporal 

bds_recur_stchg<-function(x,N0= 50, m=6, lapso=1, type="OLS-CUSUM", ... ){
  require(tseries)
  require(zoo)
  require(dplyr)
  require(ggplot2)
  require(strucchange)
  #  require(xts)
  #  if (is.data.frame(x) || is.xts(x)) {x<-as.vector(x[,1])}
  #  else x<-x
  bdsout<-data.frame()
  j=0
  #  xsigtemp<-sd(x)  # con esta calculariamos la desviación estandar de toda la serie, en lugar de hacerlo en cada subserie
  for (i in seq(from=N0,to=length(x),by = lapso)){
    j=j+1
    cat(paste0(j, " de ", round((length(x)-N0)/lapso), " \n"))
    xtemp<-x[1:i]
    xsigtemp<-sd(xtemp)*0.5
    bds<-bds.test(xtemp,m=m,eps=xsigtemp)
    bdsout<-rbind.data.frame(bdsout, c(i, as.Date(index(x[i])),bds$statistic[1:m-1], bds$p.value[1:m-1]))
  }    
  names(bdsout)<-c("indice","fecha", paste0("m",2:m), paste0("p-value",2:m))  

  bdsout$bdsM <- rowMeans(scale(bdsout[,c(4:(min(7,m+1)))]))        #promedio normamlizado del test BDS
                                                                    # scale() nos sirve para estandarizar los valores antes de hacer la media 
#  bdsout$DbdsM <- c(NA,diff(bdsout$bdsM))
#  bdsout <- bdsout[complete.cases(bdsout),]

# dibuja la curva del test BDS y la variable agragaa bdsM 
# es esto lo que nos ayuda a visualizar tendencias y posibles rupturas en la serie 
   g_bds <- bdsout %>% 
    ggplot(aes(x=indice))+
    #  geom_line(aes(y=scale(m2)), col=1)+
    geom_line(aes(y=scale(m3)), col=2)+
    geom_line(aes(y=scale(m4)), col=3)+
    geom_line(aes(y=scale(m5)), col=4)+
    geom_line(aes(y=scale(m6)), col=5)+
    geom_line(aes(y=bdsM),col=1)+
    ylab("normalized bds test")
   
    print(g_bds)
   
    
  #
     
#   g_bdsD <- ggplot(bdsout,aes(x=indice,y=DbdsM))+
#      geom_line()
    #
#    print(g_bdsD)
    
  bdsout_st<-bdsout[(N0):nrow(bdsout),]
  
  plot(bdsM~indice, data=bdsout_st, type="l")
  
  ## HO: AUSENCIA DE CAMBIO ESTRUCTURAL
  
  modelo<-bdsM ~ indice                                  # crea un modelo lineal 
  modelo.lm<-lm(modelo, data=bdsout_st, na.action=NULL)  #ojo, na.action=NULL para las series
  summary(modelo.lm)
  Chowtest<-Fstats(modelo.lm, from=1, to=(nrow(bdsout_st)), data=bdsout_st)
  #Chowtest$Fstats
  chowtest_bds <- sctest(Chowtest, type="supF") # valormáximo de los Estadísticos F (es el que se utiliza por defecto) - detectar cambios estructurales en bdsM
  testestbr<-efp(modelo,type = type, data = bdsout_st) # este utiliza ls residuos - detectar cambios en los residuos del modelo 
  plot(testestbr)
  OLS_CUSUM_bds<-sctest(testestbr)
  #a continuación identificamos cambios máximos y mínimos para identificar el momento exacto en que cambia la estructura de la serie 
  OLS_CUSUM_bds_min <- which.min(testestbr$process)+N0
  OLS_CUSUM_bds_max <- which.max(testestbr$process)+N0
  
  #Recogemos los resultados y los mostramos en un dataframe 
  salida.bds.strchg <- data.frame("chowtest_bds"=chowtest_bds$statistic,
                                  "chow_pvalue"=chowtest_bds$p.value,
                                  "CUSUM_bds"=OLS_CUSUM_bds$statistic,
                                  "CUSUM_pvalue"=OLS_CUSUM_bds$p.value,
                                  "CUSUM_bds_min"=OLS_CUSUM_bds_min,
                                  "CUSUM_bds_max"=OLS_CUSUM_bds_max)
  
  
  return(list("bdsout"=bdsout,"g_bds"=g_bds, #"g_bdsD"=g_bdsD,
              "testestbr"=testestbr,"salida.bds.strchg"=salida.bds.strchg))
  #jpeg(file="bds_recur.jpeg")
  #plot(bdsout[,3]~bdsout[,1],typ="l",xlab = "n", ylab = "BDS values")
  #dev.off()
}


