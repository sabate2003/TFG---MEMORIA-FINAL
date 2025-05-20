
################## Ejemplo de aplicación del BDS

library(tseries) # adf.test, kpss.test, bds.test, get.hist.quote, portfolio.optim, surrogate, arma, garch
library(forecast) #Autoarima, Acf, pacf
library(ggplot2)
library(strucchange)
library(splus2R) #para encontrar el máximo relativo  
library(readxl)

# Aquí tengo las funciones que voy a utilizar y las cargo con la siguiente función para poder utilizarlas
source("bds-recursive-inference-functions_v3.R")

# 1) Pruebo la capacidad del BDS para contrastar IID

# 2) utilizo el BDS recursivo para detectar cambio estructural en el proceso generador
     # aquí tengo varios estimadores del BDS recursivo 
     # que varían tengo unos con bucle otros con map
     # el de MAp ya los he paralelizado y es más rápido que el de map de purrr 
     # pero sólo cuando la serie es muy larga,
     # con series cortas sigue siendo más rápido map de purrr
     # el de bucle original es el más lento en series largas
     # por eso me queda paralelizarlo a ver si me quedo con el bucle o con map

# Después 3) utilizo la salida del BDS recursivo para intentar averiguar cuándo se produce cambio estructural
     # para ello utilizo el test de cambio estructural
     # despues de hacer muchas pruebas me quedo con dos opciones ; OLS-MOSUM y ME


######################################

############################Selección de y###############

# y serie de prueba
plot(y0,type='l')
#y0<-y0[1:length(y0)-1]
ndiff <- ndiffs(y0)

y<-diff(y0,lag=1,differences = ndiff)
plot(y,type='l')



#################################################################################
##################################################################################
###################################################################################
# BDS RECURSIVO ----

# N0= es el número inicial de muestras que utiliza para hacer el primer bds, después va añadiendo
# más observaciones (lapso) y va calculando el estadístico BDS
# nstd es el número de veces que queremos utilizar la desviación típica para el radio de la Bola
# lapso es cada cuantas observaciones se calcula el estadístico BDS
# m es el número de dimensiones de inserción a emplear, deberían ser mayor que el número de
# grados de libertad (dimensiones) del sistema original
# Como una primera aproximación conviene poner un lapso grande, y en caso de detectar
# cambio estructural, reducirlo para ser más fino a la hora de pronosticar cuando se ha realizado el cambio


# aquí llamo a las funciones que estiman el BDS recursivo ------
# me quedaría con la v2 o la v3 que utilizan en lugar de bubles programación funcional

m=6
N0=20
nstd=0.5
lapso=1


# con furrr (que paraleliza purrr) sólo para series muy largas
# bdsout<-bds_recurV3(y,nstd = 0.5, N0=100,m=6, lapso=100, tracebar = TRUE)) # BDS recursivo ))

# con purrr (sustituyo el for por purrr)
bdsout<-bds_recurV2(y,nstd = 0.5, N0=20,m=6, lapso=1, tracebar = TRUE) # BDS recursivo ))


# aquí analizo los resultados para detectar cambio estructural con structural change----


#plot(bdsout[,3]~bdsout[,1],typ="l",xlab = "n", ylab = "BDS values")
#plot(bdsout[,4]~bdsout[,1],typ="l",xlab = "n", ylab = "BDS values")
#plot(bdsout[,5]~bdsout[,1],typ="l",xlab = "n", ylab = "BDS values")

# El test lo voy a hacer siempre desde m=3 hasta Maxm
Maxm=5

# Defino la media habría que definir para qué rangos de m (aquí utilizo todos menos el primero) se quiere detectar cambio estructural
bdsout$bdsM <- rowMeans(scale(bdsout[,c(4:(min(Maxm,m+1)))]))


g_bds <- bdsout %>% 
  ggplot(aes(x=indice))+
#  geom_line(aes(y=scale(m2)), col=1)+
  geom_line(aes(y=scale(m3)), col=3)+
  geom_line(aes(y=scale(m4)), col=4)+
  geom_line(aes(y=scale(m5)), col=5)+
#  geom_line(aes(y=scale(m6)), col=6)+
  geom_line(aes(y=bdsM),col=1, lwd=1.05)+
  ylab("normalized bds test")

print(g_bds)

# para detectar cambio estructural utilizo la función de structural change sobre la media de los valores BDS y un modelo simple 

modelo<-bdsM ~ indice
modelo.lm<-lm(modelo, data=bdsout, na.action=NULL)  #ojo, na.action=NULL para las series
summary(modelo.lm)


bdsout <- cbind.data.frame(bdsout,predict.lm(modelo.lm,interval = "confidence",level=0.95))


g_bds2 <- bdsout %>% 
  ggplot(aes(x=indice))+
  geom_line(aes(y=bdsM),col=1, lwd=1.05)+
  geom_smooth(aes(y=bdsM), method="lm", se=TRUE,col="red")


print(g_bds2)


###################################ESTE ES EL QUE FUNCIONA############################################
# OLS-MOSUM (me parece que SI funciona) ESTO HABRIA QUE VER SI SE PUEDE PONER COMO FUNCION----
############################################################
h_0=0.05
testestbr<-efp(modelo, type="OLS-MOSUM", data = bdsout, h=h_0) # Este toma los residuos haciendo una Media Movil
bandas <- boundary(testestbr, type="level", level=0.05)
T_inicio <- start(bandas)[2]
T_final <- end(bandas)[2]

plot(bdsout[(T_inicio):T_final,1], testestbr$process,
     ylim=c(min(c(-bandas,min(testestbr$process))),max(c(bandas,max(testestbr$process)))),
     xlim=c(bdsout[T_inicio,1],bdsout[T_final,1]),
     type="l", xlab = "tiempo", ylab = "test")
abline(h=0, col=1,lty=3)
lines(bdsout[(T_inicio):T_final, 1], bandas, col=2, type = "l", lty=2)
lines(bdsout[(T_inicio):T_final, 1], -bandas, col=2, type = "l", lty=2)


par(mfrow=c(3,1))
plot(bdsout[(T_inicio):T_final,1], testestbr$process,
     ylim=c(min(c(-bandas,min(testestbr$process))),max(c(bandas,max(testestbr$process)))),
     xlim=c(bdsout[T_inicio,1],bdsout[T_final,1]),
              type="l", xlab = "tiempo", ylab = "test")
abline(h=0, col=1,lty=3)
lines(bdsout[(T_inicio):T_final, 1], bandas, col=2, type = "l", lty=2)
lines(bdsout[(T_inicio):T_final, 1], -bandas, col=2, type = "l", lty=2)

plot(y,type='l',
     xlim=c(bdsout[T_inicio,1],bdsout[T_final,1]))


plot(y0,type='l',
     xlim=c(bdsout[T_inicio,1],bdsout[T_final,1]))


par(mfrow=c(1,1))





# plot(testestbr)
sctest(testestbr)

rowmax<-abs(testestbr$process)
plot(bdsout[T_inicio:T_final,1],rowmax,
     ylim=c(0,max(c(bandas,max(abs(testestbr$process))))),
     xlim=c(bdsout[T_inicio,1],bdsout[T_final,1]),
     type="l", xlab=NA) # Parece que dibuja el máximo de las dos columnas
abline(h=0, col=1,lty=3)
lines(bdsout[T_inicio:T_final,1], bandas, col=2, type = "l", lty=2)

maximos<-peaks(rowmax, span=numRows(bdsout)*0.10, strict=TRUE, endbehavior=0)
table(maximos)
bdsout[T_inicio:T_final,1][maximos]
abline(v=c(bdsout[T_inicio:T_final,1][maximos]), col=2,lty=3)



par(mfrow=c(3,1))
rowmax<-abs(testestbr$process)
plot(bdsout[T_inicio:T_final,1],rowmax,
     ylim=c(0,max(c(bandas,max(abs(testestbr$process))))),
     xlim=c(0,length(y0)),
     type="l", xlab=NA) # Parece que dibuja el máximo de las dos columnas
abline(h=0, col=1,lty=3)
lines(bdsout[T_inicio:T_final,1], bandas, col=2, type = "l", lty=2)

maximos<-peaks(rowmax, span=numRows(bdsout)*0.10, strict=TRUE, endbehavior=0)
table(maximos)
bdsout[T_inicio:T_final,1][maximos]
abline(v=c(bdsout[T_inicio:T_final,1][maximos]), col=2,lty=3)

plot(y,type='l',
     xlim=c(0,length(y0)))
abline(v=c(bdsout[T_inicio:T_final,1][maximos]), col=2,lty=3)


plot(y0,type='l',
     xlim=c(0,length(y0)))
abline(v=c(bdsout[T_inicio:T_final,1][maximos]), col=2,lty=3)

par(mfrow=c(1,1))







# Para añadir puntos de cortes en el gráfico

g_bds <- g_bds+
  #geom_vline(xintercept = 400, col="black",lty=3 )+
  #geom_vline(xintercept = 800, col="black",lty=3 )+
  #geom_vline(xintercept = 1200, col="black",lty=3 )+
  geom_vline(xintercept =c(bdsout[T_inicio:T_final,1][maximos]), col="red",lty=3 )
print(g_bds)


# ggsave(bds_out$g_bds, filename = "gbdsserietest.jpg",  width =15,  height = 8, units = c("cm"), dpi=600)

####################################################
# Queda pendiente simular una gran cantidad de series temporales para ver si se detectan los cambios en los parámetros
# Este sería el trabajo qeu tendrías que realizar tú con las series de tu TFG
##############################################################################

