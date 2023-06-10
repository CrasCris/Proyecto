library(tidyverse)
library(lubridate)
library(zoo)
library(forecast)
library(TSA)
library(tseries)
library(imputeTS)
library(ZIM)
library(fGarch)
library(rugarch)

view(Fecha.de.inicio.de.síntomas)
#Datos de covid en santander
datoss<-read.csv("C:/Users/HP/Downloads/Casos_positivos_de_COVID-19_en_SANTANDER.csv")
attach(datoss)
casos_inicio_sintomas<-table(datoss$Fecha.de.inicio.de.síntomas)#Este comando organiza por frecuencia y de la fecha mas nueva a la mas antigua
tabla_inicio<-transform(casos_inicio_sintomas)
tabla_inicio[,1]<-as.Date(tabla_inicio[,1])#OJO 281 Observaciones que son NA's
tabla_inicio <- tabla_inicio[-1, ]
head(tabla_inicio)
tail(tabla_inicio)
View(tabla_inicio)
fechas <- seq(as.Date("2020-03-12"), as.Date("2023-04-14"), by="day")
matriz_inicial = matrix(0, nrow = length(fechas), ncol = 2)
matriz_inicial[,1] <- fechas
sierra = c('maceta', 'guitarra')
lista = c('arbol', 'maceta', 'coche', 'guitarra')
lista %in% sierra
?replace
matriz_inicial[,2] = replace(matriz_inicial[,2], 
                             list = which(matriz_inicial[,1] %in% tabla_inicio[,1] == FALSE), 
                             values = NA)
matriz_inicial[,2] = replace(matriz_inicial[,2], 
                             list = which(matriz_inicial[,1] %in% tabla_inicio[,1] == TRUE), 
                             values = tabla_inicio[,2])

#ARIMA

#GRAFICO
plot(tabla_inicio$Freq~tabla_inicio$Var1,type="l",
     col="darkblue",xaxt="n",xlab="Tiempo(Dias)",ylab="Número de contagios")
axis(1,tabla_inicio[,1],format(tabla_inicio[,1],"%b %m"),
     cex.axis=0.7,tck=0)
#Interpolación usando spline para los datos faltantes
matriz_inicial[,2]<-na_interpolation(matriz_inicial[,2],option="spline")
serie_inicio<-ts(matriz_inicial[,2])
plot(serie_inicio, xlab="Tiempo(Dias)",ylab="Número de contagios")

periodogram(serie_inicio)
abline(v=0.0055, col="blue",lwd=2)
abline(v=0.014, col="purple",lwd=2)
s_1=1/0.0055
s_2=1/0.014

serie_inicio_p1<-ts(serie_inicio,frequency = 182)
stl(serie_inicio_p1,s.window = "periodic")
plot(stl(serie_inicio_p1,s.window = "periodic"))


serie_inicio_p2<-ts(serie_inicio,frequency = 71)
stl(serie_inicio_p2,s.window = "periodic")
plot(stl(serie_inicio_p2,s.window = "periodic"))

###GRAFICA DE LA SERIE DE TIEMPO CON FRECUENCIA DE 71
plot(serie_inicio_p2,xaxt="n",main="Serie de tiempo con frecuencia 71",
     col="darkblue",xlab="Tiempo cada 71 dias",ylab="Número de contagios")
axis(side = 1, serie_inicio_p2)
###
?nsdiffs
ndiffs(serie_inicio_p2)#1
nsdiffs(serie_inicio_p2)#valor para d=0
?pacf
?acf
pacf(serie_inicio_p2,main="Función de autocorrelación parcial")#Legalmente se salen 16, pero 4 son los mas significativos 
acf(serie_inicio_p2,main="Función de autocorrelación")#Todos son representativos, pero tomemos 5, ademas se van haciendo cada mas pequeños
#Graficas con mas contenido usando un paquete especial
ggAcf(serie_inicio_p2)
ggPacf(serie_inicio_p2)
###
#Analisis de residuales


pacf(diff(serie_resid,differences = 1))#5 se van haciendo mas pequeños
acf(diff(serie_resid,differences = 1))#5 no se ve claro que se vayan haciendo cada vez mas pequeños
plot(forecast(serie_resid))

#ESCOGER EL MEJOR MODELO (i,0,j)
matriz_4 = matrix(0, ncol = 4, nrow = 4)
for (i in 0:3) {
  for (j in 0:3) {
    matriz_4[i+1,j+1] = AIC(arima(serie_inicio,
                                  order = c(5, 0, 5),
                                  seasonal = list(order = c(i, 0, j),
                                                  period = 71)))}}

matriz_4
min(matriz_4[which(matriz_4 > 0)])#(0,0,0)
plot(forecast(arima(serie_inicio,order = c(5, 0, 5),seasonal = list(order = c(i, 0, j),period = 71))))


#ESCOGER EL MEJOR MODELO (i,1,j)
matriz_5 = matrix(0, ncol = 4, nrow = 4)
for (i in 0:3) {
  for (j in 0:3) {
    matriz_5[i+1,j+1] = AIC(arima(serie_inicio,
                                  order = c(5, 0, 5),
                                  seasonal = list(order = c(i, 1, j),
                                                  period = 71)))}}
matriz_5
min(matriz_5[which(matriz_5 > 0)])#(1,1,0)


#ESCOGER EL MEJOR MODELO (i,2,j)
matriz_6 = matrix(0, ncol = 4, nrow = 4)
for (i in 0:3) {
  for (j in 0:3) {
    matriz_6[i+1,j+1] = AIC(arima(serie_inicio,
                                  order = c(5, 0, 5),
                                  seasonal = list(order = c(i, 2, j),
                                                  period = 71)))}}
matriz_6
min(matriz_6[which(matriz_6 > 0)])#(2,2,0)



#Mejores modelos 
Mejor_modelo1<-Arima(serie_inicio, order = c(5,0,5),
                     seasonal = list(order=c(2,2,0),period=71))
Mejor_modelo2<-Arima(serie_inicio, order = c(5,0,5),
                     seasonal = list(order=c(1,1,0),period=71))
Mejor_modelo3<-Arima(serie_inicio, order = c(5,0,5),
                     seasonal = list(order=c(0,0,0),period=71))

#Graficas de los modelos
plot(forecast(Mejor_modelo1))
plot(forecast(Mejor_modelo2))

plot(forecast(Mejor_modelo3),xaxt="n",main = "ARIMA(5,0,5)x(0,0,0)",col="darkblue",xlab="Tiempo cada 71 dias",ylab="Número de contagios")
axis(side = 1,at=c(1,71,142,213,284,355,426,497,568,639,710,781,852,923,994,1065,1136,1207)
     ,labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))

plot(forecast(Mejor_modelo2),xaxt="n",main = "ARIMA(5,0,5)x(1,1,0)",col="darkblue",xlab="Tiempo cada 71 dias",ylab="Número de contagios")
axis(side = 1,at=c(1,71,142,213,284,355,426,497,568,639,710,781,852,923,994,1065,1136,1207)
     ,labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))

plot(forecast(Mejor_modelo1),xaxt="n",main = "ARIMA(5,0,5)x(2,2,0)",col="darkblue",xlab="Tiempo cada 71 dias",ylab="Número de contagios")
axis(side = 1,at=c(1,71,142,213,284,355,426,497,568,639,710,781,852,923,994,1065,1136,1207)
     ,labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))

summary(Mejor_modelo1)


?McLeod.Li.test
McLeod.Li.test(Mejor_modelo1, serie_inicio) #No heterocedastico 


serie_resid1 <- ts(Mejor_modelo1$residuals)
plot((serie_resid1),main="Analisis de residuales")





#GARCH


# “sGARCH”, “eGARCH”, GARCH(Para este parte hay que hacerlo distinto a los anteriores)
# armaOrder = c(i, j) i. j = 0, ...,5
?ugarchspec
?ugarchfit
#####################GARCH
model_garch <-ugarchspec(serie_inicio, variance.model = list(model = "fGARCH", 
                                                             submodel = "GARCH", garchOrder = c(0,5)), 
                         mean.model = list(armaOrder = c(5, 5), include.mean = TRUE), 
                         distribution.model = "norm")

# USANDO LOS MISMO COEFICIENTES DE ARMA(p,q) del anterior modelo
ajuste = ugarchfit(model_garch, serie_inicio,solver ='hybrid')
ajuste@fit$LLH # verosimilitud (el que tenga menos/mas valor de so verosimilitud)(el mas grande en valor absoluto)







##############sGARCH
model_sgarch <-ugarchspec(serie_inicio, variance.model = list(model = "sGARCH", garchOrder = c(0,0)), 
                          mean.model = list(armaOrder = c(5, 5), include.mean = TRUE), 
                          distribution.model = "norm")

# USANDO LOS MISMO COEFICIENTES DE ARMA(p,q) del anterior modelo
ajuste1 = ugarchfit(model_sgarch, serie_inicio,solver ='hybrid')
ajuste1@fit$LLH 



###########eGARCH
model_egarch <-ugarchspec(serie_inicio, variance.model = list(model = "sGARCH", garchOrder = c(1,0)), 
                          mean.model = list(armaOrder = c(5, 5), include.mean = TRUE), 
                          distribution.model = "norm")

# USANDO LOS MISMO COEFICIENTES DE ARMA(p,q) del anterior modelo
?ugarchfit
ajuste2 = ugarchfit(model_egarch, serie_inicio, solver = "hybrid")
ajuste2@fit$LLH
class(model_egarch)
?ugarchforecast
boot = ugarchforecast(ajuste2, data = serie_inicio, n.ahead = 50)
plot(boot)
class(ajuste2)
?ugarchboot
bootpred = ugarchboot(ajuste2, method = "Partial", n.ahead = 50, n.bootpred = 2000)
plot(bootpred@fsigma[1,], type = "l", ylim = c(0,500))
for (j in 2:2000) {
  lines(bootpred@fsigma[j,])
}
lines(cuantil25, lwd = 2, col = "red")
lines(cuantil75, lwd = 2, col = "red")

cuantil25 = NULL
for (k in 1:50) {
  cuantil25[k] = quantile(bootpred@fsigma[,k], 0.025)
}
cuantil75 = NULL
for (k in 1:50) {
  cuantil75[k] = quantile(bootpred@fsigma[,k], 0.975)
}


plot(bootpred@fseries[1,], type = "l")
bootpred