##########################################################
#             AN?LISIS DE SERIES TEMPORALES              #
#	            MAESTR?A EN CIENCIA DE DATOS               #
#                FACULTAD DE INGENIER?A                  #
#                 UNIVERSIDAD AUSTRAL                    #
##########################################################

## Asignatura: AN?LISIS DE SERIES TEMPORALES
## Docente: Rodrigo Del Rosso
## Asistentes: Sebasti?n Calcagno - Fernando Mart?nez

################################
####### SETEO DE CARPETA #######
################################
rm(list = ls())

path = "C:/MCD_Source_R/TSeries/tseries" ## incorporar directorio de trabajo
setwd(path)

#####################
## CARGAR PAQUETES ##
#####################
listofpackages <- c( "tseries", "forecast", "astsa", "PerformanceAnalytics", 
                     "quantmod", "Quandl", "gridExtra", "dygraphs", "PASWR2", 
                     "pastecs", "psych", "lessR")
newPackages <- listofpackages[ !(listofpackages %in%
                                   installed.packages()[, "Package"])]
if(length(newPackages)) install.packages(newPackages)
for (paquete in listofpackages) {
  suppressMessages(library(paquete, character.only = TRUE))
}


# suppressPackageStartupMessages({
#   library(tseries)
#   library(forecast)
#   library(astsa)
#   library(PerformanceAnalytics)
#   library(quantmod)
#   library(Quandl)
#   library(ggplot2)
#   library(gridExtra)
#   library(dygraphs)
#   library(PASWR2)
#   library(pastecs)
#   library(psych)
#   library(lessR)
# })

#################
## TIME SERIES ##
#################

# Simular un WN con list(order = c(0, 0, 0))
set.seed(123)
white_noise <- arima.sim(model = list(order = c(0, 0, 0)), n = 100)

# ?arima.sim
#data.class(white_noise)

# Gr?fico del Ruido Blanco
autoplot(white_noise) + 
  ggtitle("Ruido Blanco", subtitle = "En Niveles") + 
  ylab("")

# Otras funciones para graficar
ts.plot(white_noise, main = "White Noise", ylab = "")

chart.TimeSeries(white_noise)

dygraph(white_noise)

# Simular un WN con mean = 100 y sd = 10
white_noise_2 <- arima.sim(model = list(order = c(0, 0, 0)), 
                           n = 100, 
                           mean = 100, 
                           sd = 10)

# Graficar white_noise_2
autoplot(white_noise_2) + 
  ggtitle("Ruido Blanco", subtitle = "Con Media") + 
  ylab("")

# RANDOM WALK
# Generar un modelo RW con un drift mediante arima.sim
rw_drift <- arima.sim(model = list(order = c(0, 1, 0)), 
                      n = 100, 
                      mean = 1)

# Graficar rw_drift
autoplot(rw_drift, main = "Random Walk with Drift", ylab = "")

set.seed(123)
rw_drift <- arima.sim(model = list(order = c(0, 1, 0)), n = 100)
a1 = autoplot(rw_drift, main = "Random Walk", ylab = "")
a2 = autoplot(diff(rw_drift), main = "Differenced Random Walk", ylab = "")
grid.arrange(a1,a2)

# Descargar info sobre S&P 500
getSymbols("^GSPC", 
           auto.assign = TRUE, 
           src = "yahoo")

head(GSPC)

lgspc <- log(Ad(GSPC)) ## Logaritmo del Nivel Ajustado del ?ndice Bursatil
dlgspc <- diff(lgspc)  ## Rendimiento logaritmico

## Sobre GSPC
chart.TimeSeries(Ad(GSPC))
chart.TimeSeries(log(Ad(GSPC)))
chart.TimeSeries(dlgspc)

y1 = autoplot(Ad(GSPC)) + 
  ggtitle("GSPC", subtitle = "En Niveles") + 
  ylab("Valor")

y2 = autoplot(diff(Ad(GSPC))) + 
  ggtitle("GSPC", subtitle = "Primera Diferencia") + 
  ylab("En %")

grid.arrange(y1,y2)

## FUNCI?N DE AUTOCORRELACI?N ## (FAC) o ACF

## Gr?fico de la Funci?n de Autocorrelaci?n
acf(rw_drift)

## Otra forma de calcular la FAC

ggAcf(rw_drift) + 
  ggtitle("Random Walk")

g1 = ggAcf(rw_drift)+ 
  ggtitle("Random Walk")

g2 = ggAcf(diff(rw_drift)) + 
  ggtitle("Differenced Random Walk")

g3 = ggAcf(diff(white_noise)) + 
  ggtitle("White Noise")

g4 = ggAcf(Ad(GSPC)) + 
  ggtitle("Adjusted Price of GSPC")

grid.arrange(g1,g2,g3,g4,ncol = 2)

ggAcf(dlgspc)

## PRECIO DEL BARRIL DE PETROLEO - QUANDL ## 

# Quandl
api_key = "..........." ## la deben obtener de la p?gina de Quandl
Quandl::Quandl.api_key(api_key)

## Descargar Precio del Barril de Petroleo
daily <- Quandl("OPEC/ORB",
                type = "ts")

data.class(daily) ## zoo

monthly <- Quandl("OPEC/ORB", 
                  collapse = "monthly", 
                  type = "ts")

yearly <- Quandl("OPEC/ORB", 
                 collapse = "annual", 
                 type = "ts")

b1 = autoplot(daily, main = "Daily Price Oil")
b2 = autoplot(monthly, main = "Monthly Price Oil")
b3 = autoplot(yearly, main = "Annual Price Oil")

grid.arrange(b1,b2,b3)

## TASA DE DESEMPLEO ## 

unemployment_rate = Quandl("FRED/UNRATE", 
                           type = "ts")

u1 = autoplot(unemployment_rate) + ggtitle("Tasa de Desempleo", subtitle = "En Niveles") + ylab("En %")
u2 = ggAcf(unemployment_rate) + ggtitle("Tasa de Desempleo", subtitle = "FAC")

grid.arrange(u1,u2)

### MORTALIDAD INFANTIL DE ARGENTINA

mortalidad <- read.csv2(file = "tasa-mortalidad-infantil.csv",
                        sep = ",", 
                        header = T)
View(mortalidad)

names(mortalidad)

data.class(mortalidad$mortalidad_infantil_argentina)

morta = ts(mortalidad$mortalidad_infantil_argentina, 
           frequency = 1, 
           start = c(1990,01))

data.class(morta)

data.class(mortalidad$mortalidad_infantil_argentina) ## CHARACTER

autoplot(morta)

## como arreglar que la variable sea un factor ##

time = as.Date(mortalidad$?..indice_tiempo)
data.class(time)

mortalidad_infantil_argentina <- as.numeric(as.character(mortalidad$mortalidad_infantil_argentina))

mortalidad_argentina = as.xts(mortalidad_infantil_argentina,
                              order.by = time)

data.class(mortalidad_argentina)

autoplot(mortalidad_argentina, 
         main = "Mortalidad Infantil Argentina") + 
  ylab("En %")

ggAcf(mortalidad_argentina)

## ESTACIONALIDAD ##

data("co2")
autoplot(co2, 
         main = "Consumo de Di?xido de Carbono", 
         ylab = "ppm")

ggAcf(co2)

data("AirPassengers")
autoplot(AirPassengers, 
         main = "Pasajeros A?reos", 
         ylab = "Cantidad de Personas")

# Diferenciar la Estacionalidad
nsdiffs(AirPassengers)  # Cantidad necesaria de diferenciaci?n estacional

ap_seasdiff <- diff(AirPassengers, 
                    lag = frequency(AirPassengers), 
                    differences = 1)  # Diferenciaci?n estacional

autoplot(ap_seasdiff, 
         main="Seasonally Differenced")  # no es estacionaria!!!

# Convertirla en estacionaria
ndiffs(ap_seasdiff)  # Cantidad necesaria de diferenciaci?n para hacerla estacionaria
stationaryTS <- diff(ap_seasdiff, differences= 1)
autoplot(stationaryTS, main="Differenced and Stationary")  # Parece ser estacionaria

# Descomposici?n de series
decompose(AirPassengers, type = "additive")
autoplot(decompose(AirPassengers, type = "additive"))
autoplot(decompose(AirPassengers, type = "multiplicative"))

ggAcf(AirPassengers, main = "Pasajeros Aéreos")
ggAcf(ap_seasdiff, main = "Seasonally Differenced")
ggAcf(stationaryTS, main = "Differenced and Stationary")

## ESTADISTICA DESCRIPTIVA ##

## Histograma
Histogram(white_noise, density = T)

hist(white_noise, 
     breaks = 30, 
     col = rainbow(10))

eda(white_noise)

## Descriptiva
stat.desc(white_noise)
stat.desc(white_noise,p = 0.95)

## Boxplot
boxplot(white_noise, 
        col = "yellow", 
        main = "White Noise")

fivenum(Ad(GSPC))

summary(Ad(GSPC))

mean(Ad(GSPC)) ## ??

mean(Ad(GSPC), na.rm = T)  ## Remover NA?s

sd(Ad(GSPC), na.rm = T)

## Percentiles
quantile(Ad(GSPC), na.rm = T)
quantile(Ad(GSPC), na.rm = T, probs = c(0.10,0.40,0.70))

## Eliminar Objetos ##

rm(list = ls())