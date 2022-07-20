########################################
######### Script Trabajo Final #########
############ Matías Herrera ############
############ Nicolás Tolosa ############
########### Roberto Urquiza ############
########################################


######################################
# Paquetes y librerias ###############
######################################
install.packages("tidyverse")
install.packages("readxl")
install.packages("tseries")
install.packages("lubridate")
install.packages("astsa")
install.packages("forecast")
install.packages("foreign")
install.packages("timsac")
install.packages("vars")
install.packages("mFilter")
install.packages("dynlm")
install.packages("xts")
install.packages("psych")
install.packages("spdep")

library(tidyverse)
library(readxl)
library(tseries)
library(lubridate)
library(astsa)
library(forecast)
library(foreign)
library(timsac)
library(vars)
library(mFilter)
library(dynlm)
library(xts)
library(car)
library(spdep)
######################################
# Importe y creacion de datos ########
######################################

rm(list = ls())

setwd("~/Econometria Juan Cortez/Archivos R")


data <- read_excel("Base.xlsx", col_types = c("date","numeric","numeric","numeric", "numeric"))
ldata <- cbind(log(data[2:5]))
names(ldata) <- c("lipsa","limacec", "ltpm", "lipc")
datos <- data
df <- as.data.frame(datos[2:5])
df2 <- as.data.frame(log(datos[2:5]))

n <- length(data$fecha)

######################################
# Modelos ############################
######################################

# Regresion
modelo <- lm(ipsa ~ ipc + imacec + tpm, data = data)

# Regresion logaritmica
fit <- lm(lipsa ~ lipc + limacec + ltpm, data = ldata)

######################################
# Estadistica basica #################
######################################
summary(data[2:5]) # Grafico 1
summary(ldata) # Grafico 2

summary(modelo)
anova(modelo)
summary(fit)
anova(fit)

# Promedio, varianza y desviacion estandar IPSA
mean(data$ipsa)
var(data$ipsa)
sd(data$ipsa)

# Promedio, varianza y desviacion estandar IPC
mean(data$ipc)
var(data$ipc)
sd(data$ipc)

# Promedio, varianza y desviacion estandar IMACEC
mean(data$imacec)
var(data$imacec)
sd(data$imacec)

# Promedio, varianza y desviacion estandar TPM
mean(data$tpm)
var(data$tpm)
sd(data$tpm)


# Matriz de correlaciones
correlaciones <- cor(df)
correlacioneslog <- cor(df2)
psych::pairs.panels(df, pch=20, stars = TRUE, main = "Matriz de Correlaciones con histograma") # Grafico 3
psych::pairs.panels(df2, stars = TRUE, main = "Matriz Log de Correlaciones con Histograma") # Grafico 4



# Distribucion Boxplot del IPSA por ciclos de Junio a Junio
ipsabase <- ts(data$ipsa, start = c(2010,6), frequency = 12)
plot(ipsabase, col = "red",lwd = "2", main ="IPSA vs Tiempo", xlab = "Tiempo", ylab = "IPSA")
boxplot(ipsabase ~ cycle(ipsabase))


# Distribucion Boxplot del IPC por ciclos de Junio a Junio
ipcbase <- ts(data$ipc, start = c(2010,6), frequency = 12)
plot(ipcbase, col = "red",lwd = "2", main ="IPC vs Tiempo", xlab = "Tiempo", ylab = "IPC")
boxplot(ipcbase ~ cycle(ipcbase))


# Distribucion Boxplot del IMACEC por ciclos de Junio a Junio
imacecbase <- ts(data$imacec, start = c(2010,6), frequency = 12)
plot(imacecbase, col = "red",lwd = "2", main ="IIMACEC vs Tiempo", xlab = "Tiempo", ylab = "IMACEC")
boxplot(imacecbase ~ cycle(imacecbase))


# Distribucion Boxplot del TPM por ciclos de Junio a Junio
tpmbase <- ts(data$tpm, start = c(2010,6), frequency = 12)
plot(tpmbase, col = "red",lwd = "2", main ="TPM vs Tiempo", xlab = "Tiempo", ylab = "TPM")
boxplot(tpmbase ~ cycle(tpmbase))

######################################
# Graficos basicos de variables ######
######################################
# LogIPSA
ts_lipsa <- ts(ldata$lipsa, start = c(2010,6), frequency = 12)
decom_lipsa <- decompose(ts_lipsa, type = "multiplicative")
plot(decom_lipsa, col = "gold4", xlab = "Tiempo") # Grafico 5

# LogIPC
ts_lipc <- ts(ldata$lipc, start = c(2010,6), frequency = 12)
decom_lipc <- decompose(ts_lipc, type = "multiplicative")
plot(decom_lipc, col = "darkgreen", xlab = "Tiempo") # Grafico 6

# LogIMACEC
ts_limacec <- ts(ldata$limacec, start = c(2010,6), frequency = 12)
decom_limacec <- decompose(ts_limacec, type = "multiplicative")
plot(decom_limacec, col = "darkblue", xlab = "Tiempo") # Grafico 7

# LogTPM
ts_ltpm <- ts(ldata$ltpm, start = c(2010,6), frequency = 12)
decom_ltpm <- decompose(ts_ltpm, type = "multiplicative")
plot(decom_ltpm, col = "darkviolet", xlab = "Tiempo") # Grafico 8

#####################################################
# Diagrama de dispersion en niveles y logaritmica ###
#####################################################

par(mfrow = c(2,1))
# IPSA vs IPC
plot(ipsa ~ ipc, data = data, main = "Diagrama de dispersión y correlacion entre IPC vs IPSA", xlab = "IPC", ylab = "IPSA", col = "darkgreen") + abline(lm(ipsa ~ ipc, data = data), col ="red")

plot(lipsa ~ lipc, data = ldata, main = "Diagrama de dispersión y correlacion entre LogIPC vs LogIPSA", xlab = "LogIPC", ylab = "LogIPSA", col = "darkgreen") + abline(lm(lipsa ~ lipc, data = ldata), col ="red")



# IPSA vs IMACEC
plot(ipsa ~ imacec, data = data, main = "Diagrama de dispersión y correlacion entre IMACEC vs IPSA", xlab = "IMACEC", ylab = "IPSA", col = "darkblue") + abline(lm(ipsa ~ imacec, data = data), col ="red")

plot(lipsa ~ limacec, data = ldata, main = "Diagrama de dispersión y correlacion entre LogIMACEC vs LogIPSA", xlab = "LogIMACEC", ylab = "LogIPSA", col = "darkblue") + abline(lm(lipsa ~ limacec, data = ldata), col ="red")



# IPSA vs TPM
plot(ipsa ~ tpm, data = data, main = "Diagrama de dispersión y correlacion entre TPM vs IPSA", xlab = "TPM", ylab = "IPSA", col = "darkviolet") + abline(lm(ipsa ~ tpm, data = data), col ="red")

plot(lipsa ~ ltpm, data = ldata, main = "Diagrama de dispersión y correlacion entre LogTPM vs LogIPSA", xlab = "LogTPM", ylab = "LogIPSA", col = "darkviolet") + abline(lm(lipsa ~ ltpm, data = ldata), col ="red")

######################################
# Significancia global ###############
######################################
summary(modelo)$r.squared
summary(modelo)$adj.r.squared

# La significancia de acuerdo al r squared y el adj.r.squared nos indica un modelo que explica muy poco de la varianza de la variable dependiente, por tanto no es un buen modelo

summary(modelo)$fstatistic
k<-length(modelo$coefficients)
n <- length(data$fecha)
alpha<-0.05
qf(1-alpha,k-1,n-k)

# Estadistica stepwise
modelo_y <- lm(ipsa ~ 1,data)

# Regresion forward
modelo_forward <- step(modelo_y, scope = list(lower = modelo_y, upper = modelo), direction = "forward")
summary(modelo_forward)

# Regresion backward
modelo_backward <- step(modelo, scope = list(lower = modelo_y, upper = modelo), direction = "backward")
summary(modelo_forward)




###################################################
# Analisis de los residuos y normalidad ###########
###################################################
res <- modelo$residuals
m <- mean(res)
sd <- sqrt(var(res))

par(mfrow = c(2,1))

# Histograma de los residuos
hist(res, density = 10, breaks = 20, prob = T, col = "red", xlim = c(-1500, 1500), xlab = "Residuos", ylab = "Densidad", main = "Histograma de los residuos")
curve(dnorm(x, mean = m, sd = sd), col = "blue", add = T)

# QQNorm
qqnorm(modelo$residuals)
qqline(modelo$residuals, col = "red", lwd = 2)

# Test de Shapiro-Wilk
shapiro.test(res)

# Test de Jarque Bera
jarque.bera.test(res)

#########################################
# Autocorrelacion y Multicolinealidad ###
#########################################

# Test Durbin Watson
dwtest(modelo)

# Test Breusch-Godfrey
bgtest(modelo, order = 1)
bgtest(modelo, order = 2)
bgtest(modelo, order = 3)
bgtest(modelo, order = 4)


# Analisis del ACF
# Series observadas
ts_ipsa <- ts(data$ipsa, start = c(2010,6), frequency = 12)
ts_ipc <- ts(data$ipc, start = c(2010,6), frequency = 12)
ts_imacec <- ts(data$imacec, start = c(2010,6), frequency = 12)
ts_tpm <- ts(data$tpm, start = c(2010,6), frequency = 12)


# AFC observadas
par(mfrow = c(2,2))

acf(ts_ipsa, main = "ACF de IPSA", col = "gold4")
acf(ts_ipc, main = "ACF de IPC", col = "darkgreen")
acf(ts_imacec, main = "ACF de IMACEC", col = "darkblue")
acf(ts_tpm, main = "ACF de TPM", col = "darkviolet")


# Series desestacionalizadas
ts_diffipsa <- diff(ts_ipsa)
plot(ts_diffipsa, main = "IPSA desestacionalizado", col = "gold4")
ts_diffipc <- diff(ts_ipc)
plot(ts_diffipc, main = "IPC desestacionalizado", col = "darkgreen")
ts_diffimacec <- diff(ts_imacec)
plot(ts_diffimacec, main = "IMACEC desestacionalizado", col = "darkblue")
ts_difftpm <- diff(ts_tpm)
plot(ts_difftpm, main = "TPM desestacionalizado", col = "darkviolet")

# AFC desestacionalizado
acf(ts_diffipsa, main = "ACF de IPSA desestacionalizado", col = "gold4")
acf(ts_diffipc, main = "ACF de IPC desestacionalizado", col = "darkgreen")
acf(ts_diffimacec, main = "ACF de IMACEC desestacionalizado", col = "darkblue")
acf(ts_difftpm, main = "ACF de TPM desestacionalizado", col = "darkviolet")


# Analisis de multicolinealidad - VIF
car::vif(modelo)

######################################
# Heterocedasticidad #################
######################################

# test de White
r2 <- summary(modelo[r.squared]) 
Modelo_white <- bptest(modelo, ~I(ipc^2) + I(imacec^2) + I(tpm^2) + ipc*imacec*tpm, data= data) 
print(Modelo_white)

# test de Goldfeld-Quandt
gqtest(modelo)
gqtest(modelo, fraction = 48)

# test de Breusch Pagan
bptest(modelo)
######################################
n <- length(data$fecha)
r2_modelo <- 0.1759
r2_modelo*n
# test RESET RANSEY
resettest(modelo)
modelo2 <- lm(ipsa ~ imacec + ipc, data)
resettest(modelo2)



