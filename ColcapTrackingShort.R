library(matlib)
library(quantmod)
library(xts)
library(readr)
library(fitdistrplus)
library(Matrix)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(modopt.matlab)
library(fPortfolio)
library(xtable)

#Descargamos los datos de investing.com y los llamamos. Después de llamarlos, se convierten en series de tiempo sin considerar algunos ítems y cambiando el oreden para que las funcionen de "quantmod" funcionen exactamente igual que con un dato descargado desde yahoofinance.

setwd("C:/Users/victo/OneDrive/Escritorio/Académico/UNAL/Finanzas Matemáticas/Portafolio de Seguimiento a Índice/HistóricosCOLCAP")
#Grupo Argos
ARG <- read_csv("ARG.csv", locale = locale(decimal_mark = ",", grouping_mark = "."))
ARG[6:7] <- list(NULL)
ARG <- ARG[c(1,3,4,5,2)]
ARG$Fecha <- format(as.Date(ARG$Fecha, format="%d.%m.%Y"))
ARG <- xts(ARG[-1], as.POSIXct(ARG$Fecha))
#Bancolombia
BIC <- read_csv("BIC.csv", locale = locale(decimal_mark = ",", grouping_mark = "."))
BIC[6:7] <- list(NULL)
BIC <- BIC[c(1,3,4,5,2)]
BIC$Fecha <- format(as.Date(BIC$Fecha, format="%d.%m.%Y"))
BIC <- xts(BIC[-1], as.POSIXct(BIC$Fecha))
#Preferencial Bancolombia
BIC_p <- read_csv("BIC_p.csv", locale = locale(decimal_mark = ",", grouping_mark = ".")) 
BIC_p[6:7] <- list(NULL)
BIC_p <- BIC_p[c(1,3,4,5,2)]
BIC_p$Fecha <- format(as.Date(BIC_p$Fecha, format="%d.%m.%Y"))
BIC_p <- xts(BIC_p[-1], as.POSIXct(BIC_p$Fecha))
#Cementos ARGOS
CCB <- read_csv("CCB.csv", locale = locale(decimal_mark = ",", grouping_mark = "."))
CCB[6:7] <- list(NULL)
CCB <- CCB[c(1,3,4,5,2)]
CCB$Fecha <- format(as.Date(CCB$Fecha, format="%d.%m.%Y"))
CCB <- xts(CCB[-1], as.POSIXct(CCB$Fecha))
#Ecopetrol
ECO <- read_csv("ECO.csv", locale = locale(decimal_mark = ",", grouping_mark = "."))
ECO[6:7] <- list(NULL)
ECO <- ECO[c(1,3,4,5,2)]
ECO$Fecha <- format(as.Date(ECO$Fecha, format="%d.%m.%Y"))
ECO <- xts(ECO[-1], as.POSIXct(ECO$Fecha))
#Preferencial Grupo Aval
GAA_p <- read_csv("GAA_p.csv", locale = locale(decimal_mark = ",", grouping_mark = "."))
GAA_p[6:7] <- list(NULL)
GAA_p <- GAA_p[c(1,3,4,5,2)]
GAA_p$Fecha <- format(as.Date(GAA_p$Fecha, format="%d.%m.%Y"))
GAA_p <- xts(GAA_p[-1], as.POSIXct(GAA_p$Fecha))
#Interconexión Eléctrica
ISA <- read_csv("ISA.csv", locale = locale(decimal_mark = ",", grouping_mark = "."))
ISA[6:7] <- list(NULL)
ISA <- ISA[c(1,3,4,5,2)]
ISA$Fecha <- format(as.Date(ISA$Fecha, format="%d.%m.%Y"))
ISA <- xts(ISA[-1], as.POSIXct(ISA$Fecha))
#Grupo Nutresa
NCH <- read_csv("NCH.csv", locale = locale(decimal_mark = ",", grouping_mark = "."))
NCH[6:7] <- list(NULL)
NCH <- NCH[c(1,3,4,5,2)]
NCH$Fecha <- format(as.Date(NCH$Fecha, format = "%d.%m.%Y"), "%Y/%m/%d")
NCH <- xts(NCH[-1], as.POSIXct(NCH$Fecha))
#Grupo Sura
SIS <- read_csv("SIS.csv", locale = locale(decimal_mark = ",", grouping_mark = "."))
SIS[6:7] <- list(NULL)
SIS <- SIS[c(1,3,4,5,2)]
SIS$Fecha <- format(as.Date(SIS$Fecha, format="%d.%m.%Y"))
SIS <- xts(SIS[-1], as.POSIXct(SIS$Fecha))
#Grupo Sura Preferencial
SIS_p <- read_csv("SIS_p.csv", locale = locale(decimal_mark = ",", grouping_mark = "."))
SIS_p[6:7] <- list(NULL)
SIS_p <- SIS_p[c(1,3,4,5,2)]
SIS_p$Fecha <- format(as.Date(SIS_p$Fecha, format="%d.%m.%Y"))
SIS_p <- xts(SIS_p[-1], as.POSIXct(SIS_p$Fecha))
#Canasta COLCAP
Canasta <- read_delim("Canasta.csv",";", escape_double = FALSE, col_types = cols(FECHA = col_date(format = "%d.%m.%Y")), locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)
Canasta <- data.frame(Canasta[-1])

#RETORNOS
#El procedimiento realizado para poder obtener el mejor ajuste del portafolio escogido al ínidice fue el siguiente:
#1. Obtener los retornos logarítmicos diarios y mensuales
#2. Seguir el retorno del ínidice a partir de los retornos diarios
#3. Fijar los pesos y generar el comportamiento del portafolio con los retornos mensuales


#PRIMEROS 3 AÑOS
r1 <- periodReturn(ARG, period = "monthly", type = "log")
r1v <- periodReturn(ARG[1:731,], period = "daily", type = "log")
r1t <- periodReturn(ARG[1:731,], period = "monthly", type = "log")
r2 <- periodReturn(BIC, period = "monthly", type = "log")
r2v <- periodReturn(BIC[1:731,], period = "daily", type = "log")
r2t <- periodReturn(BIC[1:731,], period = "monthly", type = "log")
r3 <- periodReturn(BIC_p, period = "monthly", type = "log")
r3v <- periodReturn(BIC_p[1:731,], period = "daily", type = "log")
r3t <- periodReturn(BIC_p[1:731,], period = "monthly", type = "log")
r4 <- periodReturn(CCB, period = "monthly", type = "log")
r4v <- periodReturn(CCB[1:731,], period = "daily", type = "log")
r4t <- periodReturn(CCB[1:731,], period = "monthly", type = "log")
r5 <- periodReturn(ECO, period = "monthly", type = "log")
r5v <- periodReturn(ECO[1:731,], period = "daily", type = "log")
r5t <- periodReturn(ECO[1:731,], period = "monthly", type = "log")
r6 <- periodReturn(GAA_p, period = "monthly", type = "log")
r6v <- periodReturn(GAA_p[1:731,], period = "daily", type = "log")
r6t <- periodReturn(GAA_p[1:731,], period = "monthly", type = "log")
r7 <- periodReturn(ISA, period = "monthly", type = "log")
r7v <- periodReturn(ISA[1:731,], period = "daily", type = "log")
r7t <- periodReturn(ISA[1:731,], period = "monthly", type = "log")
r8 <- periodReturn(NCH, period = "monthly", type = "log")
r8v <- periodReturn(NCH[1:731,], period = "daily", type = "log")
r8t <- periodReturn(NCH[1:731,], period = "monthly", type = "log")
r9 <- periodReturn(SIS, period = "monthly", type = "log")
r9v <- periodReturn(SIS[1:731,], period = "daily", type = "log")
r9t <- periodReturn(SIS[1:731,], period = "monthly", type = "log")
r10 <- periodReturn(SIS_p, period = "monthly", type = "log")
r10v <- periodReturn(SIS_p[1:731,], period = "daily", type = "log")
r10t <- periodReturn(SIS_p[1:731,], period = "monthly", type = "log")
#Una vez encontrados los retornos logarítmicos 

retornos <- cbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10)
retornosv <- cbind(r1v, r2v, r3v, r4v, r5v, r6v, r7v, r8v, r9v, r10v)
retornost <- cbind(r1t, r2t, r3t, r4t, r5t, r6t, r7t, r8t, r9t, r10t)

colnames(retornos) <- c("ARG","BIC","BIC_p","CCB","ECO","GAA_p","ISA","NCH","SIS","SIS_p")
colnames(retornosv) <- c("ARG","BIC","BIC_p","CCB","ECO","GAA_p","ISA","NCH","SIS","SIS_p")

#Generamos el retorno que queremos para nuestro portafolio
#Descargamos y llamamos los datos históricos del COLCAP
COLC <- read_csv("COLC.csv", locale = locale(decimal_mark = ",", grouping_mark = "."))
COLC[6:7] <- list(NULL)
COLC <- COLC[c(1,3,4,5,2)]
COLC$Fecha <- format(as.Date(COLC$Fecha, format="%d.%m.%Y"),"%Y/%m/%d")
COLC <- xts(COLC[-1], as.POSIXct(COLC$Fecha))
#Al igual con las acciones anteriores formamos un vector de retornos logarítmicos
rcol <- periodReturn(COLC, period = "monthly", type = "log")
rcolv <- periodReturn(COLC[1:731,], period = "daily", type = "log")
rcolt <- periodReturn(COLC[1:731,], period = "monthly", type = "log")
#Ahora escogemos el mejor ajuste probabilistico de estos datos, de manera que se pueda obtener, con alguna distribución, un retorno esperado
fit.norm <- fitdist(as.numeric(as.character(rcolv$daily.returns)), "norm")
r_p <- fit.norm$estimate["mean"]

#Establecemos el problema de optimización, en este caso con restricción de operaciones en corto que no superen el 7.5% y no más del 39% en largo para un activo
pesoptim <- function(R){
  funds <- colnames(R)
  pfolio <- portfolio.spec(assets=funds)
  pfolio <- add.constraint(portfolio = pfolio, type = "full_investment")
  pfolio <- add.constraint(portfolio = pfolio, type = "box", min = -0.075, max=0.39)
  pfolio <- add.objective(portfolio = pfolio, type = "return", name = "mean", target = r_p)
  pfolio <- add.objective(portfolio = pfolio, type = "risk", name = "var")
  pfolio <- optimize.portfolio(R=R, portfolio = pfolio, optimize_method = "ROI", trace = TRUE)
  return(pfolio)
}

k=189
R = cbind(retornosv[,COMB[1,k]], retornosv[,COMB[2,k]], retornosv[,COMB[3,k]], retornosv[,COMB[4,k]], retornosv[,COMB[5,k]])
t0 <- returnValue(pesoptim(R))
p1 <- t0$weights[1]*retornost[,COMB[1,k]]
p2 <- t0$weights[2]*retornost[,COMB[2,k]]
p3 <- t0$weights[3]*retornost[,COMB[3,k]]
p4 <- t0$weights[4]*retornost[,COMB[4,k]]
p5 <- t0$weights[5]*retornost[,COMB[5,k]]

p0t <- p1+p2+p3+p4+p5
total <- cbind(rcolt, p0t)
plot.xts(total, ylim = c(-0.09,0.15), main = k)


#La estrategia de balanceo se realizó a partir del problema de optimización ajustado a una serie de tiempo posterior por trimestres


#REBALANCEO 1
r1 <- periodReturn(ARG, period = "monthly", type = "log")
r1v <- periodReturn(ARG[1:791,], period = "daily", type = "log")
r1t <- periodReturn(ARG[1:791,], period = "monthly", type = "log")
r2 <- periodReturn(BIC, period = "monthly", type = "log")
r2v <- periodReturn(BIC[1:791,], period = "daily", type = "log")
r2t <- periodReturn(BIC[1:791,], period = "monthly", type = "log")
r3 <- periodReturn(BIC_p, period = "monthly", type = "log")
r3v <- periodReturn(BIC_p[1:791,], period = "daily", type = "log")
r3t <- periodReturn(BIC_p[1:791,], period = "monthly", type = "log")
r4 <- periodReturn(CCB, period = "monthly", type = "log")
r4v <- periodReturn(CCB[1:791,], period = "daily", type = "log")
r4t <- periodReturn(CCB[1:791,], period = "monthly", type = "log")
r5 <- periodReturn(ECO, period = "monthly", type = "log")
r5v <- periodReturn(ECO[1:791,], period = "daily", type = "log")
r5t <- periodReturn(ECO[1:791,], period = "monthly", type = "log")
r6 <- periodReturn(GAA_p, period = "monthly", type = "log")
r6v <- periodReturn(GAA_p[1:791,], period = "daily", type = "log")
r6t <- periodReturn(GAA_p[1:791,], period = "monthly", type = "log")
r7 <- periodReturn(ISA, period = "monthly", type = "log")
r7v <- periodReturn(ISA[1:791,], period = "daily", type = "log")
r7t <- periodReturn(ISA[1:791,], period = "monthly", type = "log")
r8 <- periodReturn(NCH, period = "monthly", type = "log")
r8v <- periodReturn(NCH[1:791,], period = "daily", type = "log")
r8t <- periodReturn(NCH[1:791,], period = "monthly", type = "log")
r9 <- periodReturn(SIS, period = "monthly", type = "log")
r9v <- periodReturn(SIS[1:791,], period = "daily", type = "log")
r9t <- periodReturn(SIS[1:791,], period = "monthly", type = "log")
r10 <- periodReturn(SIS_p, period = "monthly", type = "log")
r10v <- periodReturn(SIS_p[1:791,], period = "daily", type = "log")
r10t <- periodReturn(SIS_p[1:791,], period = "monthly", type = "log")
#Una vez encontrados los retornos logarítmicos 

retornos <- cbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10)
retornosv <- cbind(r1v, r2v, r3v, r4v, r5v, r6v, r7v, r8v, r9v, r10v)
retornost <- cbind(r1t, r2t, r3t, r4t, r5t, r6t, r7t, r8t, r9t, r10t)

colnames(retornos) <- c("ARG","BIC","BIC_p","CCB","ECO","GAA_p","ISA","NCH","SIS","SIS_p")
colnames(retornosv) <- c("ARG","BIC","BIC_p","CCB","ECO","GAA_p","ISA","NCH","SIS","SIS_p")

#Generamos el retorno que queremos para nuestro portafolio
#Descargamos y llamamos los datos históricos del COLCAP
COLC <- read_csv("COLC.csv", locale = locale(decimal_mark = ",", grouping_mark = "."))
COLC[6:7] <- list(NULL)
COLC <- COLC[c(1,3,4,5,2)]
COLC$Fecha <- format(as.Date(COLC$Fecha, format="%d.%m.%Y"),"%Y/%m/%d")
COLC <- xts(COLC[-1], as.POSIXct(COLC$Fecha))
#Al igual con las acciones anteriores formamos un vector de retornos logarítmicos
rcol <- periodReturn(COLC, period = "monthly", type = "log")
rcolv <- periodReturn(COLC[1:791,], period = "daily", type = "log")
rcolt <- periodReturn(COLC[1:791,], period = "monthly", type = "log")
#Ahora escogemos el mejor ajuste probabilistico de estos datos, de manera que se pueda obtener, con alguna distribución, un retorno esperado

fit.norm <- fitdist(as.numeric(as.character(rcolv$daily.return)), "norm")
r_p <- fit.norm$estimate["mean"]

COMB <- as.matrix(combn(1:10, 5))

pesoptim <- function(R){
  funds <- colnames(R)
  pfolio <- portfolio.spec(assets=funds)
  pfolio <- add.constraint(portfolio = pfolio, type = "full_investment")
  pfolio <- add.constraint(portfolio = pfolio, type = "box", min = -0.075, max=0.39)
  pfolio <- add.objective(portfolio = pfolio, type = "return", name = "mean", target = r_p)
  pfolio <- add.objective(portfolio = pfolio, type = "risk", name = "var")
  pfolio <- optimize.portfolio(R=R, portfolio = pfolio, optimize_method = "ROI", trace = TRUE)
  return(pfolio)
}

k=189
R = cbind(retornosv[,COMB[1,k]], retornosv[,COMB[2,k]], retornosv[,COMB[3,k]], retornosv[,COMB[4,k]], retornosv[,COMB[5,k]])
t1 <- returnValue(pesoptim(R))
p1 <- t1$weights[1]*retornost[,COMB[1,k]]
p2 <- t1$weights[2]*retornost[,COMB[2,k]]
p3 <- t1$weights[3]*retornost[,COMB[3,k]]
p4 <- t1$weights[4]*retornost[,COMB[4,k]]
p5 <- t1$weights[5]*retornost[,COMB[5,k]]

p1t <- p1+p2+p3+p4+p5
p1t <- p1t[36:39]
total <- cbind(rcolt, p1t)
plot.xts(total, ylim = c(-0.09,0.15))




#BALANCEO 2
r1 <- periodReturn(ARG, period = "monthly", type = "log")
r1v <- periodReturn(ARG[1:854,], period = "daily", type = "log")
r1t <- periodReturn(ARG[1:854,], period = "monthly", type = "log")
r2 <- periodReturn(BIC, period = "monthly", type = "log")
r2v <- periodReturn(BIC[1:854,], period = "daily", type = "log")
r2t <- periodReturn(BIC[1:854,], period = "monthly", type = "log")
r3 <- periodReturn(BIC_p, period = "monthly", type = "log")
r3v <- periodReturn(BIC_p[1:854,], period = "daily", type = "log")
r3t <- periodReturn(BIC_p[1:854,], period = "monthly", type = "log")
r4 <- periodReturn(CCB, period = "monthly", type = "log")
r4v <- periodReturn(CCB[1:854,], period = "daily", type = "log")
r4t <- periodReturn(CCB[1:854,], period = "monthly", type = "log")
r5 <- periodReturn(ECO, period = "monthly", type = "log")
r5v <- periodReturn(ECO[1:854,], period = "daily", type = "log")
r5t <- periodReturn(ECO[1:854,], period = "monthly", type = "log")
r6 <- periodReturn(GAA_p, period = "monthly", type = "log")
r6v <- periodReturn(GAA_p[1:854,], period = "daily", type = "log")
r6t <- periodReturn(GAA_p[1:854,], period = "monthly", type = "log")
r7 <- periodReturn(ISA, period = "monthly", type = "log")
r7v <- periodReturn(ISA[1:854,], period = "daily", type = "log")
r7t <- periodReturn(ISA[1:854,], period = "monthly", type = "log")
r8 <- periodReturn(NCH, period = "monthly", type = "log")
r8v <- periodReturn(NCH[1:854,], period = "daily", type = "log")
r8t <- periodReturn(NCH[1:854,], period = "monthly", type = "log")
r9 <- periodReturn(SIS, period = "monthly", type = "log")
r9v <- periodReturn(SIS[1:854,], period = "daily", type = "log")
r9t <- periodReturn(SIS[1:854,], period = "monthly", type = "log")
r10 <- periodReturn(SIS_p, period = "monthly", type = "log")
r10v <- periodReturn(SIS_p[1:854,], period = "daily", type = "log")
r10t <- periodReturn(SIS_p[1:854,], period = "monthly", type = "log")
#Una vez encontrados los retornos logarítmicos 

retornos <- cbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10)
retornosv <- cbind(r1v, r2v, r3v, r4v, r5v, r6v, r7v, r8v, r9v, r10v)
retornost <- cbind(r1t, r2t, r3t, r4t, r5t, r6t, r7t, r8t, r9t, r10t)

colnames(retornos) <- c("ARG","BIC","BIC_p","CCB","ECO","GAA_p","ISA","NCH","SIS","SIS_p")
colnames(retornosv) <- c("ARG","BIC","BIC_p","CCB","ECO","GAA_p","ISA","NCH","SIS","SIS_p")

#Generamos el retorno que queremos para nuestro portafolio
#Descargamos y llamamos los datos históricos del COLCAP
COLC <- read_csv("COLC.csv", locale = locale(decimal_mark = ",", grouping_mark = "."))
COLC[6:7] <- list(NULL)
COLC <- COLC[c(1,3,4,5,2)]
COLC$Fecha <- format(as.Date(COLC$Fecha, format="%d.%m.%Y"),"%Y/%m/%d")
COLC <- xts(COLC[-1], as.POSIXct(COLC$Fecha))
#Al igual con las acciones anteriores formamos un vector de retornos logarítmicos
rcol <- periodReturn(COLC, period = "monthly", type = "log")
rcolv <- periodReturn(COLC[1:854,], period = "daily", type = "log")
rcolt <- periodReturn(COLC[1:854,], period = "monthly", type = "log")
#Ahora escogemos el mejor ajuste probabilistico de estos datos, de manera que se pueda obtener, con alguna distribución, un retorno esperado

fit.norm <- fitdist(as.numeric(as.character(rcolv$daily.return)), "norm")
r_p <- fit.norm$estimate["mean"]

COMB <- as.matrix(combn(1:10, 5))

pesoptim <- function(R){
  funds <- colnames(R)
  pfolio <- portfolio.spec(assets=funds)
  pfolio <- add.constraint(portfolio = pfolio, type = "full_investment")
  pfolio <- add.constraint(portfolio = pfolio, type = "box", min = -0.075, max=0.39)
  pfolio <- add.objective(portfolio = pfolio, type = "return", name = "mean", target = r_p)
  pfolio <- add.objective(portfolio = pfolio, type = "risk", name = "var")
  pfolio <- optimize.portfolio(R=R, portfolio = pfolio, optimize_method = "ROI", trace = TRUE)
  return(pfolio)
}

k=189
R = cbind(retornosv[,COMB[1,k]], retornosv[,COMB[2,k]], retornosv[,COMB[3,k]], retornosv[,COMB[4,k]], retornosv[,COMB[5,k]])
t2 <- returnValue(pesoptim(R))
p1 <- t2$weights[1]*retornost[,COMB[1,k]]
p2 <- t2$weights[2]*retornost[,COMB[2,k]]
p3 <- t2$weights[3]*retornost[,COMB[3,k]]
p4 <- t2$weights[4]*retornost[,COMB[4,k]]
p5 <- t2$weights[5]*retornost[,COMB[5,k]]

p2t <- p1+p2+p3+p4+p5
p2t <- p2t[39:42]
total <- cbind(rcolt, p2t)
plot.xts(total, ylim = c(-0.09,0.15))



#BALANCEO 3
r1 <- periodReturn(ARG, period = "monthly", type = "log")
r1v <- periodReturn(ARG[1:913,], period = "daily", type = "log")
r1t <- periodReturn(ARG[1:913,], period = "monthly", type = "log")
r2 <- periodReturn(BIC, period = "monthly", type = "log")
r2v <- periodReturn(BIC[1:913,], period = "daily", type = "log")
r2t <- periodReturn(BIC[1:913,], period = "monthly", type = "log")
r3 <- periodReturn(BIC_p, period = "monthly", type = "log")
r3v <- periodReturn(BIC_p[1:913,], period = "daily", type = "log")
r3t <- periodReturn(BIC_p[1:913,], period = "monthly", type = "log")
r4 <- periodReturn(CCB, period = "monthly", type = "log")
r4v <- periodReturn(CCB[1:913,], period = "daily", type = "log")
r4t <- periodReturn(CCB[1:913,], period = "monthly", type = "log")
r5 <- periodReturn(ECO, period = "monthly", type = "log")
r5v <- periodReturn(ECO[1:913,], period = "daily", type = "log")
r5t <- periodReturn(ECO[1:913,], period = "monthly", type = "log")
r6 <- periodReturn(GAA_p, period = "monthly", type = "log")
r6v <- periodReturn(GAA_p[1:913,], period = "daily", type = "log")
r6t <- periodReturn(GAA_p[1:913,], period = "monthly", type = "log")
r7 <- periodReturn(ISA, period = "monthly", type = "log")
r7v <- periodReturn(ISA[1:913,], period = "daily", type = "log")
r7t <- periodReturn(ISA[1:913,], period = "monthly", type = "log")
r8 <- periodReturn(NCH, period = "monthly", type = "log")
r8v <- periodReturn(NCH[1:913,], period = "daily", type = "log")
r8t <- periodReturn(NCH[1:913,], period = "monthly", type = "log")
r9 <- periodReturn(SIS, period = "monthly", type = "log")
r9v <- periodReturn(SIS[1:913,], period = "daily", type = "log")
r9t <- periodReturn(SIS[1:913,], period = "monthly", type = "log")
r10 <- periodReturn(SIS_p, period = "monthly", type = "log")
r10v <- periodReturn(SIS_p[1:913,], period = "daily", type = "log")
r10t <- periodReturn(SIS_p[1:913,], period = "monthly", type = "log")
#Una vez encontrados los retornos logarítmicos 

retornos <- cbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10)
retornosv <- cbind(r1v, r2v, r3v, r4v, r5v, r6v, r7v, r8v, r9v, r10v)
retornost <- cbind(r1t, r2t, r3t, r4t, r5t, r6t, r7t, r8t, r9t, r10t)

colnames(retornos) <- c("ARG","BIC","BIC_p","CCB","ECO","GAA_p","ISA","NCH","SIS","SIS_p")
colnames(retornosv) <- c("ARG","BIC","BIC_p","CCB","ECO","GAA_p","ISA","NCH","SIS","SIS_p")

#Generamos el retorno que queremos para nuestro portafolio
#Descargamos y llamamos los datos históricos del COLCAP
COLC <- read_csv("COLC.csv", locale = locale(decimal_mark = ",", grouping_mark = "."))
COLC[6:7] <- list(NULL)
COLC <- COLC[c(1,3,4,5,2)]
COLC$Fecha <- format(as.Date(COLC$Fecha, format="%d.%m.%Y"),"%Y/%m/%d")
COLC <- xts(COLC[-1], as.POSIXct(COLC$Fecha))
#Al igual con las acciones anteriores formamos un vector de retornos logarítmicos
rcol <- periodReturn(COLC, period = "monthly", type = "log")
rcolv <- periodReturn(COLC[1:913,], period = "daily", type = "log")
rcolt <- periodReturn(COLC[1:913,], period = "monthly", type = "log")
#Ahora escogemos el mejor ajuste probabilistico de estos datos, de manera que se pueda obtener, con alguna distribución, un retorno esperado

fit.norm <- fitdist(as.numeric(as.character(rcolv$daily.return)), "norm")
r_p <- fit.norm$estimate["mean"]

COMB <- as.matrix(combn(1:10, 5))

pesoptim <- function(R){
  funds <- colnames(R)
  pfolio <- portfolio.spec(assets=funds)
  pfolio <- add.constraint(portfolio = pfolio, type = "full_investment")
  pfolio <- add.constraint(portfolio = pfolio, type = "box", min = -0.075, max=0.39)
  pfolio <- add.objective(portfolio = pfolio, type = "return", name = "mean", target = r_p)
  pfolio <- add.objective(portfolio = pfolio, type = "risk", name = "var")
  pfolio <- optimize.portfolio(R=R, portfolio = pfolio, optimize_method = "ROI", trace = TRUE)
  return(pfolio)
}

k=189
R = cbind(retornosv[,COMB[1,k]], retornosv[,COMB[2,k]], retornosv[,COMB[3,k]], retornosv[,COMB[4,k]], retornosv[,COMB[5,k]])
t3 <- returnValue(pesoptim(R))
p1 <- t3$weights[1]*retornost[,COMB[1,k]]
p2 <- t3$weights[2]*retornost[,COMB[2,k]]
p3 <- t3$weights[3]*retornost[,COMB[3,k]]
p4 <- t3$weights[4]*retornost[,COMB[4,k]]
p5 <- t3$weights[5]*retornost[,COMB[5,k]]

p3t <- p1+p2+p3+p4+p5
p3t <- p3t[42:45]
total <- cbind(rcolt, p3t)
plot.xts(total, ylim = c(-0.09,0.15))



#BALANCEO 4
r1 <- periodReturn(ARG, period = "monthly", type = "log")
r1v <- periodReturn(ARG[1:973,], period = "daily", type = "log")
r1t <- periodReturn(ARG[1:973,], period = "monthly", type = "log")
r2 <- periodReturn(BIC, period = "monthly", type = "log")
r2v <- periodReturn(BIC[1:973,], period = "daily", type = "log")
r2t <- periodReturn(BIC[1:973,], period = "monthly", type = "log")
r3 <- periodReturn(BIC_p, period = "monthly", type = "log")
r3v <- periodReturn(BIC_p[1:973,], period = "daily", type = "log")
r3t <- periodReturn(BIC_p[1:973,], period = "monthly", type = "log")
r4 <- periodReturn(CCB, period = "monthly", type = "log")
r4v <- periodReturn(CCB[1:973,], period = "daily", type = "log")
r4t <- periodReturn(CCB[1:973,], period = "monthly", type = "log")
r5 <- periodReturn(ECO, period = "monthly", type = "log")
r5v <- periodReturn(ECO[1:973,], period = "daily", type = "log")
r5t <- periodReturn(ECO[1:973,], period = "monthly", type = "log")
r6 <- periodReturn(GAA_p, period = "monthly", type = "log")
r6v <- periodReturn(GAA_p[1:973,], period = "daily", type = "log")
r6t <- periodReturn(GAA_p[1:973,], period = "monthly", type = "log")
r7 <- periodReturn(ISA, period = "monthly", type = "log")
r7v <- periodReturn(ISA[1:973,], period = "daily", type = "log")
r7t <- periodReturn(ISA[1:973,], period = "monthly", type = "log")
r8 <- periodReturn(NCH, period = "monthly", type = "log")
r8v <- periodReturn(NCH[1:973,], period = "daily", type = "log")
r8t <- periodReturn(NCH[1:973,], period = "monthly", type = "log")
r9 <- periodReturn(SIS, period = "monthly", type = "log")
r9v <- periodReturn(SIS[1:973,], period = "daily", type = "log")
r9t <- periodReturn(SIS[1:973,], period = "monthly", type = "log")
r10 <- periodReturn(SIS_p, period = "monthly", type = "log")
r10v <- periodReturn(SIS_p[1:973,], period = "daily", type = "log")
r10t <- periodReturn(SIS_p[1:973,], period = "monthly", type = "log")
#Una vez encontrados los retornos logarítmicos 

retornos <- cbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10)
retornosv <- cbind(r1v, r2v, r3v, r4v, r5v, r6v, r7v, r8v, r9v, r10v)
retornost <- cbind(r1t, r2t, r3t, r4t, r5t, r6t, r7t, r8t, r9t, r10t)

colnames(retornos) <- c("ARG","BIC","BIC_p","CCB","ECO","GAA_p","ISA","NCH","SIS","SIS_p")
colnames(retornosv) <- c("ARG","BIC","BIC_p","CCB","ECO","GAA_p","ISA","NCH","SIS","SIS_p")

#Generamos el retorno que queremos para nuestro portafolio
#Descargamos y llamamos los datos históricos del COLCAP
COLC <- read_csv("COLC.csv", locale = locale(decimal_mark = ",", grouping_mark = "."))
COLC[6:7] <- list(NULL)
COLC <- COLC[c(1,3,4,5,2)]
COLC$Fecha <- format(as.Date(COLC$Fecha, format="%d.%m.%Y"),"%Y/%m/%d")
COLC <- xts(COLC[-1], as.POSIXct(COLC$Fecha))
#Al igual con las acciones anteriores formamos un vector de retornos logarítmicos
rcol <- periodReturn(COLC, period = "monthly", type = "log")
rcolv <- periodReturn(COLC[1:973,], period = "daily", type = "log")
rcolt <- periodReturn(COLC[1:973,], period = "monthly", type = "log")
#Ahora escogemos el mejor ajuste probabilistico de estos datos, de manera que se pueda obtener, con alguna distribución, un retorno esperado

fit.norm <- fitdist(as.numeric(as.character(rcolv$daily.return)), "norm")
r_p <- fit.norm$estimate["mean"]

COMB <- as.matrix(combn(1:10, 5))

pesoptim <- function(R){
  funds <- colnames(R)
  pfolio <- portfolio.spec(assets=funds)
  pfolio <- add.constraint(portfolio = pfolio, type = "full_investment")
  pfolio <- add.constraint(portfolio = pfolio, type = "box", min = -0.075, max=0.39)
  pfolio <- add.objective(portfolio = pfolio, type = "return", name = "mean", target = r_p)
  pfolio <- add.objective(portfolio = pfolio, type = "risk", name = "var")
  pfolio <- optimize.portfolio(R=R, portfolio = pfolio, optimize_method = "ROI", trace = TRUE)
  return(pfolio)
}

k=189
R = cbind(retornosv[,COMB[1,k]], retornosv[,COMB[2,k]], retornosv[,COMB[3,k]], retornosv[,COMB[4,k]], retornosv[,COMB[5,k]])
t4 <- returnValue(pesoptim(R))
p1 <- t4$weights[1]*retornost[,COMB[1,k]]
p2 <- t4$weights[2]*retornost[,COMB[2,k]]
p3 <- t4$weights[3]*retornost[,COMB[3,k]]
p4 <- t4$weights[4]*retornost[,COMB[4,k]]
p5 <- t4$weights[5]*retornost[,COMB[5,k]]

p4t <- p1+p2+p3+p4+p5
p4t <- p4t[45:48]
total <- cbind(rcolt, p4t)
plot.xts(total, ylim = c(-0.09,0.15))




#BALANCEO 5
r1 <- periodReturn(ARG, period = "monthly", type = "log")
r1v <- periodReturn(ARG[1:1033,], period = "daily", type = "log")
r1t <- periodReturn(ARG[1:1033,], period = "monthly", type = "log")
r2 <- periodReturn(BIC, period = "monthly", type = "log")
r2v <- periodReturn(BIC[1:1033,], period = "daily", type = "log")
r2t <- periodReturn(BIC[1:1033,], period = "monthly", type = "log")
r3 <- periodReturn(BIC_p, period = "monthly", type = "log")
r3v <- periodReturn(BIC_p[1:1033,], period = "daily", type = "log")
r3t <- periodReturn(BIC_p[1:1033,], period = "monthly", type = "log")
r4 <- periodReturn(CCB, period = "monthly", type = "log")
r4v <- periodReturn(CCB[1:1033,], period = "daily", type = "log")
r4t <- periodReturn(CCB[1:1033,], period = "monthly", type = "log")
r5 <- periodReturn(ECO, period = "monthly", type = "log")
r5v <- periodReturn(ECO[1:1033,], period = "daily", type = "log")
r5t <- periodReturn(ECO[1:1033,], period = "monthly", type = "log")
r6 <- periodReturn(GAA_p, period = "monthly", type = "log")
r6v <- periodReturn(GAA_p[1:1033,], period = "daily", type = "log")
r6t <- periodReturn(GAA_p[1:1033,], period = "monthly", type = "log")
r7 <- periodReturn(ISA, period = "monthly", type = "log")
r7v <- periodReturn(ISA[1:1033,], period = "daily", type = "log")
r7t <- periodReturn(ISA[1:1033,], period = "monthly", type = "log")
r8 <- periodReturn(NCH, period = "monthly", type = "log")
r8v <- periodReturn(NCH[1:1033,], period = "daily", type = "log")
r8t <- periodReturn(NCH[1:1033,], period = "monthly", type = "log")
r9 <- periodReturn(SIS, period = "monthly", type = "log")
r9v <- periodReturn(SIS[1:1033,], period = "daily", type = "log")
r9t <- periodReturn(SIS[1:1033,], period = "monthly", type = "log")
r10 <- periodReturn(SIS_p, period = "monthly", type = "log")
r10v <- periodReturn(SIS_p[1:1033,], period = "daily", type = "log")
r10t <- periodReturn(SIS_p[1:1033,], period = "monthly", type = "log")
#Una vez encontrados los retornos logarítmicos 

retornos <- cbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10)
retornosv <- cbind(r1v, r2v, r3v, r4v, r5v, r6v, r7v, r8v, r9v, r10v)
retornost <- cbind(r1t, r2t, r3t, r4t, r5t, r6t, r7t, r8t, r9t, r10t)

colnames(retornos) <- c("ARG","BIC","BIC_p","CCB","ECO","GAA_p","ISA","NCH","SIS","SIS_p")
colnames(retornosv) <- c("ARG","BIC","BIC_p","CCB","ECO","GAA_p","ISA","NCH","SIS","SIS_p")

#Generamos el retorno que queremos para nuestro portafolio
#Descargamos y llamamos los datos históricos del COLCAP
COLC <- read_csv("COLC.csv", locale = locale(decimal_mark = ",", grouping_mark = "."))
COLC[6:7] <- list(NULL)
COLC <- COLC[c(1,3,4,5,2)]
COLC$Fecha <- format(as.Date(COLC$Fecha, format="%d.%m.%Y"),"%Y/%m/%d")
COLC <- xts(COLC[-1], as.POSIXct(COLC$Fecha))
#Al igual con las acciones anteriores formamos un vector de retornos logarítmicos
rcol <- periodReturn(COLC, period = "monthly", type = "log")
rcolv <- periodReturn(COLC[1:1033,], period = "daily", type = "log")
rcolt <- periodReturn(COLC[1:1033,], period = "monthly", type = "log")
#Ahora escogemos el mejor ajuste probabilistico de estos datos, de manera que se pueda obtener, con alguna distribución, un retorno esperado

fit.norm <- fitdist(as.numeric(as.character(rcolv$daily.return)), "norm")
r_p <- fit.norm$estimate["mean"]

COMB <- as.matrix(combn(1:10, 5))

pesoptim <- function(R){
  funds <- colnames(R)
  pfolio <- portfolio.spec(assets=funds)
  pfolio <- add.constraint(portfolio = pfolio, type = "full_investment")
  pfolio <- add.constraint(portfolio = pfolio, type = "box", min = -0.075, max=0.39)
  pfolio <- add.objective(portfolio = pfolio, type = "return", name = "mean", target = r_p)
  pfolio <- add.objective(portfolio = pfolio, type = "risk", name = "var")
  pfolio <- optimize.portfolio(R=R, portfolio = pfolio, optimize_method = "ROI", trace = TRUE)
  return(pfolio)
}

k=189
R = cbind(retornosv[,COMB[1,k]], retornosv[,COMB[2,k]], retornosv[,COMB[3,k]], retornosv[,COMB[4,k]], retornosv[,COMB[5,k]])
t5 <- returnValue(pesoptim(R))
p1 <- t5$weights[1]*retornost[,COMB[1,k]]
p2 <- t5$weights[2]*retornost[,COMB[2,k]]
p3 <- t5$weights[3]*retornost[,COMB[3,k]]
p4 <- t5$weights[4]*retornost[,COMB[4,k]]
p5 <- t5$weights[5]*retornost[,COMB[5,k]]

p5t <- p1+p2+p3+p4+p5
p5t <- p5t[48:51]
total <- cbind(rcolt, p5t)
plot.xts(total, ylim = c(-0.09,0.15))





#BALANCEO 6
r1 <- periodReturn(ARG, period = "monthly", type = "log")
r1v <- periodReturn(ARG[1:1096,], period = "daily", type = "log")
r1t <- periodReturn(ARG[1:1096,], period = "monthly", type = "log")
r2 <- periodReturn(BIC, period = "monthly", type = "log")
r2v <- periodReturn(BIC[1:1096,], period = "daily", type = "log")
r2t <- periodReturn(BIC[1:1096,], period = "monthly", type = "log")
r3 <- periodReturn(BIC_p, period = "monthly", type = "log")
r3v <- periodReturn(BIC_p[1:1096,], period = "daily", type = "log")
r3t <- periodReturn(BIC_p[1:1096,], period = "monthly", type = "log")
r4 <- periodReturn(CCB, period = "monthly", type = "log")
r4v <- periodReturn(CCB[1:1096,], period = "daily", type = "log")
r4t <- periodReturn(CCB[1:1096,], period = "monthly", type = "log")
r5 <- periodReturn(ECO, period = "monthly", type = "log")
r5v <- periodReturn(ECO[1:1096,], period = "daily", type = "log")
r5t <- periodReturn(ECO[1:1096,], period = "monthly", type = "log")
r6 <- periodReturn(GAA_p, period = "monthly", type = "log")
r6v <- periodReturn(GAA_p[1:1096,], period = "daily", type = "log")
r6t <- periodReturn(GAA_p[1:1096,], period = "monthly", type = "log")
r7 <- periodReturn(ISA, period = "monthly", type = "log")
r7v <- periodReturn(ISA[1:1096,], period = "daily", type = "log")
r7t <- periodReturn(ISA[1:1096,], period = "monthly", type = "log")
r8 <- periodReturn(NCH, period = "monthly", type = "log")
r8v <- periodReturn(NCH[1:1096,], period = "daily", type = "log")
r8t <- periodReturn(NCH[1:1096,], period = "monthly", type = "log")
r9 <- periodReturn(SIS, period = "monthly", type = "log")
r9v <- periodReturn(SIS[1:1096,], period = "daily", type = "log")
r9t <- periodReturn(SIS[1:1096,], period = "monthly", type = "log")
r10 <- periodReturn(SIS_p, period = "monthly", type = "log")
r10v <- periodReturn(SIS_p[1:1096,], period = "daily", type = "log")
r10t <- periodReturn(SIS_p[1:1096,], period = "monthly", type = "log")
#Una vez encontrados los retornos logarítmicos 

retornos <- cbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10)
retornosv <- cbind(r1v, r2v, r3v, r4v, r5v, r6v, r7v, r8v, r9v, r10v)
retornost <- cbind(r1t, r2t, r3t, r4t, r5t, r6t, r7t, r8t, r9t, r10t)

colnames(retornos) <- c("ARG","BIC","BIC_p","CCB","ECO","GAA_p","ISA","NCH","SIS","SIS_p")
colnames(retornosv) <- c("ARG","BIC","BIC_p","CCB","ECO","GAA_p","ISA","NCH","SIS","SIS_p")

#Generamos el retorno que queremos para nuestro portafolio
#Descargamos y llamamos los datos históricos del COLCAP
COLC <- read_csv("COLC.csv", locale = locale(decimal_mark = ",", grouping_mark = "."))
COLC[6:7] <- list(NULL)
COLC <- COLC[c(1,3,4,5,2)]
COLC$Fecha <- format(as.Date(COLC$Fecha, format="%d.%m.%Y"),"%Y/%m/%d")
COLC <- xts(COLC[-1], as.POSIXct(COLC$Fecha))
#Al igual con las acciones anteriores formamos un vector de retornos logarítmicos
rcol <- periodReturn(COLC, period = "monthly", type = "log")
rcolv <- periodReturn(COLC[1:1096,], period = "daily", type = "log")
rcolt <- periodReturn(COLC[1:1096,], period = "monthly", type = "log")
#Ahora escogemos el mejor ajuste probabilistico de estos datos, de manera que se pueda obtener, con alguna distribución, un retorno esperado

fit.norm <- fitdist(as.numeric(as.character(rcolv$daily.return)), "norm")
r_p <- fit.norm$estimate["mean"]

COMB <- as.matrix(combn(1:10, 5))

pesoptim <- function(R){
  funds <- colnames(R)
  pfolio <- portfolio.spec(assets=funds)
  pfolio <- add.constraint(portfolio = pfolio, type = "full_investment")
  pfolio <- add.constraint(portfolio = pfolio, type = "box", min = -0.075, max=0.39)
  pfolio <- add.objective(portfolio = pfolio, type = "return", name = "mean", target = r_p)
  pfolio <- add.objective(portfolio = pfolio, type = "risk", name = "var")
  pfolio <- optimize.portfolio(R=R, portfolio = pfolio, optimize_method = "ROI", trace = TRUE)
  return(pfolio)
}

k=189
R = cbind(retornosv[,COMB[1,k]], retornosv[,COMB[2,k]], retornosv[,COMB[3,k]], retornosv[,COMB[4,k]], retornosv[,COMB[5,k]])
t6 <- returnValue(pesoptim(R))
p1 <- t6$weights[1]*retornost[,COMB[1,k]]
p2 <- t6$weights[2]*retornost[,COMB[2,k]]
p3 <- t6$weights[3]*retornost[,COMB[3,k]]
p4 <- t6$weights[4]*retornost[,COMB[4,k]]
p5 <- t6$weights[5]*retornost[,COMB[5,k]]

p6t <- p1+p2+p3+p4+p5
p6t <- p6t[51:54]
total <- cbind(rcolt, p6t)
plot.xts(total, ylim = c(-0.09,0.15))





#BALANCEO 7
r1 <- periodReturn(ARG, period = "monthly", type = "log")
r1v <- periodReturn(ARG[1:1156,], period = "daily", type = "log")
r1t <- periodReturn(ARG[1:1156,], period = "monthly", type = "log")
r2 <- periodReturn(BIC, period = "monthly", type = "log")
r2v <- periodReturn(BIC[1:1156,], period = "daily", type = "log")
r2t <- periodReturn(BIC[1:1156,], period = "monthly", type = "log")
r3 <- periodReturn(BIC_p, period = "monthly", type = "log")
r3v <- periodReturn(BIC_p[1:1156,], period = "daily", type = "log")
r3t <- periodReturn(BIC_p[1:1156,], period = "monthly", type = "log")
r4 <- periodReturn(CCB, period = "monthly", type = "log")
r4v <- periodReturn(CCB[1:1156,], period = "daily", type = "log")
r4t <- periodReturn(CCB[1:1156,], period = "monthly", type = "log")
r5 <- periodReturn(ECO, period = "monthly", type = "log")
r5v <- periodReturn(ECO[1:1156,], period = "daily", type = "log")
r5t <- periodReturn(ECO[1:1156,], period = "monthly", type = "log")
r6 <- periodReturn(GAA_p, period = "monthly", type = "log")
r6v <- periodReturn(GAA_p[1:1156,], period = "daily", type = "log")
r6t <- periodReturn(GAA_p[1:1156,], period = "monthly", type = "log")
r7 <- periodReturn(ISA, period = "monthly", type = "log")
r7v <- periodReturn(ISA[1:1156,], period = "daily", type = "log")
r7t <- periodReturn(ISA[1:1156,], period = "monthly", type = "log")
r8 <- periodReturn(NCH, period = "monthly", type = "log")
r8v <- periodReturn(NCH[1:1156,], period = "daily", type = "log")
r8t <- periodReturn(NCH[1:1156,], period = "monthly", type = "log")
r9 <- periodReturn(SIS, period = "monthly", type = "log")
r9v <- periodReturn(SIS[1:1156,], period = "daily", type = "log")
r9t <- periodReturn(SIS[1:1156,], period = "monthly", type = "log")
r10 <- periodReturn(SIS_p, period = "monthly", type = "log")
r10v <- periodReturn(SIS_p[1:1156,], period = "daily", type = "log")
r10t <- periodReturn(SIS_p[1:1156,], period = "monthly", type = "log")
#Una vez encontrados los retornos logarítmicos 

retornos <- cbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10)
retornosv <- cbind(r1v, r2v, r3v, r4v, r5v, r6v, r7v, r8v, r9v, r10v)
retornost <- cbind(r1t, r2t, r3t, r4t, r5t, r6t, r7t, r8t, r9t, r10t)

colnames(retornos) <- c("ARG","BIC","BIC_p","CCB","ECO","GAA_p","ISA","NCH","SIS","SIS_p")
colnames(retornosv) <- c("ARG","BIC","BIC_p","CCB","ECO","GAA_p","ISA","NCH","SIS","SIS_p")

#Generamos el retorno que queremos para nuestro portafolio
#Descargamos y llamamos los datos históricos del COLCAP
COLC <- read_csv("COLC.csv", locale = locale(decimal_mark = ",", grouping_mark = "."))
COLC[6:7] <- list(NULL)
COLC <- COLC[c(1,3,4,5,2)]
COLC$Fecha <- format(as.Date(COLC$Fecha, format="%d.%m.%Y"),"%Y/%m/%d")
COLC <- xts(COLC[-1], as.POSIXct(COLC$Fecha))
#Al igual con las acciones anteriores formamos un vector de retornos logarítmicos
rcol <- periodReturn(COLC, period = "monthly", type = "log")
rcolv <- periodReturn(COLC[1:1156,], period = "daily", type = "log")
rcolt <- periodReturn(COLC[1:1156,], period = "monthly", type = "log")
#Ahora escogemos el mejor ajuste probabilistico de estos datos, de manera que se pueda obtener, con alguna distribución, un retorno esperado

fit.norm <- fitdist(as.numeric(as.character(rcolv$daily.return)), "norm")
r_p <- fit.norm$estimate["mean"]

COMB <- as.matrix(combn(1:10, 5))

pesoptim <- function(R){
  funds <- colnames(R)
  pfolio <- portfolio.spec(assets=funds)
  pfolio <- add.constraint(portfolio = pfolio, type = "full_investment")
  pfolio <- add.constraint(portfolio = pfolio, type = "box", min = -0.075, max=0.39)
  pfolio <- add.objective(portfolio = pfolio, type = "return", name = "mean", target = r_p)
  pfolio <- add.objective(portfolio = pfolio, type = "risk", name = "var")
  pfolio <- optimize.portfolio(R=R, portfolio = pfolio, optimize_method = "ROI", trace = TRUE)
  return(pfolio)
}

k=189
R = cbind(retornosv[,COMB[1,k]], retornosv[,COMB[2,k]], retornosv[,COMB[3,k]], retornosv[,COMB[4,k]], retornosv[,COMB[5,k]])
t7 <- returnValue(pesoptim(R))
p1 <- t7$weights[1]*retornost[,COMB[1,k]]
p2 <- t7$weights[2]*retornost[,COMB[2,k]]
p3 <- t7$weights[3]*retornost[,COMB[3,k]]
p4 <- t7$weights[4]*retornost[,COMB[4,k]]
p5 <- t7$weights[5]*retornost[,COMB[5,k]]

p7t <- p1+p2+p3+p4+p5
p7t <- p7t[54:57]
total <- cbind(rcolt, p7t)
plot.xts(total, ylim = c(-0.09,0.16))





#BALANCEO 8
r1 <- periodReturn(ARG, period = "monthly", type = "log")
r1v <- periodReturn(ARG[1:1216,], period = "daily", type = "log")
r1t <- periodReturn(ARG[1:1216,], period = "monthly", type = "log")
r2 <- periodReturn(BIC, period = "monthly", type = "log")
r2v <- periodReturn(BIC[1:1216,], period = "daily", type = "log")
r2t <- periodReturn(BIC[1:1216,], period = "monthly", type = "log")
r3 <- periodReturn(BIC_p, period = "monthly", type = "log")
r3v <- periodReturn(BIC_p[1:1216,], period = "daily", type = "log")
r3t <- periodReturn(BIC_p[1:1216,], period = "monthly", type = "log")
r4 <- periodReturn(CCB, period = "monthly", type = "log")
r4v <- periodReturn(CCB[1:1216,], period = "daily", type = "log")
r4t <- periodReturn(CCB[1:1216,], period = "monthly", type = "log")
r5 <- periodReturn(ECO, period = "monthly", type = "log")
r5v <- periodReturn(ECO[1:1216,], period = "daily", type = "log")
r5t <- periodReturn(ECO[1:1216,], period = "monthly", type = "log")
r6 <- periodReturn(GAA_p, period = "monthly", type = "log")
r6v <- periodReturn(GAA_p[1:1216,], period = "daily", type = "log")
r6t <- periodReturn(GAA_p[1:1216,], period = "monthly", type = "log")
r7 <- periodReturn(ISA, period = "monthly", type = "log")
r7v <- periodReturn(ISA[1:1216,], period = "daily", type = "log")
r7t <- periodReturn(ISA[1:1216,], period = "monthly", type = "log")
r8 <- periodReturn(NCH, period = "monthly", type = "log")
r8v <- periodReturn(NCH[1:1215,], period = "daily", type = "log")
r8t <- periodReturn(NCH[1:1215,], period = "monthly", type = "log")
r9 <- periodReturn(SIS, period = "monthly", type = "log")
r9v <- periodReturn(SIS[1:1216,], period = "daily", type = "log")
r9t <- periodReturn(SIS[1:1216,], period = "monthly", type = "log")
r10 <- periodReturn(SIS_p, period = "monthly", type = "log")
r10v <- periodReturn(SIS_p[1:1215,], period = "daily", type = "log")
r10t <- periodReturn(SIS_p[1:1215,], period = "monthly", type = "log")
#Una vez encontrados los retornos logarítmicos 

retornos <- cbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10)
retornosv <- cbind(r1v, r2v, r3v, r4v, r5v, r6v, r7v, r8v, r9v, r10v)
retornost <- cbind(r1t, r2t, r3t, r4t, r5t, r6t, r7t, r8t, r9t, r10t)

colnames(retornos) <- c("ARG","BIC","BIC_p","CCB","ECO","GAA_p","ISA","NCH","SIS","SIS_p")
colnames(retornosv) <- c("ARG","BIC","BIC_p","CCB","ECO","GAA_p","ISA","NCH","SIS","SIS_p")

#Generamos el retorno que queremos para nuestro portafolio
#Descargamos y llamamos los datos históricos del COLCAP
COLC <- read_csv("COLC.csv", locale = locale(decimal_mark = ",", grouping_mark = "."))
COLC[6:7] <- list(NULL)
COLC <- COLC[c(1,3,4,5,2)]
COLC$Fecha <- format(as.Date(COLC$Fecha, format="%d.%m.%Y"),"%Y/%m/%d")
COLC <- xts(COLC[-1], as.POSIXct(COLC$Fecha))
#Al igual con las acciones anteriores formamos un vector de retornos logarítmicos
rcol <- periodReturn(COLC, period = "monthly", type = "log")
rcolv <- periodReturn(COLC[1:1216,], period = "daily", type = "log")
rcolt <- periodReturn(COLC[1:1216,], period = "monthly", type = "log")
#Ahora escogemos el mejor ajuste probabilistico de estos datos, de manera que se pueda obtener, con alguna distribución, un retorno esperado

fit.norm <- fitdist(as.numeric(as.character(rcolv$daily.return)), "norm")
r_p <- fit.norm$estimate["mean"]

COMB <- as.matrix(combn(1:10, 5))

pesoptim <- function(R){
  funds <- colnames(R)
  pfolio <- portfolio.spec(assets=funds)
  pfolio <- add.constraint(portfolio = pfolio, type = "full_investment")
  pfolio <- add.constraint(portfolio = pfolio, type = "box", min = -0.075, max=0.39)
  pfolio <- add.objective(portfolio = pfolio, type = "return", name = "mean", target = r_p)
  pfolio <- add.objective(portfolio = pfolio, type = "risk", name = "var")
  pfolio <- optimize.portfolio(R=R, portfolio = pfolio, optimize_method = "ROI", trace = TRUE)
  return(pfolio)
}

k=189
R = cbind(retornosv[,COMB[1,k]], retornosv[,COMB[2,k]], retornosv[,COMB[3,k]], retornosv[,COMB[4,k]], retornosv[,COMB[5,k]])
t8 <- returnValue(pesoptim(R))
p1 <- t8$weights[1]*retornost[,COMB[1,k]]
p2 <- t8$weights[2]*retornost[,COMB[2,k]]
p3 <- t8$weights[3]*retornost[,COMB[3,k]]
p4 <- t8$weights[4]*retornost[,COMB[4,k]]
p5 <- t8$weights[5]*retornost[,COMB[5,k]]

p8t <- p1+p2+p3+p4+p5
p8t <- p8t[57:60]
total <- cbind(rcolt, p8t)
plot.xts(total, ylim = c(-0.09,0.16))




#VISUALIZACIÓN DEL PORTAFOLIO
#Una vez establecidos los compportamientos de nuestro portafolio, generamos la matriz que nos permite observar los rebalanceos y la gráfica del portafolio comparada con el índice
PORTAF <- merge(p0t, p1t, p2t, p3t, p4t, p5t, p6t, p7t, p8t)
plot(PORTAF)

PORTAFOLIO <- cbind(PORTAF, rcolt)
plot.xts(PORTAFOLIO, ylim=c(-0.12,0.15))


PESOS <- cbind(extractWeights(t0), extractWeights(t1), extractWeights(t2), extractWeights(t3), extractWeights(t4), extractWeights(t5), extractWeights(t6), extractWeights(t7), extractWeights(t8))
colnames(PESOS) <- c("Portafolio 3 años", "Balance 1", "Balance 2", "Balance 3", "Balance 4", "Balance 5", "Balance 6", "Balance 7", "Balance 8")
PESOS
xtable(PESOS)

