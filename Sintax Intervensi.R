library (forecast)  #BoxCox Arima auto.arima function is in forecast package
library (MASS)      #boxcox function is in MASS package
library (FitAR)     #LjungBoxTest function is in FitAR package
library (tsoutliers) #tso function is in tsoutliers package
library (lmtest)    #coeftest function is in lmtest package
library (stargazer) #stargazer function is in stargazer package
library (TSA)       #arimax function is in TSA package
library(tseries)
rm(list=ls())

setwd("C:/Users/Asem/Downloads/ADS/Time Series Analysis Harga Daging")
#Import Data
options (width=80)
raw_data <- read.csv("Inflation.csv",header = FALSE)
dim(raw_data)
head(raw_data)
PDB_ID <- ts(raw_data, start=1970, end=2013)
PDB_ID

#Time Series Plot
plot(PDB_ID, xlab="Waktu", ylab="US Dollar", col="blue", type="l",
     main="Plot PDB Indonesia 1970-2013")
points(PDB_ID, cex = .5, col = "red")
abline(v=1998, col=1, lty=2)
text(1998, PDB_ID[41], "1998\n(t=29)", pos=2)


#Pemodelan ARIMA untuk Data Sebelum Intervensi
Nt <- ts(PDB_ID[1:28], start=1970, end=1997)
Nt

#Creating Layout 1,1;2,3 
m <-rbind(c(1, 1),c(2, 3))
layout(m)
par(mar=c(3, 3, 1, 1))
plot(Nt, xlab="Waktu", ylab="US Dollar", col="blue", type="p",
     main="Plot PDB Indonesia 1970-1997 (Nt)")
points(Nt, cex = .5, col = "red")

# Trend Linear #Menunjukkan kalau trend nya naik
t <- 1970:1997
trend_PDB <- glm(Nt~t)
abline(trend_PDB, col="dark blue", lwd=2)

# ACF 
acf(Nt, 20, xlim=c(1,20))
text(10,0.8, "ACF")
# PACF 
pacf(Nt, 20, ylim=c(-.4,1))
text(10,0.8, "PACF")
# Show the ACF & PACF value
acf(Nt, 27, plot=FALSE)
pacf(Nt, 27, plot=FALSE)

#Stationarity check using Dickey Fuller Test
adf.test(Nt)  #lebih dari 0.05 maka belum stasioner dalam mean

#Box Cox
#Belum stasioner dlm varians
t1 <- 1:length(Nt)
#Search for optimal lambda 
par(mar = c(2, 4, 0, 1))
MASS::boxcox(lm(Nt~t1), lambda=seq(-1,1,1/10),ylab = "log-Likelihood")
lambda.model <- forecast::BoxCox.lambda(Nt)

#Box-Cox Transformation as A Treatment
Nt_box <- forecast::BoxCox(Nt, lambda = lambda.model)

#Identifikasi ARIMA Model
par(mfrow=c(1,1))
# Save ARIMA model
Nt_arima <-auto.arima(Nt, max.d=4, seasonal=FALSE, lambda=lambda.model,
                      stepwise=TRUE, trace=TRUE, max.p=10, max.q=10)
coeftest(Nt_arima)
Nt_arima$residuals
#drift : Should the ARIMA model include a linear drift term? (i.e., a linear regression with ARIMA errors is fitted.) The default is FALSE.
#trace : If TRUE, the list of ARIMA models considered will be reported


#Diagnosis Model ARIMA
#Ljung-Box Test for Nt  #Sudah white noise
Box.test(Nt_arima$residuals, lag=round(length(Nt)/5,0),
         type = "Ljung-Box", fitdf = 1)

#Kolmogorov-Smirnov Test  #Sudah berdistribusi normal
ks.test(Nt_arima$residuals, "pnorm",
        mean(Nt_arima$residuals),
        sd(Nt_arima$residuals))

#Analisis Intervensi
#Identifikasi Orde Interval
Nt_forecast <-forecast(Nt_arima, h = 16)
#h-step forecast for Nt
Nt_forecast

#Identification intervention order with plot of model residuals
error_idintv <-rep(0,44)
error_idintv[1:28] <- Nt_arima$residuals
error_idintv[29:44] <- PDB_ID[29:44] - Nt_forecast$mean
plot(error_idintv, type="h", xlab="Waktu (T)", ylab = "Residual", xaxt = "n")
abline(h=c(-3*sd(Nt_arima$residuals), 3*sd(Nt_arima$residuals)),col="blue", lty=2)
abline(v = 29, col = "red", lty = 3, lwd = 1.5)
text(29, 200, "T=29",cex = .8, pos = 2)
axis(1, at=c(0,10,20,30,40), labels=c("T-29" ,"T-19", "T-9", "T+1", "T+11"))
error_idintv

#ACF 
acf(error_idintv, 20, xlim=c(1,20))
text(10,0.2, "ACF")
#PACF 
pacf(error_idintv, 20)
text(10,0.2, "PACF")
ccf(error_idintv, raw_data, lag.max = 12, type="correlation", ylab='CCF')

#Plot PDB_ID vs forecasting of Nt
plot(forecast::forecast(Nt_arima, h=16), main =NA, ylab="US Dollar" )
points(PDB_ID, cex=.5, col="dark red", pch=19)
lines(PDB_ID, col="red")
abline(v=1998, lty=2)
text(1998, PDB_ID[41], "1998\n(t=29)", pos=2)
legend("topleft", legend=c("PDB_ID", "Peramalan Nt"), cex=0.75, lty=1,
       col=c("dark red", "blue"), pch=c(19,NA))

#dugaan b=0, s=1, r=0
#Peramalan Model Intervensi
#Generating f(It)
pulse29 <-filter(1*(seq(PDB_ID)==29),
                 filter = 0.2055, method = "rec",
                 sides = 1)*-112.4912
#Compute Model for PDB_ID 
PDB_arima <-Arima(PDB_ID, lambda = lambda.model,
                  order = c(0,1,1),
                  include.constant = TRUE, xreg = pulse29)
#Future Value of xreg 
xreg.rob = forecast::forecast(auto.arima(pulse29), h=5)$mean
#Forecasting 
forecast::forecast(PDB_arima, xreg = xreg.rob)
plot(forecast::forecast(PDB_arima, xreg = xreg.rob), main=NA)

