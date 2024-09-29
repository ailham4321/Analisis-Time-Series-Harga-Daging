library(ggplot2)
library(forecast)
library(astsa)
library(lmtest)
library(TSA)
library(strucchange)
library(reshape)
library(Rmisc)
library(fBasics)
library(tseries)
library(aTSA)
library(plotly)
library(keras)

setwd("C:/Users/Asem/Downloads")
df = read.csv("Harga Daging.csv")
str(df)

date_object = as.Date(df$Datetime, format = "%B %d, %Y")
df$Datetime = date_object

#write.csv(df, "Harga Daging Fixed.csv")

ggplot(data = df, aes(x = Datetime)) + 
  geom_line(aes(y = Daging))

Daging = ts(df$Daging, frequency = 1)
plot(Daging, main = "Harga Daging Jakarta", ylab = "Price", xlab = "Time")
abline(v=66, col=1, lty=2)

df_insample = df[1:76]


fig <- plot_ly() %>%
  # In-sample data
  add_lines(data = df[1:76,], x = ~Datetime, y = ~Daging, 
            name = 'In-Sample', line = list(color = 'blue')) %>%
  add_markers(data = df[1:76,], x = ~Datetime, y = ~Daging, 
              name = 'In-Sample Points', marker = list(color = 'red', size = 5)) %>%
  # Out-of-sample data
  add_lines(data = df[77:86,], x = ~Datetime, y = ~Daging, 
            name = 'Out-Sample', line = list(color = 'green', dash = 'dash')) %>%
  add_markers(data = df[77:86,], x = ~Datetime, y = ~Daging, 
              name = 'Out-Sample Points', marker = list(color = 'orange', size = 5)) %>%
  layout(title = "Harga Daging Jakarta: In-Sample dan Out-Sample",
         xaxis = list(title = "Waktu"),
         yaxis = list(title = "Harga Daging"),
         shapes = list(
           list(type = 'line', 
                x0 = "2021-03-29", x1 = "2021-03-29", 
                y0 = min(Daging),
                y1 = max(Daging),
                line = list(color = 'black', width = 2, dash = 'dash'))
         )) %>%
  add_annotations(x = "2021-03-29", y = Daging[66], text = "2021-03-29\n(t=66)", 
                  showarrow = TRUE, arrowhead = 2, ax = 20, ay = -30)

fig

df$Datetime

adf_test = adf.test(Daging[1:76])
summary(adf_test)

adf_test2 = adf.test(diff(Daging[1:76], 1))
print(adf_test2)
Daging_diff = diff(Daging[1:76])

par(mfrow=c(1,2))
acf(Daging_diff)
pacf(Daging_diff)
plot(Daging_diff, main = "Harga Daging Jakarta", ylab = "Price", xlab = "Time")

fit = auto.arima(start.p = 0, start.q = 0, max.p = 10, max.d = 10, max.q = 10, Daging, seasonal = FALSE)  
summary(fit)
coeftest(fit)

aic_results <- data.frame(p = integer(), d = integer(), q = integer(), AIC = numeric())

# Set the maximum values for p, d, and q
max_p <- 10
max_d <- 1  # Typically d is set to 0 or 1
max_q <- 10

# Loop over all combinations of p, d, and q
for (p in 0:max_p) {
  for (d in 0:max_d) {
    for (q in 0:max_q) {
      
      fit = tryCatch({
        arima(Daging, order = c(p, d, q))
      }, error = function(e) {
        return(NULL)
      })
      
      if (!is.null(fit)) {
        aic_results = rbind(aic_results, data.frame(p = p, d = d, q = q, AIC = fit$aic))
      }
    }
  }
}


print(aic_results)


best_model = aic_results[which.min(aic_results$AIC), ]
print(best_model)



checkresiduals(fit)

print(forecast_values)
forecast_values <- forecast::forecast(fit, h = 10)  # Forecast for 12 weeks ahead (about 3 months)
plot(forecast_values, main = "Harga Daging Jakarta Forecast", ylab = "Price", xlab = "Time")

forecast_values$mean
ks.test(fit$residuals, "pnorm",
        mean(fit$residuals),
        sd(fit$residuals))


#### Intervensi


ggplot(data = df, aes(x = Datetime)) + 
  geom_line(aes(y = Daging))

Daging = ts(df$Daging, frequency = 1)
plot(Daging, main = "Harga Daging Jakarta", ylab = "Price", xlab = "Time")
abline(v=66, col=1, lty=2)

ts_intervensi = ts(df$Daging[1:66], frequency = 1)
fig = plot_ly(y = ts_intervensi, type = 'scatter', mode = 'lines', 
               line = list(color = 'blue')) %>%
  layout(title = "Harga Daging Jakarta Sebelum Intervensi",
         xaxis = list(title = "Time"),
         yaxis = list(title = "Harga Daging"))

fig

t = 1:66
trend_daging = glm(ts_intervensi~t)
abline(trend_daging, col="dark blue", lwd=2)

lambda = BoxCox.lambda(ts_intervensi); lambda
#ts_intervensi = BoxCox(ts_intervensi, lambda)
#BoxCox.lambda(ts_intervensi)
plot(ts_intervensi, main = "Harga Daging Jakarta Sebelum Intervensi", ylab = "Price", xlab = "Time")


adf_test = adf.test(ts_intervensi)
print(adf_test)

adf_test2 = adf.test(diff(ts_intervensi, 1))
print(adf_test2)
ts_intervensi_diff = diff(ts_intervensi)

par(mfrow=c(1,2))
acf(ts_intervensi_diff)
pacf(ts_intervensi_diff)
plot(ts_intervensi_diff, main = "Harga Daging Jakarta Sebelum Intervensi", ylab = "Price", xlab = "Time")

fit = auto.arima(start.p = 2, start.q = 0, max.p = 2, d = 1, max.q = 0, ts_intervensi, seasonal = FALSE)  
fit = arima(ts_intervensi, order = c(2,1,0))
summary(fit)

max_p <- 2
max_d <- 1  # Typically d is set to 0 or 1
max_q <- 1

aic_results_3 <- data.frame(p = integer(), d = integer(), q = integer(), AIC = numeric())
# Loop over all combinations of p, d, and q
for (p in 0:max_p) {
  for (d in 0:max_d) {
    for (q in 0:max_q) {
      # Fit the ARIMA model
      fit_3 <- tryCatch({
        arima(ts_intervensi, order = c(p, d, q))
      }, error = function(e) {
        return(NULL)  # If fitting fails, return NULL
      })
      
      # If the fit was successful, store the AIC
      if (!is.null(fit)) {
        aic_results_3 <- rbind(aic_results_3, data.frame(p = p, d = d, q = q, AIC = fit_3$aic))
      }
    }
  }
}

# Show all AIC values
print(aic_results_3)

arch.test(fit)
Box.test(fit$residuals, lag = 10, type = "Ljung-Box")
ks.test(fit$residuals, "pnorm",
        mean(fit$residuals),
        sd(fit$residuals))

length(df$Daging)-66
forecast = forecast::forecast(fit, h = 20); forecast
forecast$fitted[67:76]
length(forecast$mean[1:10])

error_idintv <-rep(0,76)
error_idintv[1:66] <- fit$residuals
error_idintv[67:76] <- df$Daging[67:76] - forecast$mean[1:10]
plot(error_idintv, type="h", xlab="Waktu (T)", ylab = "Residual", xaxt = "n")
abline(h=c(-3*sd(fit$residuals), 3*sd(fit$residuals)),col="blue", lty=2)
abline(v = 67, col = "red", lty = 3, lwd = 1.5)

acf(error_idintv, 20, xlim=c(1,20))
pacf(error_idintv, 20)
ccf(error_idintv, raw_data, lag.max = 12, type="correlation", ylab='CCF')


plot(forecast, main =NA, ylab="Daging", ylim = c(90, 120))
points(df$Daging, cex=.5, col="dark red", pch=19)
lines(df$Daging, col="red")
abline(v=66, lty=2)
text(66, df$Daging[66]+3, "t=66", pos=2)
legend("topleft", legend=c("Daging", "Peramalan Daging"), cex=0.75, lty=1,
       col=c("dark red", "blue"), pch=c(19,NA))


intervention_step = 1 * (seq_along(Daging[1:76]) >= 66)  
fit_arima_intervention = Arima(Daging[1:76], lambda = NULL,
                                order = c(2,1,0), 
                                include.constant = TRUE,
                                xreg = intervention_step)

Box.test(fit_arima_intervention$residuals, lag = 10, type = "Ljung-Box")
ks.test(fit_arima_intervention$residuals, "pnorm",
        mean(fit_arima_intervention$residuals),
        sd(fit_arima_intervention$residuals))

future_intervention = rep(1, 20)  
forecast_results = forecast::forecast(fit_arima_intervention, xreg = future_intervention, h=20)$mean
plot(forecast_results, main = "Intervention Analysis with Step Function")


plot(df$Daging, main =NA, ylab="Daging", ylim = c(90, 120))
lines(1:76, fit_arima_intervention$fitted_values, col = "blue", lty = 1)
lines(fit_arima_intervention$fitted, col="red")
lines(77:86, forecast_results, col = "blue", lty = 2)
points(1:86, df$Daging, pch = 19, col = "dark red")
abline(v = 66, lty = 2, col = "black")
text(66, df$Daging[66]+3, "t=66 (Intervention)", pos = 2)
legend("topleft", legend = c("Actual Data", "Fitted Values (In-Sample)", 
                             "Forecasted Values (Out-Sample)"),
       col = c("dark red", "blue", "blue"), lty = c(1, 1, 2), 
       pch = c(19, NA, NA), cex = 0.8)

p = plot_ly(x = 1:86, y = df$Daging, type = 'scatter', mode = 'lines+markers', 
             line = list(color = 'red'), marker = list(color = 'dark red'), 
             name = 'Actual Data') %>%
  
  add_lines(x = 1:76, y = fit_arima_intervention$fitted, 
            line = list(color = 'blue', dash = 'solid'), name = 'Fitted Values (In-Sample)') %>%
  
  add_lines(x = 77:86, y = forecast_results, 
            line = list(color = 'blue', dash = 'dash'), name = 'Forecasted Values (Out-Sample)') %>%
  
  add_lines(x = c(66, 66), y = c(90, 120), 
            line = list(color = 'black', dash = 'dash'), name = 't=66 (Intervention)') %>%
  
  layout(title = '', 
         xaxis = list(title = 'Time'), 
         yaxis = list(title = 'Daging', range = c(90, 120)), 
         legend = list(orientation = 'h', y = -0.2))

p

coeftest(fit_arima_intervention)
forecast::forecast(fit_arima_intervention, xreg = future_intervention, h=20)$mean

df[66,]



