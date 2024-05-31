library(Mcomp)
library(ggplot2)



M3[[1911]]
cw_data <- M3[[1911]]
summary(cw_data)


historical_time_series <- cw_data$x
cw_data$xx


# Historical time series data
historical_time_series <- cw_data$x

# Summary statistics
summary_stats <- summary(historical_time_series)

#Specific values
min_production <- min(historical_time_series)
max_production <- max(historical_time_series)
median_production <- median(historical_time_series)
iqr_range <- IQR(historical_time_series)
variability <- sd(historical_time_series)  # Standard Deviation as a measure of variability

# Results
cat("Minimum Production:", min_production, "\n")
cat("Maximum Production:", max_production, "\n")
cat("Median Production:", median_production, "\n")
cat("IQR Range:", iqr_range, "\n")
cat("Variability (Standard Deviation):", variability, "\n")

# Average monthly production
average_production <- mean(historical_time_series)

# Result
cat("Average Monthly Production:", average_production, "\n")

# Converting the data to a time series object

# Time series plot
plot(historical_time_series, main = "Historical Data Time Series Plot", ylab = "Value",xlab="Time")
boxplot(historical_time_series, main = "Boxplot of Historical Data",ylab="Value")


library(ggfortify)
autoplot(stl(historical_time_series, s.window = "periodic")) +
  labs(title = "Seasonal Decomposition Plot")

library(tseries)
library(urca)
ur.df(historical_time_series, type = "trend", lags = 5)
adf.test(historical_time_series)

tsdisplay(historical_time_series)
logy <- log(historical_time_series)

# ACF (Autocorrelation Function) plot
acf(historical_time_series, lag.max = 36, main = "Autocorrelation Function (ACF)")

# PACF (Partial Autocorrelation Function) plot
pacf(historical_time_series, lag.max = 36, main = "Partial Autocorrelation Function (PACF)")

#Shapiro Wilk test
shapiro.test(historical_time_series)

# Creating a time variable
time_variable <- time(historical_time_series)

# Install and load the forecast package if not already installedinstall.packages("forecast")
library(forecast)

# Fit a linear regression model using tslm
model <- tslm(historical_time_series ~ trend + season, data = data.frame(historical_time_series))


summary(model)

qqnorm(residuals)
qqline(residuals)



library(lmtest)

bptest(model)

residuals <- residuals(model)
plot(fitted(model), residuals, xlab="Fitted Values", ylab="Residuals", 
     main="Residuals vs. Fitted Values")

dwtest(model)

shapiro.test(residuals)


forecast_values <- forecast(model, h = 18, level = c(80,90,95,99))
forecast_values
plot(forecast_values)




#ETS

library(forecast)

# Assuming the time series is stored in a variable named historical_time_series
ts_data <- data.frame(historical_time_series)

# Fit an ETS model
ets_model <- ets(historical_time_series)

fit1 <- ets(historical_time_series, model="ANN")
summary(fit1)

fit2 <- ets(historical_time_series, model="AAN", damped=FALSE)
summary(fit2)

fit3 <- ets(historical_time_series, model="AAN", damped=TRUE)
summary(fit3)

fit4 <- ets(historical_time_series, model="AAA", damped=TRUE)
summary(fit4)

fit5 <- ets(historical_time_series, model="MAM", damped=TRUE)
summary(fit5)

# Model summary
summary(ets_model)

# Forecast future values
forecast_values_ets <- forecast(ets_model, h = 18, level = c(80, 90, 95, 99))
print(forecast_values_ets)

plot(forecast_values_ets)

# Extracting residuals from the ETS model
ets_residuals <- residuals(ets_model)
plot(ets_residuals, type = "l", main = "Residuals of ETS Model", ylab = "Residuals", xlab = "Time")
abline(h =0,col="red")


# Normality
qqnorm(ets_residuals)
qqline(ets_residuals)

# Histogram of residuals
hist(ets_residuals, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")






#ARIMA

# Defining the vectors for p, d, q, P, D, Q for the model combinations
p_values <- c(0, 0, 0)  # Non-seasonal AR order
d_values <- c(1, 1, 1)  # Differencing order
q_values <- c(0, 0, 0)  # Non-seasonal MA order
P_values <- c(3, 2, 3)  # Seasonal AR order
D_values <- c(1, 1, 1)  # Seasonal differencing order
Q_values <- c(2, 0, 0)  # Seasonal MA order

# An empty list to store AIC values
aic_values <- numeric(length(p_values))


for (i in 1:length(p_values)) {
  model <- arima(historical_time_series, order = c(p_values[i], d_values[i], q_values[i]), 
                 seasonal = list(order = c(P_values[i], D_values[i], Q_values[i]), period = 12))
  
 
  aic_values[i] <- AIC(model)
  
  
  cat("Model Summary for Combination", i, ":\n")
  print(summary(model))
  cat("\n")
}

# AIC values for each model
cat("AIC values for each model:\n")
print(aic_values)

library(forecast)
# Forecast future values with ARIMA
forecast_values_arima <- forecast(model, h = 18, level = c(80, 90, 95, 99))


print(forecast_values_arima)


plot(forecast_values_arima)

# Extracting residuals from the ARIMA model
arima_residuals <- residuals(arima_model)

# Residual Analysis

plot(arima_residuals, type = "l", main = "Residuals of ARIMA Model", ylab = "Residuals", xlab = "Time")
abline(h =0,col="red")

tsdisplay(residuals(arima_model))
# ACF
acf(arima_residuals, main = "ACF of Residuals")

# PACF
pacf(arima_residuals, main = "PACF of Residuals")

# Histogram and density plot of residuals
hist(arima_residuals, main = "Histogram of Residuals", col = "lightblue", border = "black")
lines(density(arima_residuals), col = "red")

# Q-Q plot of residuals
qqnorm(arima_residuals, main = "Q-Q Plot of Residuals")
qqline(arima_residuals, col = 2)

# Ljung-Box test for autocorrelation in residuals
Box.test(arima_residuals, lag = 20, type = "Ljung-Box")

shapiro.test(arima_residuals)

checkresiduals(arima_model)








#Batch forecasting

library(forecast)
library(Mcomp)

tsset <- seq(1510, 2500, 10)
y <- M3[tsset]
print(y)

horizon <- 18

mape_matrix <- matrix(0, nrow = length(tsset), ncol = 1)
mae_matrix <- matrix(0, nrow = length(tsset), ncol = 1)
rmse_matrix <- matrix(0, nrow = length(tsset),ncol=1)

freq_best <- array(0, 4)

#ETS
for (tsi in seq_along(tsset)) {
  series <- y[[tsi]]$x
  
  # Splitting the series into training and validation data
  yt <- head(series, length(series))
  
  # Fitting ETS model
  ets_model <- ets(yt)
  
  # Generating forecasts for the specified horizon
  forecast_values <- forecast(ets_model, h = horizon)$mean
  
 
  cat(sprintf("Forecast for series %d:\n", tsset[tsi]))
  print(forecast_values)
}

mape_matrix4 <- matrix(0, nrow = length(tsset), ncol = 1)
mae_matrix4 <- matrix(0, nrow = length(tsset), ncol = 1)
rmse_matrix4 <- matrix(0, nrow = length(tsset), ncol = 1)


for (tsi in seq_along(tsset)) {
  series <- y[[tsi]]$x
  
  
  yt <- head(series, length(series))
  actual_values <- y[[tsi]]$xx
  
  
  ets_model <- ets(yt)
  
  forecast_values <- forecast(ets_model, h = horizon)$mean
  
  mape <- 100 * mean(abs(actual_values - forecast_values) / abs(actual_values), na.rm = TRUE)
  mae <- mean(abs(actual_values - forecast_values), na.rm = TRUE)
  rmse <- sqrt(mean((actual_values - forecast_values)^2, na.rm = TRUE))
  
  mape_matrix4[tsi, ] <- mape
  mae_matrix4[tsi, ] <- mae
  rmse_matrix4[tsi, ] <- rmse
  
  cat(sprintf("Forecast for series %d:\n", tsset[tsi]))
  print(forecast_values)
  cat(sprintf("MAPE: %.2f%%\n", mape))
  cat(sprintf("MAE: %.2f\n", mae))
  cat(sprintf("RMSE: %.2f\n", rmse))
}

avg_mape <- mean(mape_matrix4, na.rm = TRUE)
avg_mae <- mean(mae_matrix4, na.rm = TRUE)
avg_rmse <- mean(rmse_matrix4, na.rm = TRUE)

cat("MAPE Matrix:\n")
print(mape_matrix4)

cat("MAE Matrix:\n")
print(mae_matrix4)

cat("RMSE Matrix:\n")
print(rmse_matrix4)

cat(sprintf("Average MAPE: %.2f%%\n", avg_mape))
cat(sprintf("Average MAE: %.2f\n", avg_mae))
cat(sprintf("Average RMSE: %.2f\n",avg_rmse))









# Batch forecasting using ARIMA model

# Initializing matrices to store evaluation metrics
mape_matrix_arima <- matrix(0, nrow = length(tsset), ncol = 1)
mae_matrix_arima <- matrix(0, nrow = length(tsset), ncol = 1)
rmse_matrix_arima <- matrix(0, nrow = length(tsset), ncol = 1)


for (tsi in seq_along(tsset)) {
  series <- y[[tsi]]$x
  
  # Splitting the series into training and validation data
  yt <- head(series, length(series))
  actual_values <- y[[tsi]]$xx
  
  # Fitting ARIMA model
  arima_model <- auto.arima(yt)
  

  forecast_values_arima <- forecast(arima_model, h = horizon)$mean
  
  # Evaluation metrics
  mape_arima <- 100 * mean(abs(actual_values - forecast_values_arima) / abs(actual_values), na.rm = TRUE)
  mae_arima <- mean(abs(actual_values - forecast_values_arima), na.rm = TRUE)
  rmse_arima <- sqrt(mean((actual_values - forecast_values_arima)^2, na.rm = TRUE))
  

  mape_matrix_arima[tsi, ] <- mape_arima
  mae_matrix_arima[tsi, ] <- mae_arima
  rmse_matrix_arima[tsi, ] <- rmse_arima
  
  # Print results for each time series
  cat(sprintf("Forecast for series %d:\n", tsset[tsi]))
  print(forecast_values_arima)
  cat(sprintf("MAPE: %.2f%%\n", mape_arima))
  cat(sprintf("MAE: %.2f\n", mae_arima))
  cat(sprintf("RMSE: %.2f\n", rmse_arima))
}

# Average metrics
avg_mape_arima <- mean(mape_matrix_arima, na.rm = TRUE)
avg_mae_arima <- mean(mae_matrix_arima, na.rm = TRUE)
avg_rmse_arima <- mean(rmse_matrix_arima, na.rm = TRUE)

cat("MAPE Matrix (ARIMA):\n")
print(mape_matrix_arima)

cat("MAE Matrix (ARIMA):\n")
print(mae_matrix_arima)

cat("RMSE Matrix (ARIMA):\n")
print(rmse_matrix_arima)

cat(sprintf("Average MAPE (ARIMA): %.2f%%\n", avg_mape_arima))
cat(sprintf("Average MAE (ARIMA): %.2f\n", avg_mae_arima))
cat(sprintf("Average RMSE (ARIMA): %.2f\n", avg_rmse_arima))



#ETS or ARIMA
for (tsi in seq_along(tsset)) {
  
  
  series <- y[[tsi]]$x
  yt <- head(series, length(series) - horizon)
  yv <- tail(series, horizon)
  
  future_data <- y[[tsi]]$xx
  
  FCs <- array(0, c(2, horizon))
  MAPEs <- numeric(2)
  
  
  for (m in 1:2){
    if (m==1){
      fit <- ets(yt)
    } else {
      fit <- auto.arima(yt)
    }
    FCs[m,] <- forecast(fit, h = horizon)$mean
    
    MAPEs <- array(0, 2)
    for (m in 1:2){
      MAPEs[m] <- 100 * mean(abs(yv - FCs[m,])/abs(yv))
    }
  }
  
  
  best_model <- which.min(MAPEs)
  freq_best[best_model] <- freq_best[best_model] + 1
  
  cat(sprintf("For series %d, selected model: %s\n", tsset[tsi], ifelse(best_model == 1, "ETS", "ARIMA")))
  
  if (best_model == 1) {
    fit_out <- ets(yt)
  } else {
    fit_out <- auto.arima(yt)
  }
  forecast_out <- forecast(fit_out, h = horizon)$mean
  
 
  
  actual_values <- y[[tsi]]$x
  
  forecast_values <- forecast_out
  
  
  mape_matrix[tsi, ] <- 100 * mean(abs(actual_values - forecast_values) / abs(actual_values), na.rm = TRUE)
  mae_matrix[tsi, ] <- mean(abs(actual_values - forecast_values), na.rm = TRUE)
  rmse_matrix[tsi, ] <- sqrt(mean((actual_values - forecast_values)^2, na.rm =TRUE))
  
}

cat("MAPE Matrix:\n")
print(mape_matrix)

cat("MAE Matrix:\n")
print(mae_matrix)

cat("RMSE Matrix:\n")
print(rmse_matrix)

avg_mape <- mean(mape_matrix, na.rm = TRUE)
avg_mae <- mean(mae_matrix, na.rm = TRUE)
avg_rmse <- mean(rmse_matrix, na.rm = TRUE)

print(paste("Average MAPE:", avg_mape))
print(paste("Average MAE:", avg_mae))
print(paste("Average RMSE:",avg_rmse))





  


# Naive Forecasting Approach


mape_matrix_naive <- matrix(0, nrow = length(tsset), ncol = 1)
mae_matrix_naive <- matrix(0, nrow = length(tsset), ncol = 1)
rmse_matrix_naive <- matrix(0, nrow = length(tsset), ncol = 1)


for (tsi in seq_along(tsset)) {
  series <- y[[tsi]]$x
  
  
  yt <- head(series, length(series))
  actual_values <- y[[tsi]]$xx
  
  
  forecast_values_naive <- rep(tail(yt, 1), horizon)
  
  
  mape_naive <- 100 * mean(abs(actual_values - forecast_values_naive) / abs(actual_values), na.rm = TRUE)
  mae_naive <- mean(abs(actual_values - forecast_values_naive), na.rm = TRUE)
  rmse_naive <- sqrt(mean((actual_values - forecast_values_naive)^2, na.rm = TRUE))
  
  
  mape_matrix_naive[tsi, ] <- mape_naive
  mae_matrix_naive[tsi, ] <- mae_naive
  rmse_matrix_naive[tsi, ] <- rmse_naive
  
  
  cat(sprintf("Forecast for series %d (Naive Forecasting):\n", tsset[tsi]))
  print(forecast_values_naive)
  cat(sprintf("MAPE (Naive): %.2f%%\n", mape_naive))
  cat(sprintf("MAE (Naive): %.2f\n", mae_naive))
  cat(sprintf("RMSE (Naive): %.2f\n", rmse_naive))
}


avg_mape_naive <- mean(mape_matrix_naive, na.rm = TRUE)
avg_mae_naive <- mean(mae_matrix_naive, na.rm = TRUE)
avg_rmse_naive <- mean(rmse_matrix_naive, na.rm = TRUE)


cat("MAPE Matrix (Naive):\n")
print(mape_matrix_naive)

cat("MAE Matrix (Naive):\n")
print(mae_matrix_naive)

cat("RMSE Matrix (Naive):\n")
print(rmse_matrix_naive)

cat(sprintf("Average MAPE (Naive): %.2f%%\n", avg_mape_naive))
cat(sprintf("Average MAE (Naive): %.2f\n", avg_mae_naive))
cat(sprintf("Average RMSE (Naive): %.2f\n", avg_rmse_naive))






# Mean Forecasting Approach


mape_matrix_mean <- matrix(0, nrow = length(tsset), ncol = 1)
mae_matrix_mean <- matrix(0, nrow = length(tsset), ncol = 1)
rmse_matrix_mean <- matrix(0, nrow = length(tsset), ncol = 1)


for (tsi in seq_along(tsset)) {
  series <- y[[tsi]]$x
  
  
  yt <- head(series, length(series))
  actual_values <- y[[tsi]]$xx
  
  
  mean_forecast <- rep(mean(yt, na.rm = TRUE), horizon)
  
  
  mape_mean <- 100 * mean(abs(actual_values - mean_forecast) / abs(actual_values), na.rm = TRUE)
  mae_mean <- mean(abs(actual_values - mean_forecast), na.rm = TRUE)
  rmse_mean <- sqrt(mean((actual_values - mean_forecast)^2, na.rm = TRUE))
  
  
  mape_matrix_mean[tsi, ] <- mape_mean
  mae_matrix_mean[tsi, ] <- mae_mean
  rmse_matrix_mean[tsi, ] <- rmse_mean
  
  
  cat(sprintf("Forecast for series %d (Mean Forecasting):\n", tsset[tsi]))
  print(mean_forecast)
  cat(sprintf("MAPE (Mean): %.2f%%\n", mape_mean))
  cat(sprintf("MAE (Mean): %.2f\n", mae_mean))
  cat(sprintf("RMSE (Mean): %.2f\n", rmse_mean))
}


avg_mape_mean <- mean(mape_matrix_mean, na.rm = TRUE)
avg_mae_mean <- mean(mae_matrix_mean, na.rm = TRUE)
avg_rmse_mean <- mean(rmse_matrix_mean, na.rm = TRUE)

# Print overall results for Mean Forecasting
cat("MAPE Matrix (Mean Forecasting):\n")
print(mape_matrix_mean)

cat("MAE Matrix (Mean Forecasting):\n")
print(mae_matrix_mean)

cat("RMSE Matrix (Mean Forecasting):\n")
print(rmse_matrix_mean)

cat(sprintf("Average MAPE (Mean Forecasting): %.2f%%\n", avg_mape_mean))
cat(sprintf("Average MAE (Mean Forecasting): %.2f\n", avg_mae_mean))
cat(sprintf("Average RMSE (Mean Forecasting): %.2f\n", avg_rmse_mean))







mape_matrix_hw <- matrix(0, nrow = length(tsset), ncol = 1)
mae_matrix_hw <- matrix(0, nrow = length(tsset), ncol = 1)
rmse_matrix_hw <- matrix(0, nrow = length(tsset), ncol = 1)


for (tsi in seq_along(tsset)) {
  series <- y[[tsi]]$x
  
  
  yt <- head(series, length(series))
  actual_values <- y[[tsi]]$xx
  
  tryCatch({
   
    hw_model <- HoltWinters(yt, seasonal = "multiplicative")
    
   
    forecast_values_hw <- forecast(hw_model, h = horizon)$mean
    
    
    mape_hw <- 100 * mean(abs(actual_values - forecast_values_hw) / abs(actual_values), na.rm = TRUE)
    mae_hw <- mean(abs(actual_values - forecast_values_hw), na.rm = TRUE)
    rmse_hw <- sqrt(mean((actual_values - forecast_values_hw)^2, na.rm = TRUE))
    
    
    mape_matrix_hw[tsi, ] <- mape_hw
    mae_matrix_hw[tsi, ] <- mae_hw
    rmse_matrix_hw[tsi, ] <- rmse_hw
    
    
    cat(sprintf("Forecast for series %d (Holt-Winters Forecasting):\n", tsset[tsi]))
    print(forecast_values_hw)
    cat(sprintf("MAPE (Holt-Winters): %.2f%%\n", mape_hw))
    cat(sprintf("MAE (Holt-Winters): %.2f\n", mae_hw))
    cat(sprintf("RMSE (Holt-Winters): %.2f\n", rmse_hw))
  }, error = function(e) {
    
    cat(sprintf("Optimization difficulties encountered. Fallback to ARIMA for series %d.\n", tsset[tsi]))
    
    
    arima_model <- auto.arima(yt)
    
    
    forecast_values_arima <- forecast(arima_model, h = horizon)$mean
    
    
    mape_arima <- 100 * mean(abs(actual_values - forecast_values_arima) / abs(actual_values), na.rm = TRUE)
    mae_arima <- mean(abs(actual_values - forecast_values_arima), na.rm = TRUE)
    rmse_arima <- sqrt(mean((actual_values - forecast_values_arima)^2, na.rm = TRUE))
    
    
    mape_matrix_hw[tsi, ] <- mape_arima
    mae_matrix_hw[tsi, ] <- mae_arima
    rmse_matrix_hw[tsi, ] <- rmse_arima
    
    
    cat(sprintf("Forecast for series %d (ARIMA):\n", tsset[tsi]))
    print(forecast_values_arima)
    cat(sprintf("MAPE (ARIMA): %.2f%%\n", mape_arima))
    cat(sprintf("MAE (ARIMA): %.2f\n", mae_arima))
    cat(sprintf("RMSE (ARIMA): %.2f\n", rmse_arima))
  })
}


avg_mape_hw <- mean(mape_matrix_hw, na.rm = TRUE)
avg_mae_hw <- mean(mae_matrix_hw, na.rm = TRUE)
avg_rmse_hw <- mean(rmse_matrix_hw, na.rm = TRUE)


cat("MAPE Matrix (Holt-Winters Forecasting):\n")
print(mape_matrix_hw)

cat("MAE Matrix (Holt-Winters Forecasting):\n")
print(mae_matrix_hw)

cat("RMSE Matrix (Holt-Winters Forecasting):\n")
print(rmse_matrix_hw)

cat(sprintf("Average MAPE (Holt-Winters): %.2f%%\n", avg_mape_hw))
cat(sprintf("Average MAE (Holt-Winters): %.2f\n", avg_mae_hw))
cat(sprintf("Average RMSE (Holt-Winters): %.2f\n", avg_rmse_hw))


