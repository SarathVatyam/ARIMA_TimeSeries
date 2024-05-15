library(forecast)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("/Users/sarathkumarvatyam/Downloads")

walmart.data <- read.csv("walmart.csv")

#TIME-SERIES.TS()
revenue <- walmart.data$Revenue
walmart.ts <- ts(walmart.data$Revenue, 
                 start = c(2005, 1), end = c(2023, 4), freq = 4)

walmart.ts

# Apply the plot() function to create a plot of the historical data
# Plotting the Time Series Data
plot(walmart.ts, 
     main = "Quarterly Revenue Time Series (2005-2023)",
     xlab = "Year",
     ylab = "Revenue",
     type = "l",  # 'l' for line plot
     col = "blue"
)


# Data Partition
nValid <- 16
nTrain <- length(walmart.ts) - nValid
train.ts <- window(walmart.ts, start = c(2005, 1), end = c(2019, 4))
valid.ts <- window(walmart.ts, start = c(2020, 1), end = c(2023, 4))
train.ts
valid.ts


#Q1A- Using Arima() function to fit AR(1) model.
walmart.ar1<- Arima(walmart.ts, order = c(1,0,0))
summary(walmart.ar1)

#Q1B-
# Create first difference of data using diff() function.
diff.walmart <- diff(walmart.ts, lag = 1)
diff.walmart

#Use Acf() function to identify autocorrealtion for first differenced shipment
Acf(diff.walmart, lag.max = 8, 
    main = " Autocorrelation plot of the first differencing (lag1) with the max of 8 lags")



#Q2a
# Use tslm() function to create linear trend and seasonal model.
train.lin.season <- tslm(train.ts ~ trend + season)
# See summary of linear trend equation and associated parameters.
summary(train.lin.season)


# Apply forecast() function to make predictions for ts with 
# linear trend and seasonal model in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred

#Q2B.

#Extract residuals for the training period
train_residuals <- residuals(train.lin.season)
train_residuals 

# Plot residuals of the predictions with trend and seasonality.

ylin <- c(-9000, 11000)
plot(train_residuals, 
     xlab = "Time", ylab = "Residuals", 
     ylim = ylin, bty = "l",
     xlim = c(2005, 2025), xaxt = "n",
     main = "Regression Residuals for Training Data", 
     col = "brown", lwd = 2) 
axis(1, at = seq(2005, 2025, 1), labels = format(seq(2005, 2025, 1)))


# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020, 2020), c(-9000, 11000))
text(2011, 10000, "Training")
arrows(2006, 3800, 2020, 3800, code = 3, length = 0.1,
       lwd = 1, angle = 30)




#ACF()plot for Train residuals
Acf(train_residuals, lag.max = 08, 
    main = "Autocorrelation for Shipment Training  Residuals")

#validation autocorrelation
Acf(valid.ts - train.lin.season.pred$mean, lag.max = 08, 
    main = "Autocorrelation for Shipment Validation Residuals")

#Q2C.
#ar1 model for regression residuals

residuals.ar1 <- Arima(train.lin.season$residuals, order = c(1,0,0))
residuals.ar1

#summary of ar1 model for regression residuals
summary(residuals.ar1)
residuals.ar1$fitted



Acf(residuals.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Shipment Training Residuals of Residuals")

# Plot residuals of the residuals for training data after AR(1).
plot(residuals.ar1$residuals, 
     xlab = "Time", ylab = "Residuals", 
     ylim = c(-6000, 7000), bty = "l",
     xlim = c(2005, 2023), xaxt = "n",
     main = "Residuals of Residuals for Training Data after AR(1)", 
     col = "brown", lwd = 2) 
axis(1, at = seq(2005, 2025, 1), labels = format(seq(2005, 2025, 1)))
# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020, 2020), c(-8000, 10000))
text(2011, 10000, "Training")
arrows(2005, 3800, 2020, 3800, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#2d
#creation of two level forecast
residuals.ar1.pred <- forecast(residuals.ar1, h = nValid, level = 0)
residuals.ar1.pred

two.level.forecast <- train.lin.season.pred$mean + residuals.ar1.pred$mean
two.level.forecast

Table.df <- round(data.frame(valid.ts, train.lin.season.pred$mean, 
                             residuals.ar1.pred$mean, two.level.forecast),3)
names(Table.df) <- c("walmart_revenue", "Regression.Forecast", 
                     "AR(1) Forecast", "Combined.Forecast")
Table.df

round(accuracy(two.level.forecast, valid.ts),3 )
round(accuracy(train.lin.season.pred$mean, valid.ts),3)

#2e
#two-level forecast for entire data set
lin.trend.season.walmart<- tslm(walmart.ts ~ trend + season)
summary(lin.trend.season.walmart)

res.ar1 <- Arima(lin.trend.season.walmart$residuals, order = c(1,0,0))
summary(res.ar1)

Acf(res.ar1$residuals, lag.max = 08, 
    main = "Autocorrelation for Residuals of Residuals for Entire Data Set")


# two level forecasting for future quarters
linear.trend.seasonality.pred <- forecast(lin.trend.season.walmart, h = 08, level = 0)
linear.trend.seasonality.pred

res.ar1.pred <- forecast(res.ar1, h = 08, level = 0)
res.ar1.pred

# Identify forecast for the future 8 periods as sum of linear trend and 
# seasonal model and AR(1) model for residuals.
linear.season.ar1.pred <- linear.trend.seasonality.pred$mean + res.ar1.pred$mean
linear.season.ar1.pred

Data_Table <- round(data.frame(linear.trend.seasonality.pred$mean, 
                               res.ar1.pred$mean, linear.season.ar1.pred),3)
names(Data_Table) <- c("Regression.Forecast", "AR(1)Forecast","Combined.Forecast")
Data_Table

#3a
# Using Arima() function to fit ARIMA(1,1,1)(1,1,1) model for trend and seasonality.
train.arima.seas <- Arima(train.ts, order = c(1,1,1), 
                          seasonal = c(1,1,1)) 
summary(train.arima.seas)

# Apply forecast() function to make predictions for ts with 
# ARIMA model in validation set.    
train.arima.seas.pred <- forecast(train.arima.seas, h = nValid, level = 0)
train.arima.seas.pred

#3b
# FIT AUTO ARIMA MODEL.
# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)


# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred   

# Using Acf() function, create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(train.auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

#3c
#comparing of arima models
round(accuracy(train.arima.seas.pred$mean, valid.ts), 3)
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)
#3d
# Using Arima() function to fit ARIMA(1,1,1)(1,1,1) model for 
# trend and seasonality for entire data set.
arima.seas <- Arima(walmart.ts, order = c(1,1,1), 
                    seasonal = c(1,1,1)) 
summary(arima.seas)

# Apply forecast() function to make predictions for ts with 
# seasonal ARIMA model for the future 8 periods. 
arima.seas.pred <- forecast(arima.seas, h = 8, level = 0)
arima.seas.pred

# Use auto.arima() function to fit ARIMA model for entire data set.
auto.arima <- auto.arima(walmart.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 8 periods. 
auto.arima.pred <- forecast(auto.arima, h = 8, level = 0)
auto.arima.pred

#3e
# MEASURE FORECAST ACCURACY FOR ENTIRE DATA SET.
round(accuracy(linear.trend.seasonality.pred$fitted, walmart.ts),3)
round(accuracy(linear.trend.seasonality.pred$fitted + res.ar1.pred$fitted, walmart.ts),3)
round(accuracy(arima.seas.pred$fitted, walmart.ts), 3)
round(accuracy(auto.arima.pred$fitted, walmart.ts), 3)
round(accuracy((snaive(walmart.ts))$fitted, walmart.ts), 3)


