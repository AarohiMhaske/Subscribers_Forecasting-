library(fpp2)
df<- read.csv("NFLX.csv")
View(df)
data<-ts(df$Close,start=c(2002,05,23),frequency = 365.25)
autoplot(data)+ggtitle("Netflix stock prices")
data
stationary<-diff(data)
autoplot(stationary)+ggtitle("Netflix stock prices")


###############################
# Check for monthly seasonality
ggseasonplot(diff(log(ts(df$Close, start=c(2002, 5), frequency=4, end=c(2022, 10)))))+
  ggtitle('Quarterly Seasonlity Plot')

ggseasonplot(diff(log(ts(df$Close, start=c(2002, 5), frequency=12, end=c(2022, 5)))))+
  ggtitle('Monthly Seasonlity Plot')

# No specific seasonal trend visible 
# A general dip is observed around Aug-Sep region

###############################
# Find seasonality
seasonal_part <- decompose(stationary, type = c("additive"), filter = NULL)


#PLotting the seasonal data
autoplot(seasonal_part$seasonal)+
  ggtitle('Seasonal Trend')

# Removing seasonality
stationary <- stationary - seasonal_part$seasonal


# Plot new TS
autoplot(stationary)+
  ggtitle('Closing Price of Netflix')

acf(stationary)
pacf(stationary)
pacf(stationary^2)

library (tseries)
adf.test(stationary)




# ARIMA

ARMA = arima(stationary, order=c(0,0,1))
print(ARMA)
# Plot original and predicted data
preds<-predict(ARMA, stationary, n.ahead=30)

# Residuals Analysis
checkresiduals(ARMA)
ts.plot(stationary, main='BTC Original and Predicted Data')
ARMA_fit<-stationary-residuals(ARMA)
points(ARMA_fit, type='l', col='red', lty=2)

# Prediction
prediction_ARMA<-predict(ARMA, n.ahead=30)
prediction_ARMA

autoplot(ts(prediction_ARMA$pred))

autoplot(forecast(ARMA))
