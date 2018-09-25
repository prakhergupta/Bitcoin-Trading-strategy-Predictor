#R File for Bitcoin trading strategy predictor
# By Prakher Gupta

#Read File
setwd("D:/Prakher/Desktop/IDS 575/Project - Trading Strategy/bitcoin-historical-data")
data1 <- read.csv("btceUSD_1-min_data_2012-01-01_to_2017-05-31.csv")
View(data1)

#Converting Unix time into Date and Time
data$Timestamp = as.POSIXct(data$Timestamp,origin='1970-01-01',tz='GMT')
head(data)

#dailyampling required from minutes data to Daily/Monthly/Quarterly/Annually

#Convert into Daily Data
ndata = na.omit(data)
ndata$date <- as.Date(ndata$Timestamp)
head(ndata)

#install.packages("sqldf")
#install.packages("tseries")
#install.packages("forecast")

library(sqldf)

#Calculating open,high,low,close values from the minutes data

daily <- c(sqldf("select date,open from ndata where (date,timestamp) 
                 in (select date,min(timestamp) from ndata group by date)"),
           sqldf("select date,max(High) High,min(Low) Low from ndata group by date"), 
           sqldf("select date,close from ndata where (date,timestamp) 
                 in (select date,max(timestamp) from ndata group by date)"))

#converting list into data.frame for ggplots to handle
daily <- as.data.frame(daily)
View(daily)


##Sample data
##upload <- tail(data1, 200)
##View(upload)
##install.packages("xlsx")
##library(xlsx)
##write.csv(upload, "Sample Data.csv")

#plotting ggplot for the daily graph
library(ggplot2)
ggplot(daily, aes(x=daily$date, y= daily$Open)) + geom_line() + 
  scale_x_date('month')  + 
  ylab("Bitcoin Prices") +
  xlab("")

library(tseries)
#xyz <- ts(ndata[1:100,c('Open','High','Low','Close')])
#plotOHLC(xyz,ylab="Price",main=ndata)
#head(xyz)

library(forecast)

## -----------------------------------------
##We will try to predict the market for Open values
##------------------------------------------
#Removing outliers from the market
count_ts = ts(daily[,c('Open')])
daily$clean_open = tsclean(count_ts)
ggplot() +
  geom_line(data = daily, aes(x = daily$date, y = daily$clean_open)) + 
  ylab('Cleaned Prices')

#Calculating Moving Averages
daily$Open_ma7 = ma(daily$Open,order=7) #Weekly Moving Average

daily$Open_ma30 = ma(daily$Open,order=30) #Monthly Moving Average

ggplot() +
  geom_line(data = daily, aes(x = daily$date, y = daily$Open_ma7)) + 
  ylab('Moving Average')


ggplot() +
  geom_line(data = daily, aes(x = daily$date, y = daily$clean_open, 
                              colour = "Open Values"))+
  geom_line(data = daily, aes(x = daily$date, y = daily$Open_ma7,   
                              colour = "Weekly Moving Average"))+
  geom_line(data = daily, aes(x = daily$date, y = daily$Open_ma30, 
                              colour = "Monthly Moving Average"))+
  ylab('Bitcoint Price with averages')

#Decompose data for seasonal, trend and other components
count_ma = ts(na.omit(daily$Open_ma7), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

#ADF Test for weekly moving average
adf.test(count_ma, alternative = "stationary")

acf(count_ma,main = "")
pacf(count_ma )

#ADF Test for difference of weekly moving average
count_d1 = diff(deseasonal_cnt, differences = 1)

adf.test(count_d1, alternative = "stationary")

Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

#ARIMA model for c(1,1,1)
try <- arima(deseasonal_cnt ,order = c(1,1,1))
summary(try)

#Using auto.arima to pick the best ARIMA model
auto.arima(deseasonal_cnt, seasonal=FALSE)

fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model residuals')

#Since we observe that there is high value for lag=7
fit2 = arima(count_d1, order=c(1,1,7)) 

fit2

tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model residuals')

#Forecasting the value
fcast <- forecast(fit2, h=30)
plot(fcast)
head(fcast)
View(fcast)

#Compare with actual values
hold <- window(ts(deseasonal_cnt), start=700)
tail(hold)
tail(data)

fit_no_holdout = arima(ts(deseasonal_cnt[-c(700:725)]), order=c(1,1,7))

fcast_no_holdout <- forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt),col="red")

#with Seasonality
fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality
seas_fcast <- forecast(fit_w_seasonality)
plot(seas_fcast)
lines(ts(deseasonal_cnt),col="red")
tail(seas_fcast)

##----------------------------------
##Now lets relicate this for Close, Low and High
##----------------------------------

#Removing outliers from the market
count_ts = ts(daily[,c('High')])
daily$clean_High = tsclean(count_ts)

#Calculating Moving Averages
daily$High_ma7 = ma(daily$High,order=7) #Weekly Moving Average

daily$High_ma30 = ma(daily$High,order=30) #Monthly Moving Average


ggplot() +
  geom_line(data = daily, aes(x = daily$date, y = daily$clean_High, 
                              colour = "High Values"))+
  geom_line(data = daily, aes(x = daily$date, y = daily$High_ma7,   
                              colour = "Weekly Moving Average"))+
  geom_line(data = daily, aes(x = daily$date, y = daily$High_ma30, 
                              colour = "Monthly Moving Average"))+
  ylab('Bitcoint Price with averages')

#Decompose data for seasonal, trend and other components
count_ma = ts(na.omit(daily$High_ma7), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt_High <- seasadj(decomp)
plot(decomp)

#ADF Test for weekly moving average
adf.test(count_ma, alternative = "stationary")

acf(count_ma,main = "")
pacf(count_ma )

#ADF Test for difference of weekly moving average
count_d1 = diff(deseasonal_cnt_High, differences = 1)

adf.test(count_d1, alternative = "stationary")

Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

#ARIMA model with lag=7
fitHigh = arima(count_d1, order=c(1,1,7)) 

fitHigh

tsdisplay(residuals(fitHigh), lag.max=15, main='Seasonal Model residuals')

#Forecasting the value
fcastHigh <- forecast(fitHigh, h=30)
plot(fcastHigh)
head(fcastHigh)
View(fcastHigh)

#Compare with actual values
hold <- window(ts(deseasonal_cnt_High), start=700)
tail(hold)
tail(data)

fit_no_holdout = arima(ts(deseasonal_cnt_High[-c(700:725)]), order=c(1,1,7))

fcast_no_holdout <- forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt_High),col="red")

#with Seasonality
fit_w_seasonality = auto.arima(deseasonal_cnt_High, seasonal=TRUE)
fit_w_seasonality
seas_fcast <- forecast(fit_w_seasonality)
plot(seas_fcast)
lines(ts(deseasonal_cnt_High),col="red")
tail(seas_fcast)

## LOW Values
#Removing outliers from the market
count_ts = ts(daily[,c('Low')])
daily$clean_Low = tsclean(count_ts)

#Calculating Moving Averages
daily$Low_ma7 = ma(daily$Low,order=7) #Weekly Moving Average

daily$Low_ma30 = ma(daily$Low,order=30) #Monthly Moving Average


ggplot() +
  geom_line(data = daily, aes(x = daily$date, y = daily$clean_Low, 
                              colour = "Low Values"))+
  geom_line(data = daily, aes(x = daily$date, y = daily$Low_ma7,   
                              colour = "Weekly Moving Average"))+
  geom_line(data = daily, aes(x = daily$date, y = daily$Low_ma30, 
                              colour = "Monthly Moving Average"))+
  ylab('Bitcoint Price with averages')

#Decompose data for seasonal, trend and other components
count_ma = ts(na.omit(daily$Low_ma7), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt_Low <- seasadj(decomp)
plot(decomp)

#ADF Test for weekly moving average
adf.test(count_ma, alternative = "stationary")

acf(count_ma,main = "")
pacf(count_ma )

#ADF Test for difference of weekly moving average
count_d1 = diff(deseasonal_cnt_Low, differences = 1)

adf.test(count_d1, alternative = "stationary")

Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

#ARIMA model with lag=7
fitLow = arima(count_d1, order=c(1,1,7)) 

fitLow

tsdisplay(residuals(fitLow), lag.max=15, main='Seasonal Model residuals')

#Forecasting the value
fcastLow <- forecast(fitLow, h=30)
plot(fcastLow)
head(fcastLow)
View(fcastLow)

#Compare with actual values
hold <- window(ts(deseasonal_cnt_Low), start=700)
tail(hold)
tail(data)

fit_no_holdout = arima(ts(deseasonal_cnt_Low[-c(700:725)]), order=c(1,1,7))

fcast_no_holdout <- forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt_Low),col="red")

#with Seasonality
fit_w_seasonality = auto.arima(deseasonal_cnt_Low, seasonal=TRUE)
fit_w_seasonality
seas_fcast <- forecast(fit_w_seasonality)
plot(seas_fcast)
lines(ts(deseasonal_cnt_Low),col="red")
tail(seas_fcast)


## Close Values
#Removing outliers from the market
count_ts = ts(daily[,c('Close')])
daily$clean_Close = tsclean(count_ts)

#Calculating Moving Averages
daily$Close_ma7 = ma(daily$Close,order=7) #Weekly Moving Average

daily$Close_ma30 = ma(daily$Close,order=30) #Monthly Moving Average


ggplot() +
  geom_line(data = daily, aes(x = daily$date, y = daily$clean_Close, 
                              colour = "Close Values"))+
  geom_line(data = daily, aes(x = daily$date, y = daily$Close_ma7,   
                              colour = "Weekly Moving Average"))+
  geom_line(data = daily, aes(x = daily$date, y = daily$Close_ma30, 
                              colour = "Monthly Moving Average"))+
  ylab('Bitcoint Price with averages')

#Decompose data for seasonal, trend and other components
count_ma = ts(na.omit(daily$Close_ma7), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt_Close <- seasadj(decomp)
plot(decomp)

#ADF Test for weekly moving average
adf.test(count_ma, alternative = "stationary")

acf(count_ma,main = "")
pacf(count_ma )

#ADF Test for difference of weekly moving average
count_d1 = diff(deseasonal_cnt_Close, differences = 1)

adf.test(count_d1, alternative = "stationary")

Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

#ARIMA model with lag=7
fitClose = arima(count_d1, order=c(1,1,7)) 

fitClose

tsdisplay(residuals(fitClose), lag.max=15, main='Seasonal Model residuals')

#Forecasting the value
fcastClose <- forecast(fitClose, h=30)
plot(fcastClose)
head(fcastClose)
View(fcastClose)

#Compare with actual values
hold <- window(ts(deseasonal_cnt_Close), start=700)
tail(hold)
tail(data)

fit_no_holdout = arima(ts(deseasonal_cnt_Close[-c(700:725)]), order=c(1,1,7))

fcast_no_holdout <- forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt_Close),col="red")

#with Seasonality
fit_w_seasonality = auto.arima(deseasonal_cnt_Close, seasonal=TRUE)
fit_w_seasonality
seas_fcast <- forecast(fit_w_seasonality)
plot(seas_fcast)
lines(ts(deseasonal_cnt_Close),col="red")
tail(seas_fcast)