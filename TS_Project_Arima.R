library(fpp)
library(ggplot2)
library(tseries)
library(forecast)
library(tseries) 
library(lubridate)

chickenpox <- read.csv("C:/Users/fm980/Desktop/time series/final project/hungary_chickenpox.csv")
chickenpox$Cases <- c(rowSums(chickenpox[,2:21]))
head(chickenpox)
country_cases <- chickenpox[c('Date', 'Cases')]
country_cases$Date <- as.Date(country_cases$Date, format = '%d/%m/%Y')
country_cases$Date <- yearmonth(country_cases$Date)
country_cases <- country_cases %>% group_by(Date) %>% summarise_all(sum)
head(country_cases)
country_cases <- data.frame(country_cases)
rownames(country_cases) <- country_cases$Date
country_cases$Date <- NULL
train <- country_cases[1:96, 1]
test <- country_cases[97:120, 1]

# ts_train <- ts(train,start = decimal_date(as.Date('2005-01-03')), frequency = 365.25/7)
ts_train <- ts(train, start = 2005, frequency = 12)
win.graph(width=30, height=20)
plot.ts(ts_train, ylab = 'Cases', main = 'Hungary')

ts_all <- ts(country_cases, start = 2005, frequency = 12)
plot.ts(ts_all, ylab = 'Cases', main = 'Hungary')

  diff_1st <- diff(ts_train)
plot.ts(diff_1st, main = '1st Order Difference')
diff_2nd <- diff(ts_train, differences = 2)
plot.ts(diff_2nd, main = '2nd Order Difference')
kpss.test(diff_1st, null='Level') # p-value of first order differcing is 0.1, greater than 0.05, thus we can reject the null hypothesis of the KPSS test and assume the ts is stationary.
kpss.test(diff_2nd, null='Level') 

# 确定p,d,q(before modeling)
# Acf(diff_1st)
# Pacf(diff_1st)

fit <- auto.arima(ts_train,seasonal = TRUE) # AICc=1370.27
fit$coef
fit 

# Compute the sample Extended ACF (EACF) 
'''
fit2 <- eacf(diff_1st) 
fit2.1 <- Arima(ts_train, order = c(0,1,1))
fit2.2 <- Arima(ts_train, order = c(0,1,2)) 
fit2.3 <- Arima(ts_train, order = c(1,1,2)) 
'''

forecast <-forecast(fit,level=c(80,95), h=24)
plot(forecast) 

actual <- test
estimate <- forecast$mean
error <- test - estimate
cbind(actual, estimate, error)
# Plot the errors
plot(error,main = 'Error')

library(Metrics)
rmse(actual, estimate)

