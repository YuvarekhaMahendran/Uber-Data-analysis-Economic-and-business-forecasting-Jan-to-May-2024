library(fpp2)
library(ggplot2)
library(readxl)
library(urca)
library(forecast)

UBER_Historical_Data <- read_excel("Downloads/UBER Historical Data.xls")
View(UBER_Historical_Data)
UBER_Historical_Data
length(UBER_Historical_Data)#7
nrow(UBER_Historical_Data)#1237
summary(UBER_Historical_Data)

#convert date into TS
tsn = ts(UBER_Historical_Data[,"Date"], start =c(2019, 5), frequency=12) 
tsn 
summary(tsn)
autoplot(tsn)

myts_p =ts(UBER_Historical_Data, start=c(2019, 5), end=c(2024, 4), frequency=12)#annual data
myts_p
length(myts_p)#420
summary(myts_p)
plot(myts_p)

myts_u =ts(UBER_Historical_Data, start=c(2019, 5), end=c(2024, 4), frequency=365)#daily data
myts_u
length(myts_u)#12775
summary(myts_u)
#plotting the data
plot(myts_u)

univariate_ts = myts_u[, "Price"]
univariate_ts
autoplot(univariate_ts)
ggtsdisplay(univariate_ts)
#So, taking raw data would be an optimal solution.

univariate_ts1=myts_p[,"Price"]
univariate_ts1
autoplot(univariate_ts1)
ggtsdisplay(univariate_ts1)
#There is no presence of white noise.

#daily data:
summary(univariate_ts)
length(univariate_ts)#1825
1825*0.8=1460

traindataAR = head(univariate_ts,1460)
summary(traindataAR)


testdataAR = tail(univariate_ts,365)
summary(testdataAR)


#ARIMA
#considering for frequency=365(daily data)
fit1 = Arima(univariate_ts,order = c(1,0,0))
summary(fit1)#6656.99
fit2 = Arima(univariate_ts,order = c(1,0,1))
summary(fit2)#6658.97worse
fit3 = Arima(univariate_ts,order = c(1,1,1))
summary(fit3)#6652.66
fit4 = Arima(univariate_ts,order = c(0,1,0))
summary(fit4)#6648.73-we consider as best model till now let's check for closer models
fit5 = Arima(univariate_ts,order = c(0,1,1))
summary(fit5)#6650.65-worse
#all other models fits very worse than fit4 model. So, we consider fit4 to be the best model. 
checkresiduals(fit4)
#We check residuals in which we can partially accept that there is no presence of white noise and and since p-value is greater than 0.05, null hypothesis is not rejected.
auto.arima(univariate_ts)
#We can notice the presence of white noise partially and auto.arima picked (0,1,0) model.
#we apply auto.arima to cross verify to compare with actual arima fit models.
auto = auto.arima(univariate_ts)
summary(auto)
#Series: univariate_ts 
#ARIMA(0,1,0) 

#sigma^2 = 2.239:  log likelihood = -3323.36
#AIC=6648.73   AICc=6648.73   BIC=6654.23

#Training set error measures:
#  ME     RMSE       MAE        MPE     MAPE       MASE        ACF1
#Training set -0.02005767 1.496018 0.9092903 -0.1037903 2.414944 0.04772201 0.006286273

#forecasting out previous models
fauto = forecast(univariate_ts,h=12)
autoplot(fauto)
summary(fauto)
#The forecast seems to be partially reasonable. 

trainArima=arima(traindataAR, order = c(0,1,0))  # Replace p, d, q with appropriate ARIMA order
forecastResult=forecast(trainArima, h = 365)
summary(forecastResult)
accuracy(forecastResult,testdataAR)

#annual data:
summary(univariate_ts1)
length(univariate_ts1)#60
60*0.8=48

traindataAR1 = head(univariate_ts1,48)
summary(traindataAR1)


testdataAR1 = tail(univariate_ts1,12)
summary(testdataAR1)

#considering for frequency=12
fit_1 = Arima(univariate_ts1,order = c(1,0,0))
summary(fit_1)#248.58
fit_2 = Arima(univariate_ts1,order = c(1,0,1))
summary(fit_2)#249.01
fit_3 = Arima(univariate_ts1,order = c(1,1,0))
summary(fit_3)#240.67
fit_4 = Arima(univariate_ts1,order = c(0,1,0))
summary(fit_4)#239.07(best)
fit_5 = Arima(univariate_ts1,order = c(0,0,1))
summary(fit_5)#322.99
fit_6 = Arima(univariate_ts1,order = c(1,2,1))
summary(fit_6)#243.25
#all other models fits very worse than fit_4 model. So, we consider fit4 to be the best model
checkresiduals(fit_4)
#There is a presence of white noise and since p-value is greater than 0.05, null hypothesis is not rejected.
auto.arima(univariate_ts1)
#This gives fit_4 as the result with (0,1,0) model.
auto1 = auto.arima(univariate_ts1)
summary(auto1)
#Series: univariate_ts1 
#ARIMA(0,1,0) 

#sigma^2 = 3.252:  log likelihood = -118.5
#AIC=239   AICc=239.07   BIC=241.08

#Training set error measures:
#  ME    RMSE      MAE        MPE    MAPE      MASE      ACF1
#Training set -0.1839207 1.78811 1.136079 -0.2963371 1.55166 0.2582977 0.0864448


fauto1 = forecast(auto1,h=12)
autoplot(fauto1)
summary(fauto1)
#This seems to be better forecast.

trainArima1=arima(traindataAR1, order = c(0,1,0))  # Replace p, d, q with appropriate ARIMA order
forecastResult1=forecast(trainArima1, h = 365)
summary(forecastResult1)
accuracy(forecastResult1,testdataAR1)
#We tried train test split for arima but we felt accuracies on the raw data were better.

#DYNAMIC REGRESSION MODEL
#annual data:
summary(myts_p[,"Price"])
length(myts_p[,"Price"])#60
60*0.8=48

traindataAR11 = head(myts_p[,"Price"],48)
summary(traindataAR11)


testdataAR11 = tail(myts_p[,"Price"],12)
summary(testdataAR11)

myts_p =ts(UBER_Historical_Data, start=c(2019, 5), end=c(2024, 4), frequency=12)#annual data
myts_p
length(myts_p)#420
summary(myts_p)
plot(myts_p)

#Unit root test for target variable:
ur.df(myts_p[,"Price"],type = "drift") %>% summary()
#There is a drift as there is no uniform downside.
#There is a presence of unit root. So, we need to take first difference of data
ndiffs(myts_p[,"Price"])#1
#We need to apply one differencing.
nsdiffs(myts_p[,"Price"])#0
#There is no seasonal differences.


preds = cbind(
  Date = myts_p[,"Date"],
  Open = myts_p[,"Open"],
  High = myts_p[,"High"],
  Low = myts_p[,"Low"],
  Vol = myts_p[,"Vol."],
  Change = myts_p[,"Change %"]
)

mod1_f12 = auto.arima(myts_p[,"Price"],d=1,D=0,xreg = preds)
summary(mod1_f12)#113.77-AICc
autoplot(myts_p[,"Price"])+
  autolayer(mod1_f12$fitted)
#the model seems to fit better with slight changes.

mod2_f12 = tslm(Price ~ lag(Price, 12), data = myts_p)
mod2_f12
#Call:
#  tslm(formula = Price ~ lag(Price, 12), data = myts_p)

#Coefficients:
#  (Intercept)  lag(Price, 12)  
#-3.29e-14        1.00e+00

#doubt whether to include or not:
mod2Fit1 = 0
for (t in 1:12){
  mod2Fit1[t] = myts_p[t,"Price"]
}

mod2Fit1 = ts(mod2Fit1,start=2019,frequency = 12)

autoplot(myts_p[,"Price"])+
  autolayer(mod1_f12$fitted,series = "Dynamic Reg")+
  autolayer(mod2Fit1,series = "Time Series Reg")
#There is a smooth drift for dynamic regression model whereas in time series regression, we can notice an initiation of upward trend.

summary(mod1_f12)#113.77-best till now

mod2_f12 = Arima(myts_p[,"Price"],xreg = preds,order = c(0,1,1),seasonal=c(1,0,1))
summary(mod2_f12)#114.84
mod3_f12 = Arima(myts_p[,"Price"],xreg = preds,order = c(1,1,1),seasonal=c(1,0,0))
summary(mod3_f12)#114.88
mod4_f12 = Arima(myts_p[,"Price"],xreg = preds,order = c(0,1,0),seasonal=c(0,0,1))
summary(mod4_f12)#137.8
mod5_f12 = Arima(myts_p[,"Price"],xreg = preds,order = c(1,1,0),seasonal=c(0,0,1))
summary(mod5_f12)#123.55
mod6_f12 = Arima(myts_p[,"Price"],xreg = preds,order = c(0,1,2),seasonal=c(1,0,1))
summary(mod6_f12)#117.78
mod7_f12 = Arima(myts_p[,"Price"],xreg = preds,order = c(0,1,2),seasonal=c(2,0,1))
summary(mod7_f12)#116.8
mod8_f12 = Arima(myts_p[,"Price"],xreg = preds,order = c(2,1,2),seasonal=c(1,0,1))
summary(mod8_f12)#125.15
mod9_f12 = Arima(myts_p[,"Price"],xreg = preds,order = c(1,1,2),seasonal=c(2,0,1))
summary(mod9_f12)#119.89
mod10_f12 = Arima(myts_p[,"Price"],xreg = preds,order = c(1,1,0),seasonal=c(1,0,1))
summary(mod10_f12)#123.56(worst)
mod11_f12 = Arima(myts_p[,"Price"],xreg = preds,order = c(1,1,0),seasonal=c(1,0,0))
summary(mod11_f12)#123.61(worst)
#Considering all the models, we pick mod1_f12 to be the best fit.

checkresiduals(mod1_f12)
#We can notice that there is a presence of white noise. Since p-value is more than 0.05, we can say that null hypothesis can be rejected.

#Ex ante forecast:
priceETS=ets(myts_p[,"Price"])
summary(priceETS)#It has picked up M,N,N model.
#ETS(M,N,N) 

#Call:
#  ets(y = myts_p[, "Price"]) 

#Smoothing parameters:
#  alpha = 0.9999 

#Initial states:
#  l = 74.7208 

#sigma:  0.0237

#AIC     AICc      BIC 
#317.0549 317.4834 323.3379 

#Training set error measures:
#  ME     RMSE      MAE        MPE     MAPE      MASE       ACF1
#Training set -0.1845322 1.788109 1.135469 -0.2971609 1.550841 0.2581589 0.08641052

fprice_12=forecast(priceETS,h=12)
fprice_12
futPreds=cbind(
  
    Date = fprice_12$mean,
    Open = fprice_12$mean,
    High = fprice_12$mean,
    Low = fprice_12$mean ,
    Vol = fprice_12$mean,
    Change = fprice_12$mean 
)

dynFcast = forecast(mod1_f12,h=12,xreg=futPreds)
summary(dynFcast)
autoplot(dynFcast)
#We can notice the drift and forecast is showing changes.

#comparing this to a plain ARIMA model
justArima = auto.arima(myts_p[,"Price"],d=1,D=0)
summary(justArima)#It picked (0,1,0) model
#AICc=239.07
jaFcast = forecast(justArima,h=12)
summary(jaFcast)
autoplot(window(myts_p[,"Price"],start=2019))+
  autolayer(jaFcast,series = "Just ARIMA")+
  autolayer(dynFcast,series = "Dynamic Regression")
#We can see smooth drift and the forecast seems to be reasonable.

mod1_f12Fcast = forecast(mod1_f12,h=12,xreg=futPreds)
summary(mod1_f12Fcast)#113.77
autoplot(window(myts_p[,"Price"],start=2019))+
  autolayer(jaFcast,PI=F,series = "Just Arima")+
  autolayer(dynFcast,PI=F,series = "Auto Arima")+
  autolayer(mod1_f12Fcast,PI=F,series = "Our Model")
#It cannot forecast ARIMA model but our model is slightly showing changes. 

trainD1=Arima(traindataAR11, order = c(2,1,0))  # Replace p, d, q with appropriate ARIMA order
forecastResult11=forecast(trainD1, h = 12)
summary(forecastResult11)
accuracy(forecastResult11,testdataAR11)

#daily data:
summary(myts_u[,"Price"])
length(myts_u[,"Price"])#1825
1825*0.8=48

traindataAR12 = head(myts_u[,"Price"],1460)
summary(traindataAR12)


testdataAR12 = tail(myts_u[,"Price"],365)
summary(testdataAR12)

myts_u =ts(UBER_Historical_Data, start=c(2019, 5), end=c(2024, 4), frequency=365)#daily data
myts_u
length(myts_u)#12775
summary(myts_u)
plot(myts_u)

#Unit root test for target variable:
ur.df(myts_u[,"Price"],type = "drift") %>% summary()
#There is a presence of unit root in 1% significant level. When considering 5% and 10%, we can say that the test statistic values are smaller than critical values, there is no presence of unit root. 
ndiffs(myts_u[,"Price"])#1
#We need to apply one differencing.
nsdiffs(myts_u[,"Price"])#0
#There is no seasonal differences.

preds1 = cbind(
  Date = myts_u[,"Date"],
  Open = myts_u[,"Open"],
  High = myts_u[,"High"],
  Low = myts_u[,"Low"],
  Vol = myts_u[,"Vol."],
  Change = myts_u[,"Change %"]
)

mod1_f365 = auto.arima(myts_u[,"Price"],d=1,D=0,xreg = preds1)
summary(mod1_f365)#2316.28-AICc
autoplot(myts_u[,"Price"])+
  autolayer(mod1_f365$fitted)
#This seems to be fitted well.

#mod2_f365 = tslm(Price ~ lag(Price, 12), data = myts_u)
mod2_f365
#Call:
#  tslm(formula = Price ~ lag(Price, 12), data = myts_u)

#Coefficients:
#  (Intercept)  lag(Price, 12)  
#-1.784e-13       1.000e+00  


mod2Fit11 = 0
for (t in 1:12){
  mod2Fit11[t] = myts_u[t,"Price"]
}

mod2Fit11 = ts(mod2Fit11,start=2019,h = 365)

autoplot(myts_u[,"Price"])+
  autolayer(mod1_f365$fitted,series = "Dynamic Reg")+
  autolayer(mod2Fit11,series = "Time Series Reg")
#There is a cyclicity in dynamic regression model  whereas in time series regression, it exhibits a downward trend.

summary(mod1_f365)#2316.28
#(4,1,0) model.


checkresiduals(mod1_f365)
#Since p-value is less than 0.05, null hypothesis will be rejected.

#Ex ante forecast:
price1ETS=ets(myts_u[,"Price"])
summary(price1ETS)
#It can't handle data with frequency greater than 24

fprice_365=forecast(price1ETS,h=365)
fprice_365
futPreds1=cbind(
  
  Date = fprice_365$mean,
  Open = fprice_365$mean,
  High = fprice_365$mean,
  Low = fprice_365$mean ,
  Vol = fprice_365$mean,
  Change = fprice_365$mean 
)

dynFcast1 = forecast(mod1_f365,h=365,xreg=futPreds1)
summary(dynFcast1)
autoplot(dynFcast1)
#We can notice forecast to be slightly reasonable

#comparing this to a plain ARIMA model
justArima1 = auto.arima(myts_u[,"Price"],d=1,D=0)
summary(justArima1)#It picked (0,1,0) model 
#AICc=6648.73
jaFcast1 = forecast(justArima1,h=365)
summary(jaFcast1)
autoplot(window(myts_u[,"Price"],start=2019))+
  autolayer(jaFcast1,series = "Just ARIMA")+
  autolayer(dynFcast1,series = "Dynamic Regression")
#We can notice that the forecast seems to be reasonable.

mod1_f365Fcast = forecast(mod1_f365,h=365,xreg=futPreds1)
summary(mod1_f365Fcast)
autoplot(window(myts_u[,"Price"],start=2019))+
  autolayer(jaFcast,PI=F,series = "Just Arima")+
  autolayer(dynFcast,PI=F,series = "Auto Arima")+
  autolayer(mod1_f12Fcast,PI=F,series = "Our Model")
#We can notice that the model is slightly off with just ARIMA and our model in closer areas where we can't see AUTO ARIMA forecast.

trainD2=Arima(traindataAR12, order = c(4,1,0))  # Replace p, d, q with appropriate ARIMA order
forecastResult12=forecast(trainD2, h = 365)
summary(forecastResult12)
accuracy(forecastResult12,testdataAR12)
#We tried train test split for dynamic regression model but we felt accuracies on the raw data were better.