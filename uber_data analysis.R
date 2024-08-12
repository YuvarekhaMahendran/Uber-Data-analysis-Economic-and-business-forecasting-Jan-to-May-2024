library(fpp2)
library(ggplot2)
library(readxl)
library(urca)
library(forecast)


UBER_Historical_Data =read_excel("Downloads/UBER Historical Data.xls")
View(UBER_Historical_Data)
#There is no missing values or inconsistencies, so we proceed with this dataset.
UBER_Historical_Data
length(UBER_Historical_Data)#7
nrow(UBER_Historical_Data)#1237
summary(UBER_Historical_Data)

#convert date into TS

myts_u =ts(UBER_Historical_Data, start=c(2019, 5), end=c(2024, 4), frequency=365)#daily data
myts_u
length(myts_u)#12775
summary(myts_u)
#plotting the data
plot(myts_u)
#plotting the daily data
autoplot(myts_u,facets=T)

myts_a =ts(UBER_Historical_Data, start=c(2019, 5), end=c(2024, 4), frequency=12)#annual data
myts_a
length(myts_a)#420
summary(myts_a)
plot(myts_a)
#plotting the annual data
autoplot(myts_a,facets=T)


#daily data uni variate time series
univariate_tsd = myts_u[, "Price"]
univariate_tsd
autoplot(univariate_tsd)
#increasing and decreasing trend till 2022 and decreasing from 2023 to 2024 with small cycles and no seasonality.

#annual data  uni variate time series
univariate_tsa=myts_a[,"Price"]
univariate_tsa
autoplot(univariate_tsa)
# Increasing trend till 2022 and decreasing trend till 2024 with small cycles with no seasonality.

# Splitting into Train & test Set for daily data:
#Let's define the training set from May 2019 to February 2022 and the testing set from March 2022 to April 2024

Train_set <- window(univariate_tsd, start=c(2019, 5), end=c(2022, 2))
print(length(Train_set))#1093

# Define the testing set (March 2022 to April 2024)
Test_set <- window(univariate_tsd, start=c(2022, 3), end=c(2024, 4))
print(length(Test_set))#732 

# Splitting into Train & test Set for annual data:
Train_set1 <- window(univariate_tsa, start=c(2019, 5), end=c(2022, 2))
print(length(Train_set1))#34

# Define the testing set (March 2022 to April 2024)
Test_set1 <- window(univariate_tsa, start=c(2022, 3), end=c(2024, 4))
print(length(Test_set1))#26

#Exponential smoothing:

#Daily Data
#we can either specify alpha
sesFit1 = ses(univariate_tsd,h=12,alpha = 0.5)
sesFit2 = ses(univariate_tsd,h=12,alpha = 0.25)
sesFit3 = ses(univariate_tsd,h=12,alpha=0.75)
summary(sesFit1)
summary(sesFit2)
summary(sesFit3)

autoplot(univariate_tsd)+
  autolayer(sesFit1)+
  autolayer(sesFit2)+
  autolayer(sesFit3)
#plot looks accurate 

#using annual data
sesFit11 = ses(univariate_tsa,h=12,alpha = 0.5)
sesFit21 = ses(univariate_tsa,h=12,alpha = 0.25)
sesFit31 = ses(univariate_tsa,h=12,alpha=0.75)
summary(sesFit11)
summary(sesFit21)
summary(sesFit31)

autoplot(univariate_tsa)+
  autolayer(sesFit11)+
  autolayer(sesFit21)+
  autolayer(sesFit31)
#plot looks little optimal 

# R's(picked) alpha to minimizing the squared residuals.
#using daily data
sesFitOptimalD = ses(univariate_tsd,h=12)
summary(sesFitOptimalD)
#graph it
autoplot(univariate_tsd)+
  autolayer(sesFitOptimalD)
#looks like accurate for R generated alpha value for daily data

#using annual data
sesFitOptimalA = ses(univariate_tsa,h=12)
summary(sesFitOptimalA)
#graph it
autoplot(univariate_tsa)+
  autolayer(sesFitOptimalA)
#looks like accurate for R generated alpha value for annual data

#HOLT'S MODEL:
#the Holt function will include a trend term on top of the standard ses
#using daily data
holtFit21 = holt(univariate_tsd,h=365)
summary(holtFit21)

autoplot(univariate_tsd)+
  autolayer(holtFit21,series = "Holt", PI=FALSE)
#plot for holt is slightly visible

#using annual data
holtFit11 = holt(univariate_tsa,h=12)
summary(holtFit11)

autoplot(univariate_tsa)+
  autolayer(holtFit11,series = "Holt", PI=FALSE)
#plot looks downwards as per the pattern based on the earlier breath

#HOLT DAMPED MODEL:
#daily data
#damped term
#Holt damped for daily data
holtFitD = holt(univariate_tsd,h=365,damped = TRUE)
summary(holtFitD)
#graphing the plot -using damping term 
autoplot(univariate_tsd)+
  autolayer(holtFit21,series = "Holt", PI=FALSE)+
  autolayer(holtFitD,series = "Damped", PI=FALSE)

#plot looks like visible for Holt and damped

#holt damped for annual data
holtFitA = holt(univariate_tsa,h=12,damped = TRUE)
summary(holtFitA)
# graph-show the different by damping
autoplot(univariate_tsa)+
  autolayer(holtFit11,series = "Holt", PI=FALSE)+
  autolayer(holtFitA,series = "Damped", PI=FALSE)
#plot for both Holt and damped is visible.

trainHolt = holt(Train_set,h=365)
trainDamped = holt(Train_set,h=365,damped = T)

accuracy(trainHolt,Test_set)
accuracy(trainDamped,Test_set)
#high frequency

trainHolt1 = holt(Train_set1,h=12)
trainDamped1 = holt(Train_set1,h=12,damped = T)

accuracy(trainHolt1,Test_set1)
accuracy(trainDamped1,Test_set1)

autoplot(window(univariate_tsd, start = 2019))+
  autolayer(trainDamped,series = "Damped Holt's Method",PI=F)+
  autolayer(trainHolt,series = "Holt's Method",PI=F)+
  ggtitle("Forecasting on Test Set Holt's Linear Method")+
  ylab("Price")+
  xlab("Year")
#the trainDamped model performs better on the test set than the trainHolt model

autoplot(window(univariate_tsa, start = 2019))+
  autolayer(trainDamped1,series = "Damped Holt's Method",PI=F)+
  autolayer(trainHolt1,series = "Holt's Method",PI=F)+
  ggtitle("Forecasting on Test Set Holt's Linear Method")+
  ylab("Price")+
  xlab("Year")
#the trainDamped1 model performs better on the test set than the trainHolt1 model

#HOLT WINTER METHOD:
#forecasts with holt-winter's. It adds trend and seasonality to ses.
#seasonality can be included additive

#daily data

#frequency is very high
mod_d = hw(univariate_tsd,h=365,seasonal = "additive")
#addition cant be possible for daily data
mod_d$model
#error due to high frequency

#using only price univariate time series

mod_d2 = hw(univariate_tsd,h=365,seasonal = "additive")
mod_d2$model
#Error in predicting additive factor as frequency is too high



#or multiplicatively if it increases with the level of the time series. 
mod_3 = hw(univariate_tsd,h=365,seasonal = "multiplicative")
mod_3
mod_3$model
#point forecasts are possible

#we can also include a damped trend if we're worried about over forecasting.
mod_4 = holt(univariate_tsd,h=365,damped = T)
mod_4
mod_4$model

#As we obtain error for mod_2, plot is not possible.
autoplot(window(univariate_tsd,start=2019))+
  autolayer(modD,series = "Holt",PI=F)+
  autolayer(mod_d2,series = "Additive Season",PI=F)+
  autolayer(mod_3,series = "Multiplicative Season",PI=F)+
  autolayer(mod_4,series = "Damped",PI=F)

#As we obtain error for mod_2, plot is not possible and not accurate
# have tried some possibilities

#annual data
#forecast with the Holt model. It adds trend to the ses model for annual data
modA = holt(univariate_tsa,h=12)
modA
modA$model

mod_a = hw(univariate_tsa,h=12,seasonal = "additive")
mod_a$model
mod_a

#point forecast are possible and interval of 80% and 95% Intervals


#or multiplicatively if it increases with the level of the time series. 
#annual data
mod_am = hw(univariate_tsa,h=12,seasonal = "multiplicative")
mod_am$model
mod_am
#point forecasts are possible

modAD = holt(univariate_tsa,h=12,damped = T)
modAD
modAD$model

#annual data for all possible models
autoplot(window(univariate_tsa,start=2019))+
  autolayer(modA,series = "Holt",PI=F)+
  autolayer(mod_a,series = "Additive Season",PI=F)+
  autolayer(mod_am,series = "Multiplicative Season",PI=F)+
  autolayer(modAD,series = "Damped",PI=F)

#daily data
trainAdd = hw(Train_set,h=365,seasonal = "additive")
trainAdd_damped = hw(Train_set,h=365,seasonal = "additive", damped = TRUE)
trainMulti = hw(Train_set,h=365,seasonal = "multiplicative")
trainMulti_damped = hw(Train_set,h=365,seasonal = "multiplicative", damped = TRUE)

accuracy(trainAdd,Test_set)
accuracy(trainAdd_damped,Test_set)
accuracy(trainMulti,Test_set)
accuracy(trainMulti_damped,Test_set)
#we get errors due to high frequency.

#annual data
trainAdd1 = hw(Train_set1,h=12,seasonal = "additive")
trainAdd_damped1 = hw(Train_set1,h=12,seasonal = "additive", damped = TRUE)
trainMulti1 = hw(Train_set1,h=12,seasonal = "multiplicative")
trainMulti_damped1 = hw(Train_set1,h=12,seasonal = "multiplicative", damped = TRUE)

accuracy(trainAdd1,Test_set1)
accuracy(trainAdd_damped1,Test_set1)
accuracy(trainMulti1,Test_set1)
accuracy(trainMulti_damped1,Test_set1)
#In both additive and multiplicative conditions, we find that the damped holt method is better for holt winter's method.

#ETS
#daily data:
ets1 = ets(univariate_tsd,"AAA")
summary(ets1) #high frequency

ets2 = ets(univariate_tsd,"AAN")
summary(ets2) #AIC=15185.38, AICc=15185.42, BIC=15212.93

ets3 = ets(univariate_tsd,"AAA", damped = TRUE)
summary(ets3) # high frequency

ets5 = ets(univariate_tsd,"AAN", damped = TRUE)
summary(ets5) #15186.60, 15186.65,15219.66

ets6 = ets(univariate_tsd,"ANA")
summary(ets6) #high frequency 

ets7 = ets(univariate_tsd,"ANN")
summary(ets7) #15180.78,15180.80,15197.31(best till now)

ets8 = ets(univariate_tsd,"MAA")
summary(ets8) #high frequency

ets9 = ets(univariate_tsd,"MAM")
summary(ets9) #high frequency

ets10 = ets(univariate_tsd,"MAN")
summary(ets10) #15328.00,15328.03,15355.54

ets11 = ets(univariate_tsd,"MAA", damped = TRUE)
summary(ets11) #high frequency

ets12 = ets(univariate_tsd,"MAM", damped = TRUE)
summary(ets12) #high frequency


ets13 = ets(univariate_tsd,"MAN", damped = TRUE)
summary(ets13) # 15331.65,15331.70,15364.71

ets14 = ets(univariate_tsd,"MNM")
summary(ets14) #high frequency

ets15 = ets(univariate_tsd,"MNN")
summary(ets15) #15325.56,15325.57,15342.08

ets_auto = ets(univariate_tsd)
ets_auto
#I can't handle data with frequency greater than 24. Seasonality will be ignored. Try stlf() if you need seasonal forecasts
# Based on above models, ets7 best minimizes the AIC, AICc and BIC values. The ets() function also picks ANN as the best model.

#Forecasting ets7 ANN model

ets_fcast = forecast(ets7, h=365)
ets_fcast

autoplot(univariate_tsd)+
  autolayer(ets_fcast,series = "ets",PI=T)+ 
  ggtitle("ETS")+
  ylab("Price")+
  xlab("Year")

ets_train = ets(Train_set,"ANN")
ets_test = forecast(ets_train,h=365)

#annual data:
ets11 = ets(univariate_tsa,"AAA")
summary(ets11) #AIC=340.2205, AICc=356.9034, BIC=377.9187

ets21 = ets(univariate_tsa,"AAN")
summary(ets21) #324.7557,325.8668,335.2274(best till now)

ets31 = ets(univariate_tsa,"AAA", damped = TRUE)
summary(ets31)#340.2205,356.9034,377.9187 

ets51 = ets(univariate_tsa,"AAN", damped = TRUE)
summary(ets51)#327.2333,328.8182,339.7994 

ets61 = ets(univariate_tsa,"ANA")
summary(ets61)#332.4481,343.3572,363.8633

ets71 = ets(univariate_tsa,"ANN")
summary(ets71) #321.3992,321.8278,327.6822

ets81 = ets(univariate_tsa,"MAA")
summary(ets81) #339.7506 356.4335 377.4488 

ets91 = ets(univariate_tsa,"MAM")
summary(ets91) #346.5391 363.2220 384.2373

ets101 = ets(univariate_tsa,"MAN")
summary(ets101) #320.5430 321.6541 331.0147 

ets111 = ets(univariate_tsa,"MAA", damped = TRUE)
summary(ets111) #339.7506 356.4335 377.4488 

ets121 = ets(univariate_tsa,"MAM", damped = TRUE)
summary(ets121) #346.5391 363.2220 384.2373 


ets131 = ets(univariate_tsa,"MAN", damped = TRUE)
summary(ets131) #322.7822 324.3671 335.3483

ets141 = ets(univariate_tsa,"MNM")
summary(ets141) #348.2610 359.1701 379.6762 

ets151 = ets(univariate_tsa,"MNN")
summary(ets151) #317.0549,317.4834,323.3379(best)

ets_auto1 = ets(univariate_tsa)
ets_auto1
#Based on above models, ets151 best minimizes the AIC, AICc and BIC values. The ets() function also picks MNN as the best model.

ets_fcast1 = forecast(ets151, h=12)
ets_fcast1

autoplot(univariate_tsa)+
  autolayer(ets_fcast1,series = "ets1",PI=T)+ 
  ggtitle("ETS1")+
  ylab("Price")+
  xlab("Year")
#Forecast seems to be reasonable.

ets_train1 = ets(Train_set1,"MNN")
ets_test1 = forecast(ets_train1,h=12)

#ARIMA with raw data:
#daily data:
autoplot(univariate_tsd)

#We can notice increasing variance over time and we have to apply transformation to stabilize it. We can notice cyclicity.

lamval = BoxCox.lambda(univariate_tsd)
autoplot(cbind(Raw=(univariate_tsd),BoxCox=BoxCox((univariate_tsd), lamval),Log=log(univariate_tsd)),facets = T)
#In this case, there is a deep trough using BoxCox and logs and it doesn't smooth down. Whereas in raw data, we can notice high peaks. According to me, raw data would be an optimal choice to proceed with.
ggtsdisplay(univariate_tsd,lag=100)
#We can notice a sinusoidal pattern in ACF and there is a presence of white noise in PACF graph.
nsdiffs(univariate_tsd) # No seasonal differencing required

unit_test = ur.df(univariate_tsd, type ="drift")
summary(unit_test)
#The test statistic value is greater than critical value in 1% significant level. At 5% and 10%, test statistic values are smaller than critical values.


ggtsdisplay(diff(univariate_tsd))

unit_test_2 = ur.df(diff(univariate_tsd, type ="drift"))
summary(unit_test_2)

#building initial model
#We can notice sinusoidal pattern in both ACF and PACF. Significant spikes on both graphs at lag 1 
initial_model1 = Arima(univariate_tsd,c(1,1,1),include.drift=TRUE)
summary(initial_model1)#6654.35
initial_model2 = Arima(univariate_tsd,c(0,1,1),include.drift=TRUE)
summary(initial_model2)#6652.34
initial_model3 = Arima(univariate_tsd,c(0,1,0),include.drift=TRUE)
summary(initial_model3)#6650.4(best)

#check for nearby models for initial_model 3
nearby_model1 = Arima(univariate_tsd,c(1,1,0),include.drift=TRUE)
summary(nearby_model1)#6652.34
nearby_model2 = Arima(univariate_tsd,c(0,1,2),include.drift=TRUE)
summary(nearby_model2)#worse

#Based on AICc values initial_model3 is the best
#Check residuals

checkresiduals(initial_model3)
#The residuals are white noise

#forecasting

arima_fcast = forecast(initial_model3, h = 365)
arima_fcast

autoplot(univariate_tsd)+
  autolayer(arima_fcast,series = "Arima",PI=T)+
  ggtitle("ARIMA Testset Forecast")+
  ylab("Price")+
  xlab("Year")

arima_train =Arima(Train_set,c(0,1,0), include.drift=TRUE)
arima_test = forecast(arima_train,h=365)

#annual data
autoplot(univariate_tsa)

#We can notice increasing variance over time and we have to apply transformation to stabilize it. We can notice cyclicity.

lamval1 = BoxCox.lambda(univariate_tsa)
autoplot(cbind(Raw=(univariate_tsa),BoxCox=BoxCox((univariate_tsa), lamval1),Log=log(univariate_tsa)),facets = T)
#In this case, all three looks same. According to me, raw data would be an optimal choice to proceed with.
ggtsdisplay(univariate_tsa)
#We can notice a sinusoidal pattern in ACF and there is a presence of white noise in PACF graph.
nsdiffs(univariate_tsa) # No seasonal differencing required

unit_test1 = ur.df(univariate_tsa, type ="drift")
summary(unit_test1)
#The test statistic value is greater than critical values which indicates the presence of unit root.

ggtsdisplay(diff(univariate_tsa))

unit_test_21 = ur.df(diff(univariate_tsa, type ="drift"))
summary(unit_test_21)

#building initial model
#We can notice sinusoidal pattern in both ACF and PACF. Significant spikes on both graphs at lag 1 
initial_model11 = Arima(univariate_tsa,c(1,1,1),include.drift=TRUE)
summary(initial_model11)#242.91
initial_model21 = Arima(univariate_tsa,c(0,1,1),include.drift=TRUE)
summary(initial_model21)#241.79
initial_model31 = Arima(univariate_tsa,c(0,1,0),include.drift=TRUE)
summary(initial_model31)#240.57(best)

#check for nearby models for initial_model 31
nearby_model11 = Arima(univariate_tsa,c(1,1,0),include.drift=TRUE)
summary(nearby_model11)#242.36
nearby_model21 = Arima(univariate_tsa,c(0,1,2),include.drift=TRUE)
summary(nearby_model21)#238.81(best)

#Based on AICc values nearby_model21 is the best
#Check residuals

checkresiduals(nearby_model21)
#The residuals are white noise and null hypothesis is not rejected.

#forecasting

arima_fcast1 = forecast(nearby_model21, h = 12)
arima_fcast1

autoplot(univariate_tsa)+
  autolayer(arima_fcast1,series = "Arima1",PI=T)+
  ggtitle("ARIMA Testset Forecast1")+
  ylab("Price")+
  xlab("Year")


arima_train1 =Arima(Train_set1,c(0,1,2), include.drift=TRUE)
arima_test1 = forecast(arima_train1,h=12)

accuracy(arima_test,Test_set)
accuracy(ets_test,Test_set)
accuracy(arima_test1,Test_set1)
accuracy(ets_test1,Test_set1)

#For daily data, when comparing training and test set values between ARIMA(0,1,0) and ETS(ANN) models, ETS(ANN) has the lower ME, RMSE, MAE, MPE, MAPE, MASE. So between these two models, ETS(ANN) performing better.
#For annual data, when comparing training and test set values between ARIMA(0,1,2) and ETS(MNN) models, ETS(MNN) has the lower ME, RMSE, MAE, MPE, MAPE, MASE. So between these two models, ETS(MNN) performing better.

#Comparison:
#daily data:
trainHolt = holt(Train_set,h=365)
trainDamped = holt(Train_set,h=365,damped = T)
#annual data:
trainHolt1 = holt(Train_set1,h=12)
trainDamped1 = holt(Train_set1,h=12,damped = T)

#daily data
accuracy(trainHolt,Test_set)
accuracy(trainDamped,Test_set)
#annual data
accuracy(trainHolt1,Test_set1)
accuracy(trainDamped1,Test_set1)

trainAdd = hw(Train_set,h=365,seasonal = "additive")
trainAdd_damped = hw(Train_set,h=365,seasonal = "additive", damped = TRUE)
trainMulti = hw(Train_set,h=365,seasonal = "multiplicative")
trainMulti_damped = hw(Train_set,h=365,seasonal = "multiplicative", damped = TRUE)

#daily data
accuracy(trainAdd,Test_set)
accuracy(trainAdd_damped,Test_set)
accuracy(trainMulti,Test_set)
accuracy(trainMulti_damped,Test_set)
#high frequency

#annual data
trainAdd1 = hw(Train_set1,h=12,seasonal = "additive")
trainAdd_damped1 = hw(Train_set1,h=12,seasonal = "additive", damped = TRUE)
trainMulti1 = hw(Train_set1,h=12,seasonal = "multiplicative")
trainMulti_damped1 = hw(Train_set1,h=12,seasonal = "multiplicative", damped = TRUE)

accuracy(trainAdd1,Test_set1)
accuracy(trainAdd_damped1,Test_set1)
accuracy(trainMulti1,Test_set1)
accuracy(trainMulti_damped1,Test_set1)

ets_train = ets(Train_set,"ANN")
ets_test = forecast(ets_train,h=365)

ets_train1 = ets(Train_set1,"MNN")
ets_test1 = forecast(ets_train1,h=12)

arima_train =Arima(Train_set,c(0,1,0), include.drift=TRUE)
arima_test = forecast(arima_train,h=365)

arima_train1 =Arima(Train_set1,c(0,1,2), include.drift=TRUE)
arima_test1 = forecast(arima_train1,h=12)

#daily data
accuracy(arima_test,Test_set)
accuracy(ets_test,Test_set)
#annual data
accuracy(arima_test1,Test_set1)
accuracy(ets_test1,Test_set1)

#Forecasting Accuracy:
#Daily data:
#Forecasting Models	ME	RMSE	MAE	MPE	MAPE	MASE	
#Holt's damped method	-19.1591	23.9725	19.2650	34.8821	35.2764	1.0346	
#Holt Winters 	-	HIGH FREQUENCY 					
#ETS (ANN)	19.1620	23.9748	19.2677	34.8885	35.2822	1.0348	
#ARIMA(0,1,0)	26.8891	31.4372	26.9532	51.0629	51.3025	1.4475	

#Based on the provided metrics, Holt's Damped Method seems to be the best forecasting model among the three for this dataset. It shows lower errors and percentage errors compared to the other models, indicating better forecasting accuracy. 

#Annual data:
#Forecasting Accuracy							
#Forecasting Models	ME	RMSE	MAE	MPE	MAPE	MASE	
#Holt's damped method	-4.5517	6.2881	5.3284	-6.5470	7.5099	3.1028	
#Holt Winters Multiplicative	-6.6982	7.8071	6.8077	-9.4464	9.5809	3.9642	
#ETS (MNN)	-4.4841	6.2264	5.2866	-6.4533	7.4486	3.0785	
#ARIMA(0,1,2)	-5.2777	6.9925	5.8201	-7.5564	8.2255	3.3891	
#Based on the provided metrics, Holt's Damped Method appears to be the better forecasting model among the four for this dataset. It consistently shows lower errors and percentage errors, as well as a lower MASE value, indicating better forecasting accuracy compared to the other models.
