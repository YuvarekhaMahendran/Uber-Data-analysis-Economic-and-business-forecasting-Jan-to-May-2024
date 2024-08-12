library(fpp2)
library(urca)
library(readxl)
library(ggplot2)
library(forecast)




UBER_Historical_Data =read_excel("/Users/sushmithaugle/Library/Mobile Documents/com~apple~CloudDocs/Spring 2024/ECON 5337-Business & Economic Forecasting/Project/UBER Historical Data.xls")
View(UBER_Historical_Data)
UBER_Historical_Data
length(UBER_Historical_Data)#7
nrow(UBER_Historical_Data)#1237
summary(UBER_Historical_Data)

#convert date into TS

myts_a =ts(UBER_Historical_Data, start=c(2019, 5), end=c(2024, 4), frequency=12)#annual data
myts_a
length(myts_a)#420
summary(myts_a)
plot(myts_a)
autoplot(myts_a,facets=T)
#plotting the annual data

myts_u =ts(UBER_Historical_Data, start=c(2019, 5), end=c(2024, 4), frequency=365)#daily data
myts_u
length(myts_u)#12775
summary(myts_u)
#plotting the data
plot(myts_u)
autoplot(myts_u,facets=T)
#plotting the daily data

#daily data uni variate time series
univariate_tsd = myts_u[, "Price"]
univariate_tsd
autoplot(univariate_tsd)
#increasing and decreasing trend till 2022 and decreasing from 2023 to 2024 with small cycles

#annual data -uni variate ts 
univariate_tsa=myts_a[,"Price"]
univariate_tsa
autoplot(univariate_tsa)
# Increasing trend till 2022 and decreasing trend till 2024 with small cycles

#EXPONENTIAL SMOOTHENING
#checking exponential smoothing again for price

#Daily Data
#we can either specify alpha
help("ses")
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

#using annual data
sesFitOptimalA = ses(univariate_tsa,h=12)
summary(sesFitOptimalA)
#graph it
autoplot(univariate_tsa)+
  autolayer(sesFitOptimalA)
#looks like accurate for R generated alpha value for annual data

#using daily data
sesFitOptimalD = ses(univariate_tsd,h=12)
summary(sesFitOptimalD)
#graph it
autoplot(univariate_tsd)+
  autolayer(sesFitOptimalD)
#looks like accurate for R generated alpha value for daily data


#HOLT'S MODEL


#the Holt function will include a trend term on top of the standard ses

#using annual data
holtFit11 = holt(univariate_tsa,h=12)
summary(holtFit11)

autoplot(univariate_tsa)+
  autolayer(holtFit11,series = "Holt", PI=FALSE)
#plot looks downwards as per the pattern based on the earlier breath

#using daily data
holtFit21 = holt(univariate_tsd,h=365)
summary(holtFit21)

autoplot(univariate_tsd)+
  autolayer(holtFit21,series = "Holt", PI=FALSE)
#plot for holt is slightly visible

#HOLT DAMPED MODEL

#daily data
# damped term

#holt damped for annual data

holtFitA = holt(univariate_tsa,h=12,damped = TRUE)
summary(holtFitA)
# graph-show the different by damping
autoplot(univariate_tsa)+
  autolayer(holtFit11,series = "Holt", PI=FALSE)+
  autolayer(holtFitA,series = "Damped", PI=FALSE)
#plot for both Holt and damped is visible.

#Holt damped for daily data
holtFitD = holt(univariate_tsd,h=365,damped = TRUE)
summary(holtFitD)
#grpahing the plot -using damping term 
autoplot(univariate_tsd)+
  autolayer(holtFit21,series = "Holt", PI=FALSE)+
  autolayer(holtFitD,series = "Damped", PI=FALSE)

#plot looks like visible for Holt and damped


#creating a forecast with the Holt model. It adds trend to the ses model. 


#forecast with the Holt model. It adds trend to the ses model for annual data
modA = holt(univariate_tsa,h=12)
modA
modA$model

autoplot(window(univariate_tsa,start=2019))+
  autolayer(sesFit11,series = "Alpha=0.5",PI=F)+
  autolayer(sesFit21,series = "Alpha=0.75",PI=F)+
  autolayer(sesFitOptimalA,series = "Alpha=Optimal",PI=F)+
  autolayer(modA,series = "Holt",PI=F)
#plot for the holt looks little accurate


#forecast with the Holt model. It adds trend to the ses model for daily data
modD = holt(univariate_tsd,h=365)
modD
modD$model
summary(modD)

autoplot(window(univariate_tsd,start=2019))+
  autolayer(sesFit1,series = "Alpha=0.5",PI=F)+
  autolayer(sesFit2,series = "Alpha=0.75",PI=F)+
  autolayer(sesFitOptimalD,series = "Alpha=Optimal",PI=F)+
  autolayer(modD,series = "Holt",PI=F)
#plot for the Holt looks visible and kind of same pattern


#forecasts with holt-winter's. It adds trend and seasonality to ses.
#seasonality can be included additive

#daily data

#frequency is very high
mod_d = hw(univariate_tsd,h=365,seasonal = "additive")
#addition cant be possible for daily data
mod_d$model
#error due to high frequency

#annual data
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


#training and testing data split 
#and we determine which model a particular time series best using training
#and testing sets.


#checking for uni variate time series for daily data

summary(univariate_tsd)
length(univariate_tsd)

1825*0.8
traindata = head(univariate_tsd,1460)
summary(traindata)
1825-1460


testdata = tail(univariate_tsd,365)
summary(testdata)


trainHolt = holt(traindata,h=365)
trainHolt
trainDamped= holt(traindata,h=365,damped = T)
trainDamped
#additive and multiplicative are little difficult to predict for high frequency
trainAdd= hw(traindata,h=365,seasonal = "additive")
trainAdd

#frequency is too high
trainMulti= hw(traindata,h=365,seasonal = "multiplicative")
trainMulti
#frequency is too high

accuracy(trainHolt,testdata)
accuracy(trainDamped,testdata)
#cant be changed as frequency is high 
accuracy(trainAdd,testdata)
accuracy(trainMulti,testdata)
##as we find addition and multiplication operations difficult, it becomes difficult to check accuracy.

autoplot(univariate_tsd)+
  autolayer(trainAdd,series = "seasonal add",PI=F)+ ##difficult
  autolayer(trainMulti,series = "seasonal multi",PI=F)+
  autolayer(trainDamped,series = "Damped",PI=F)+
  autolayer(trainHolt,series = "holt",PI=F)
#high frequency
#we can also include damped trends with the holt winter's model. 
trainAddDamp = hw(traindata,h=365,seasonal = "additive",damped = T)
#frequency is too high
accuracy(trainAddDamp,testdata)

autoplot(univariate_tsd)+
  autolayer(trainAdd,series = "seasonal add",PI=F)+
  autolayer(trainMulti,series = "seasonal multi",PI=F)+
  autolayer(trainDamped,series = "Damped",PI=F)+
  autolayer(trainHolt,series = "holt",PI=F)+
  autolayer(trainAddDamp,"Additive Damped",PI=F)+
  autolayer(trainMultiDamp,"Multiplicative Damped",PI=F)

#plotting is almost same
trainMultiDamp = hw(traindata,h=365,seasonal = "multiplicative",damped = T)
# seasonality is not multiplicative 
accuracy(trainMultiDamp,testdata)
#forecasts will be difficult as frequency is high as we dont have seasonality 



#training and testing data split for annual data
#and we determine which model a particular time series best using training and testing sets.


#checking for uni variate time series for daily data

summary(univariate_tsa)
length(univariate_tsa)

60*0.8
traindataA = head(univariate_tsa,48)
summary(traindataA)
60-48


testdataA = tail(univariate_tsa,12)
summary(testdataA)


trainHoltA = holt(traindataA,h=12)
trainHoltA
trainDampedA= holt(traindataA,h=12,damped = T)
trainDampedA

trainAddA= hw(traindataA,h=12,seasonal = "additive")
trainAddA


trainMultiA= hw(traindataA,h=12,seasonal = "multiplicative")
trainMultiA


accuracy(trainHoltA,testdataA)
accuracy(trainDampedA,testdataA)

accuracy(trainAddA,testdataA)
accuracy(trainMultiA,testdataA)

autoplot(univariate_tsa)+
  autolayer(trainAddA,series = "seasonal add",PI=F)+
  autolayer(trainMultiA,series = "seasonal multi",PI=F)+
  autolayer(trainDampedA,series = "Damped",PI=F)+
  autolayer(trainHoltA,series = "holt",PI=F)
#plot look little accurate for all -mostly for seasonal additive


#we can also include damped trends with the holt winter's model. 
trainAddDampA = hw(traindataA,h=12,seasonal = "additive",damped = T)

accuracy(trainAddDampA,testdataA)

trainMultiDampA = hw(traindataA,h=12,seasonal = "multiplicative",damped = T)

accuracy(trainMultiDampA,testdataA)


autoplot(univariate_tsa)+
  autolayer(trainAddA,series = "seasonal add",PI=F)+
  autolayer(trainMultiA,series = "seasonal multi",PI=F)+
  autolayer(trainDampedA,series = "Damped",PI=F)+
  autolayer(trainHoltA,series = "holt",PI=F)+
  autolayer(trainAddDampA,"Additive Damped",PI=F)+
  autolayer(trainMultiDampA,"Multiplicative Damped",PI=F)

#plotting are kind of accurate



#Time series Regression 

#creating a ts object from the data set
myts_a =ts(UBER_Historical_Data, start=c(2019, 5), end=c(2024, 4), frequency=12)#annual data
myts_a
length(myts_a)#420
summary(myts_a)
plot(myts_a)
#doubt: whether to include graph for this

myts_u =ts(UBER_Historical_Data, start=c(2019, 5), end=c(2024, 4), frequency=365)#daily data
myts_u
length(myts_u)#12775
summary(myts_u)
#plotting the data
plot(myts_u)


#running a timeseries regression
#its not working out
if ("Date" %in% colnames(data)) {
  # Plot data
  ggplot(data, aes(x = Date, y = Price)) +
    geom_line() +
    labs(title = "Uber Data: Price over Time")
} else {
  print("Error: 'Date' column not found in uber_data dataframe.")
}

##it's not working in this case.
ggplot(data, aes(x = Date, y = Price)) +
  geom_line() +
  labs(title = "Uber Data: Price over Time") +
  theme_minimal()


#models for daily data
autoplot(myts_u [,c("Price","Date")],facets=TRUE)
autoplot(myts_u [,c("Date","Price")],facets=TRUE)
##I don't find any difference between these 2 plots
#truing some models
reg1 = tslm(Price~Date,myts_u)
summary(reg1)

summary(myts_u)
autoplot(myts_u[,c("Price","Date","Open","High","Low","Vol.","Change %")],facets=TRUE)

#increasing and decreasing trends

colnames(myts_u)
#changing column name as Change % is throwing error 
colnames(myts_u)[which(colnames(myts_u) == "Change %")] <- "Change"

reg2 = tslm(Price ~ Date + Open + High + Low + Vol. + Change, data = myts_u)
summary(reg2)
#This is a multiple TS regression. Controlling for other variables, the 
#relationship between date and price 
#we can use the check residuals function to run diagnostics on our regression
checkresiduals(reg1)
#This regression violates many of our assumptions, and would likely be a poor
#choice for forecasting

checkresiduals(reg2)


#we can add a trend to this model to see if that improves the fit
reg3 = tslm(Price ~ Date +Open+High+Low+Vol.+Change+trend+season,myts_u)
summary(reg3)


checkresiduals(reg3)
#little better -the data has white noise



#another simple regression looks at the relationship between date and price predictions
#daily data
autoplot(myts_u,facets = TRUE)


#doing backwards step wise regression. 
bsr1 = tslm(Price ~ Date +Open+High+Low+Vol.+Change+trend+season,myts_u)
CV(bsr1)#-2950.5923790-start of the analysis
bsr2 = tslm(Price ~ Date +Open+High+Low+Vol.+Change+season,myts_u)
CV(bsr2)#-2953.7366570 -better
bsr3 = tslm(Price ~ Date +Open+High+Low+Vol.+Change+trend,myts_u)
CV(bsr3)#-3449.9708059 -better 
bsr4 = tslm(Price ~ Open+High+Low+Vol.+Change+trend+season,myts_u)
CV(bsr4) # -2953.7133984-worse
bsr5 = tslm(Price ~ Open+High+Vol.+trend+season,myts_u)
CV(bsr5)#-1093.9028451 -worse
bsr6 = tslm(Price ~ Open+High+Change+trend+season,myts_u)
CV(bsr6)#-1909.8753079  -worse

bsr7 = tslm(Price ~ Date +Open+High+Low+trend+season,myts_u)
CV(bsr7)#-2329.4145007- worse


bsr8 = tslm(Price ~ Date +Open+High+Low+Vol.+trend,myts_u)
CV(bsr8)# -2859.0861604-worse

bsr9 = tslm(Price ~ Date +Open+High+Low+trend+season,myts_u)
CV(bsr9)#-2329.4145007 -worse

bsr10 = tslm(Price ~ Date +Open+High+Low+trend,myts_u)
CV(bsr10) #-2859.8852456-worse 


bsr11 = tslm(Price ~ Date +Open+High+trend,myts_u)
CV(bsr11)#-1580.7686663 -worse

bsr12 = tslm(Price ~ Date +Open+trend,myts_u)
CV(bsr12) # 40.247162 -best till now

bsr13 = tslm(Price ~ Date +trend,myts_u)
CV(bsr13)#9185.4407299-worse

bsr13 = tslm(Price ~ Date +trend+season,myts_u)
CV(bsr13)#1.000887e+04-worse


bsr14 = tslm(Price ~ Date +Open+trend+season,myts_u)
CV(bsr14) #555.867956 -worse

#checking residuals for the best model 

checkresiduals(bsr12)
# 0.005747 >0.001 and 0.005747< 0.05
#looks likes white noise


#A forward stepwise regression 
sr1 = tslm(Price ~ Date,myts_u)
CV(sr1)#9244.7534522-start 
sr2 = tslm(Price~trend+Date,myts_u)
CV(sr2)# 9185.4407299-better

sr3 = tslm(Price~trend+Date+season,myts_u)
CV(sr3)#1.000887e+04 -Worse

sr4 = tslm(Price~trend+Date+Open,myts_u)
CV(sr4)#40.247162-Best till now


sr5 = tslm(Price~trend+Date+Open+High,myts_u)
CV(sr5) #-1580.7686663-Worse

sr6 = tslm(Price~trend+Date+Open+High+Low,myts_u)
CV(sr6)#-2859.8852456 -Worse

sr7 = tslm(Price~trend+Date+Open+High+Low+Vol.,myts_u)
CV(sr7) #-2859.0861604-Worse

sr8 = tslm(Price~trend+Date+Open+High+Low+Vol.+Change,myts_u)
CV(sr8) #  -3449.9708059-Worse


sr9= tslm(Price~trend+Date+Open+High+Low+Vol.+Change+season,myts_u)
CV(sr9)#-2950.5923790-Worse


#checking residuals for the best model in forward regression 
checkresiduals(sr4)

#looks like white noise in fwd regression


#for annual data
#models for daily data
myts_a
autoplot(myts_a [,c("Price","Date")],facets=TRUE)
autoplot(myts_a [,c("Date","Price")],facets=TRUE)
reg11 = tslm(Price~Date,myts_a)
summary(reg11)


autoplot(myts_a[,c("Price","Date","Open","High","Low","Vol.","Change %")],facets=TRUE)

colnames(myts_a)[which(colnames(myts_a) == "Change %")] <- "Change"
#increasing and decreasing trends
reg22 = tslm(Price ~ Date +Open+High+Low+Vol.+Change,myts_a)
summary(reg22)
#This is a multiple TS regression. Controlling for other variables, the 
#relationship between date and price 
#we can use the check residuals function to run diagnostics on our regression
checkresiduals(reg11)
#doesn't look like white noise as p value is 0.000004638

checkresiduals(reg22)
#look like white noise 

#we can start here
reg33 = tslm(Price ~ Date +Open+High+Low+Vol.+Change+trend+season,myts_a)
summary(reg33)


checkresiduals(reg33)
#little better -the data has white noise



#another simple regression looks at the relationship between price predictions using the predictors in the dataset
autoplot(myts_a,facets = TRUE)


#doing backwards stepwise regression. 
bsr11 = tslm(Price ~ Date +Open+High+Low+Vol.+Change+trend+season,myts_a)
CV(bsr11)#-43.1708617-Worse
bsr22 = tslm(Price ~ Date +Open+High+Low+Vol.+Change+season,myts_a)
CV(bsr22)# -47.5727314-better
bsr33 = tslm(Price ~ Date +Open+High+Low+Vol.+Change+trend,myts_a)
CV(bsr33)#-72.9237136 -better till now
bsr44 = tslm(Price ~ Open+High+Low+Vol.+Change+trend+season,myts_a)
CV(bsr44) #-47.5454203-didnt get better
bsr55 = tslm(Price ~ Open+High+Vol.+trend+season,myts_a)
CV(bsr55)#-11.4705997 -worse
bsr66 = tslm(Price ~ Open+High+Change+trend+season,myts_a)
CV(bsr66)#-39.0307558-still worse

bsr77 = tslm(Price ~ Date +Open+High+Low+trend+season,myts_a)
CV(bsr77)#-29.5122164 -still worse

#picking the best case scenario
bsr88 = tslm(Price ~ Date +Open+High+Low+Vol.+trend,myts_a)
CV(bsr88)#-49.5317755-getting better

bsr99 = tslm(Price ~ Date +Open+High+Low+trend+season,myts_a)
CV(bsr99)#-29.5122164-worse

bsr100 = tslm(Price ~ Date +Open+High+Low+trend,myts_a)
CV(bsr100) #-51.7616777-getting better


bsr110 = tslm(Price ~ Date +Open+High+trend,myts_a)
CV(bsr110)#-33.9662625 -getting worse

bsr120 = tslm(Price ~ Date +Open+trend,myts_a)
CV(bsr120) #34.8990123 -best till now

bsr130 = tslm(Price ~ Date +trend,myts_a)
CV(bsr130)#163.1691585-worse

bsr140 = tslm(Price ~ Date +trend+season,myts_a)
CV(bsr140)#190.6080683-getting worse


bsr150 = tslm(Price ~ Date +Open+trend+season,myts_a)
CV(bsr150) # 50.098181 -worse

#checking residuals for the best model 

checkresiduals(bsr120)
#looks likes white noise


#A forward step wise regression using annual data
sr11 = tslm(Price ~ Date,myts_a)
CV(sr11)#164.3206523-high to begin with
sr22 = tslm(Price~trend+Date,myts_a)
CV(sr22)#163.1691585-little better

sr33 = tslm(Price~trend+Date+season,myts_a)
CV(sr33)#190.6080683-Worse

sr44 = tslm(Price~trend+Date+Open,myts_a)
CV(sr44)#34.8990123-Best till now


sr55 = tslm(Price~trend+Date+Open+High,myts_a)
CV(sr55) #-33.9662625-Worse

sr66 = tslm(Price~trend+Date+Open+High+Low,myts_a)
CV(sr66)# -51.7616777-Worse

sr77 = tslm(Price~trend+Date+Open+High+Low+Vol.,myts_a)
CV(sr77) #-49.5317755-Worse

sr88 = tslm(Price~trend+Date+Open+High+Low+Vol.+Change,myts_a)
CV(sr88) # -72.9237136-Worse


sr99= tslm(Price~trend+Date+Open+High+Low+Vol.+Change+season,myts_a)
CV(sr99)#-43.1708617 -Worse


#checking residuals for the best model in forward regression 
checkresiduals(sr44)

#looks like white noise in fwd regression


#ETS MODEL
#Ex Ante Forecasting
#We're going to use the model we found fit best to forecast with
#annual data
freg = tslm(Price~trend+Date+Open,myts_a)

#we'll do this with an ets model
openETS = ets(myts_a[,"Open"])
summary(openETS)

fopen = forecast(openETS,h=12)
summary(fopen)

DateETS = ets(myts_a[,"Date"])
summary(DateETS)
fdate = forecast(DateETS,h=12)
summary(fdate)



foreCastedData = data.frame(
  Open = fopen$mean,
  Date = fdate$mean
)

fcast.exante = forecast(freg,newdata = foreCastedData)
summary(fcast.exante)

autoplot(myts_a[,"Price"])+
  autolayer(fcast.exante)

#forecast looks stable 

#Scenario Based Forecasting- we took some scenarios -but we are not sure how these scenario will help us decide 
# we are not incoroporating any scenario based forecasting in report

#for scenario based forecasting, we choose different scenarios and forecast 
#based on those.



scenario1 <- data.frame(
  Date = as.Date("2024-01-01"),  # Assuming a specific date
  Open = rep(9, 6)
)


help(rep)
s1 = rep(0,6)
s1[1]= myts_a[57,"Open"]-0.25
for( t in 2:6){
  s1[t]=s1[t-1]-0.25
}


scenario1=data.frame("Open"=s1,"Date"=c(myts_a[58:60,"Date"],s1[1:3]))
scenario1



fcast1=forecast(bsr120,newdata= scenario1)
summary(fcast1)

autoplot(myts_a[,"Price"])+
  autolayer(fcast1)
#forecast look accurate

#scenario 2

s2=rep(0,6)
s2[1] = myts_a[60,"Open"]+0.1
for (t in 2:6){
  s2[t]=s2[t-1]+0.1
  
}

s2

scenario2=data.frame("Open"=s2,"Date"=c(myts_a[58:60,"Date"],s1[1:3]))

fcast2=forecast(bsr120,newdata= scenario2)
summary(fcast2)

autoplot(myts_a[,"Price"])+
  autolayer(fcast2)
#forecast look accurate for second scenario



#For Daily Data using same code but frequency is different
#daily data
freg1 = tslm(Price~trend+Date+Open,myts_u)

#we'll do this with an ets model
openETS1 = ets(myts_u[,"Open"])
#frequency cant be handled
summary(openETS1)

fopen1 = forecast(openETS1,h=12)
summary(fopen1)

DateETS1 = ets(myts_u[,"Date"])
#frequency is still high
summary(DateETS1)
fdate1 = forecast(DateETS1,h=12)
summary(fdate1)



foreCastedDataD = data.frame(
  Open = fopen1$mean,
  Date = fdate1$mean
)

fcast.exante_daily = forecast(freg1,newdata = foreCastedDataD)
summary(fcast.exante_daily)

autoplot(myts_u[,"Price"])+
  autolayer(fcast.exante_daily)

#forecast looks accurate 

#Scenario Based Forecasting

#for scenario based forecasting, we choose different scenarios and forecast 
#based on those.



scenario2 = data.frame(
  Date = as.Date("2024-01-01"),  # Assuming a specific date
  Open = rep(9, 6)
)


help(rep)
s11 = rep(0,6)
s11[1]= myts_u[57,"Open"]-0.25
for( t1 in 2:6){
  s11[t1]=s1[t1-1]-0.25
}


scenario11=data.frame("Open"=s11,"Date"=c(myts_u[58:60,"Date"],s11[1:3]))
scenario11



fcast11=forecast(bsr12,newdata= scenario11)
summary(fcast11)

autoplot(myts_u[,"Price"])+
  autolayer(fcast11)
#forecast look accurate

#scenario 2

s22=rep(0,6)
s22[1] = myts_u[60,"Open"]+0.1
for (t1 in 2:6){
  s22[t1]=s22[t1-1]+0.1
  
}

s22

scenario22=data.frame("Open"=s22,"Date"=c(myts_u[58:60,"Date"],s11[1:3]))

fcast22=forecast(bsr12,newdata= scenario22)
summary(fcast22)

autoplot(myts_u[,"Price"])+
  autolayer(fcast22)
#forecast looks not accurate for second scenario


