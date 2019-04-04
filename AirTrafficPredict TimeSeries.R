setwd('/Users/aakash/Downloads/')
install.packages('xts')
library(readxl)
detach(xts)
library(xts)
pass<-read_xls('UK Outward Passengers Movement-1.xls') 
pass1<-pass[c(-1,-2,-3,-4,-5,-46,-47),]
names(pass1)[1]<-"Year"
names(pass1)[2]<-"Quarter"
names(pass1)[3]<-"Air Traffic for Ireland"
names(pass1)[4]<-"Air Traffic for other EU"
names(pass1)[5]<-"Air Traffic for Rest Of Europe And Middle East"
names(pass1)[6]<-"Air Traffic for Rest of the World"
names(pass1)[7]<-"Total"


#1.
library(forecast)
Ireland<-pass1[,c(1,2,3)]
str(Ireland)
Ireland$`Air Traffic for Ireland`<-as.numeric(Ireland$`Air Traffic for Ireland`)
myts1<-ts(Ireland$`Air Traffic for Ireland`, start = c(1996,1), end = c(2005,4), frequency = 4)
myts1
plot(myts1)
ndiffs(myts1) #Data not stationary
#Testing for seasonality
boxplot(myts1~cycle(myts1))
plot(stl(myts1,s.window = "period"))
fit1<-ets(myts1)
plot(fit1)
accuracy(fit1$fitted,myts1)
forecast(fit1,8)
plot(forecast(fit1,8))
IrelandForecast<-forecast(fit1,8, level = c(90,95))
IF<-IrelandForecast$mean
IF<-as.xts(IF)
class(IF)
IF<-data.frame(IF)
#2.
OtherEU<-pass1[,c(1,2,4)]
str(OtherEU)
OtherEU$`Air Traffic for other EU`<-as.numeric(OtherEU$`Air Traffic for other EU`)
myts2<-ts(OtherEU$`Air Traffic for other EU`, start = c(1996,1), end = c(2005,4), frequency = 4)
ndiffs(myts2)
plot(stl(myts2,s.window = "period"))

fit2<- ets(myts2)
ls(fit2)
summary(fit2)
accuracy(fit2$fitted,myts2)
forecast(fit2,8)
plot(forecast(fit2,8))

OtherEUForecast<-forecast(fit2,8, level = c(90,95))
OEUF<-OtherEUForecast$mean
OEUF<-as.xts(OEUF)
class(OEUF)
OEUF<-data.frame(OEUF)

#3.
RestOfEuropeAndMiddleEast<-pass1[,c(1,2,5)]
str(RestOfEuropeAndMiddleEast)
RestOfEuropeAndMiddleEast$`Air Traffic for Rest Of Europe And Middle East`<-as.numeric(RestOfEuropeAndMiddleEast$`Air Traffic for Rest Of Europe And Middle East`)
myts3<-ts(RestOfEuropeAndMiddleEast$`Air Traffic for Rest Of Europe And Middle East`, start = c(1996,1), end = c(2005,4), frequency = 4)
plot(myts3)
ndiffs(myts3)
plot(stl(myts3,s.window = "period"))
fit3<- ets(myts3)
ls(fit3)
summary(fit3)
accuracy(fit3$fitted,myts3)
forecast(fit3,8)
plot(forecast(fit3,8))

RestOfEuropeAndMiddleEastForecast<-forecast(fit3,8, level = c(90,95))
ROEF<-RestOfEuropeAndMiddleEastForecast$mean
ROEF<-as.xts(ROEF)
class(ROEF)
ROEF<-data.frame(ROEF)

#4.
RestOfWorld<-pass1[,c(1,2,6)]
str(RestOfWorld)
RestOfWorld$`Air Traffic for Rest of the World`<-as.numeric(RestOfWorld$`Air Traffic for Rest of the World`)
myts4<-ts(RestOfWorld$`Air Traffic for Rest of the World`, start = c(1996,1), end = c(2005,4), frequency = 4)
plot(myts4)
plot(stl(myts4,s.window = "period"))
ndiffs(myts4)
#Using Holts Winter Triple smoothening
fit4<-HoltWinters(myts4)
list(fit4)
accuracy(fit4$fitted,myts4)
plot(forecast(fit4,12))
#Using ets-Better MAPE
fit5<-ets(myts4)
list(fit5)
summary(fit5)
accuracy(fit5$fitted, myts4)
plot(forecast(fit5$fitted,12))

ROWForecast<-forecast(fit5,8, level = c(90,95))
ROWF<-ROWForecast$mean
ROWF<-as.xts(ROWF)
class(ROWF)
ROWF<-data.frame(ROWF)

ForecastedValuesForNext2Years<-cbind(IF,OEUF,ROEF,ROWF)
