#title: "Forecasting the Gross Domestic Product of South Africa"
#author: "yaseen alli"
#date: "5/11/2022



#time series packages
suppressMessages(library(xts))
suppressMessages(library(zoo))
suppressMessages(library(ggplot2))
suppressMessages(library(Quandl))
suppressMessages(library(forecast))
suppressMessages(library(stargazer))
suppressMessages(library(pastecs))
suppressMessages(library(dplyr))

##############################################################
#LOAD DATA
#############################################################

mydata<-read.csv("YasDataFinalSA.csv",header=TRUE, na.strings ="#N/A" )
head(mydata)
typeof(mydata)

# Create a zoo recognized date format
mydata$quarter = as.yearqtr(mydata$Date,format="%Yq%q")
#-------------------------------------------------------------------------------
#***1. Inspect the Data
typeof(mydata$gdp)

#** Logarithms to minimize influence of heteroskedasticity
mydata$lgdp=log(mydata$gdp)
mydata$lgov=log(mydata$gov)
mydata$lcons=log(mydata$cons)
mydata$linv=log(mydata$inv)

head(mydata)
mydata$time<-1:248

#--------------------------------------------
# Recession indicators
mydata$rec0809<- as.numeric(mydata$Date >= "2008q1" & mydata$Date<="2009q4")
#print (rec0809)

mydata$rec0003<- as.numeric(mydata$Date >= "2000q4" & mydata$Date<="2003q4")
#print (rec0003)

mydata$rec9698<-as.numeric(mydata$Date >= "1996q4" & mydata$Date<="1998q4")
#print (rec9698)

mydata$rec8992<-as.numeric(mydata$Date >= "1989q2" & mydata$Date<="1992q4")
#print (rec8992)

mydata$rec8486<-as.numeric(mydata$Date >= "1984q2" & mydata$Date<="1986q2")
#print (rec8486)

mydata$rec7477<-as.numeric(mydata$Date >= "1974q2" & mydata$Date<="1977q4")
#print (rec7477)

mydata$rec6063<-as.numeric(mydata$Date >= "1960q2" & mydata$Date<="1962q4")
#print (rec6063)

#--------------------------------------------
#*** Create Zoo data type (leave out the Date Variable)
zoodata<-read.zoo(mydata[,-c(1)], header=TRUE, index.column = "quarter")
xtsdata<-as.xts(zoodata) #xts object
head(zoodata)

#*** change to billions for ease of Summary Statistics
zoodata$gdp_b<-(zoodata$gdp)/1000
zoodata$cons_b<-(zoodata$cons)/1000
zoodata$inv_b<-(zoodata$inv)/1000
zoodata$gov_b<-(zoodata$gov)/1000

#*------------------------------------------------------------------------------
#Descriptive Statistics
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#*** 2. Summary Statistics

#Split the sample into two parts: 
# TRAIN: 1960 Q1 to 2018 Q4  (p1)
# TEST : 2019 Q1 to 2021 Q4  (p2)

# Define Variables according to Train and Test
train<-1:236
test<-237:248
first.q <- 1960.25
last.q <-  2018.75
zoodata$gdp.p1 <- window(zoodata$gdp, end=last.q)
zoodata$gdp.p2 <- window(zoodata$gdp, start=last.q+0.25)

zoodata$lgdp.p1 <- window(zoodata$lgdp, end=last.q)
zoodata$lgdp.p2 <- window(zoodata$lgdp, start=last.q+0.25)

zoodata$inv.p1 <- window(zoodata$inv, end=last.q)
zoodata$inv.p2 <- window(zoodata$inv, start=last.q+0.25)

zoodata$linv.p1 <- window(zoodata$linv, end=last.q)
zoodata$linv.p2 <- window(zoodata$linv, start=last.q+0.25)

zoodata$gov.p1 <- window(zoodata$gov, end=last.q)
zoodata$gov.p2 <- window(zoodata$gov, start=last.q+0.25)

zoodata$lgov.p1 <- window(zoodata$lgov, end=last.q)
zoodata$lgov.p2 <- window(zoodata$lgov, start=last.q+0.25)

zoodata$cons.p1 <- window(zoodata$cons, end=last.q)
zoodata$cons.p2 <- window(zoodata$cons, start=last.q+0.25)

zoodata$lcons.p1 <- window(zoodata$lcons, end=last.q)
zoodata$lcons.p2 <- window(zoodata$lcons, start=last.q+0.25)

zoodata$yield.p1 <- window(zoodata$yield, end=last.q)
zoodata$yield.p2 <- window(zoodata$yield, start=last.q+0.25)

head(zoodata)

tail(zoodata)

#-------------------------------------------------------------------------------
#*** Summary Table
#Growth Rate


#** During the Training Period
summary(zoodata[,c("gdp.p1","inv.p1","gov.p1","cons.p1","yield.p1")])
#stargazer(zoodata[,c("zoodata$gdp.p1","inv.p1","gov.p1","cons.p1","yield.p1")], type = "text", #median = T, flip = T, out="SummaryTRAIN.doc")
#Get the standard deviation
stat.desc(zoodata[,c("gdp.p1","inv.p1","gov.p1","cons.p1","yield.p1")])

#* During the Test period
summary(zoodata[,c("gdp.p2","inv.p2","gov.p2","cons.p2","yield.p2")])
stat.desc(zoodata[,c("gdp.p2","inv.p2","gov.p2","cons.p2","yield.p2")])

#* Get the number of Observations
sum(complete.cases(zoodata[,c("gdp.p1","inv.p1","gov.p1","cons.p1","yield.p1")]))
# 236

#* Get the number of Observations
sum(complete.cases(zoodata[,c("gdp.p2","inv.p2","gov.p2","cons.p2","yield.p2")]))
# 12

#-------------------------------------------------------------------------------
#*** 2.1 Plots

#GDP Plot
plot(zoodata$gdp_b, xlab="Quarter", ylab="Real GDP (R' billions)",  main="GDP", col="black",lwd=4)

# Acquire detrended GDP
zoodata$detrgdp_b<-lm(gdp_b~time, data=zoodata)$residuals
#detrgdp_mod<-lm(gdp_b~time, data=zoodata, subset=complete.cases(zoodata$gdp.p1))
#zoodata$detrgdp_b<-NA
#for(i in 1:sum(complete.cases(zoodata[,c("zoodata$gdp.p1")]))) {
#  zoodata$detrgdp_b[i]<- detrgdp_mod$residuals[i] 
#}

plot(zoodata$detrgdp_b[1:236,], xlab="Quarter", ylab="detrended Real GDP",  main="Train", col="green",lwd=4)

zoodata$detrlgdp_b<-lm(log(gdp_b)~time, data=zoodata)$residuals

#detrlgdp_mod<-lm(log(gdp_b)~time, data=zoodata, subset=complete.cases(lzoodata$gdp.p1))
#zoodata$detrlgdp_b<-NA
#for(i in 1:sum(complete.cases(zoodata[,c("lzoodata$gdp.p1")]))) {
#  zoodata$detrlgdp_b[i]<- detrlgdp_mod$residuals[i] 
#}

plot(zoodata$detrlgdp_b[1:236,], xlab="Quarter", ylab="detrended Log of Real GDP",  main="", col="green",lwd=4)


#First difference

zoodata$diffgdp<-diff(zoodata$gdp)
plot(zoodata$diffgdp[1:236,], xlab="Quarter", ylab="First Difference (Real GDP)",  main="", col="green",lwd=4)

zoodata$difflgdp<-diff(zoodata$lgdp)
plot(zoodata$difflgdp[1:236,], xlab="Quarter", ylab="First Difference (Log of Real GDP)",  main="", lwd=4, col="green")

#--------Transoformation----------
#Growth Rate
summary(zoodata$difflgdp[1:236,])
stat.desc(zoodata$difflgdp[1:236,])
#Test sample
summary(zoodata$difflgdp[237:248,])
stat.desc(zoodata$difflgdp[237:248,])
#------------------------------------------

#2.2.1 GDP The graphs in one plot
par(mfrow=c(1,1))
par(mfrow=c(2,2))
plot(zoodata$gdp_b, xlab="Quarter", ylab="Real GDP (R' billions)",  main="Full Sample", col="black",lwd=4)
plot(zoodata$detrgdp_b[1:236,], xlab="Quarter", ylab="detrended Real GDP",  main="Train", col="green",lwd=4)

plot(zoodata$diffgdp[1:236,], xlab="Quarter", ylab="1st Diff. (Real GDP)",  main="Train", col="green",lwd=4)

plot(zoodata$difflgdp[1:236,], xlab="Quarter", ylab="1st Diff. (Log of Real GDP)",  main="Train", lwd=4, col="green")

par(mfrow=c(1,1))

#-------------------------------------------------------------------------------
# 2.2.2
par(mfrow=c(2,2))
plot(zoodata$inv_b[1:236,], xlab="Quarter", ylab="Real Investment (R' bil)",  main="Train", col="blue",lwd=4)
plot(zoodata$cons_b[1:236,], xlab="Quarter", ylab="Real Consumption (R' bil)",  main="Train", col="blue",lwd=4)
plot(zoodata$gov_b[1:236,], xlab="Quarter", ylab="Real Government (R' bil)",  main="Train", col="blue",lwd=4)
plot(zoodata$yield[1:236,], xlab="Quarter", ylab="Interest Rate (%)",  main="Train", lwd=4, col="blue")

par(mfrow=c(1,1))

#------------------------------------------------------------------------------
library(KernSmooth)
#*** 2.2.3 Real GDP versus determinants
par(mfrow=c(2,2))
plot(x=zoodata$linv[1:236,],y=zoodata$lgdp[1:236,], xlab="Log of Investment", ylab="Log of Real GDP",  main="Train", col="blue",lwd=4)
fit <- locpoly(x=zoodata$linv[1:236,],y=zoodata$lgdp[1:236,], bandwidth = 0.25)
lines(fit, lwd=4, col="maroon")

summary(lm(zoodata$lgdp[1:236,]~zoodata$linv[1:236,]))
# 0.79280 ***

#----
plot(x=zoodata$lcons[1:236,],y=zoodata$lgdp[1:236,], xlab="Log of Consumption", ylab="Log of Real GDP",  main="Train", col="blue",lwd=4)
fit <- locpoly(x=zoodata$lcons[1:236,],y=zoodata$lgdp[1:236,], bandwidth = 0.25)
lines(fit, lwd=4, col="maroon")

summary(lm(zoodata$lgdp[1:236,]~zoodata$lcons[1:236,]))
# 0.795846 ***


#----
plot(x=zoodata$lgov[1:236,],y=zoodata$lgdp[1:236,], xlab="Log of Government", ylab="Log of Real GDP",  main="Train", col="blue",lwd=4)
fit <- locpoly(x=zoodata$lgov[1:236,],y=zoodata$lgdp[1:236,], bandwidth = 0.25)
lines(fit, lwd=4, col="maroon")

summary(lm(zoodata$lgdp[1:236,]~zoodata$lgov[1:236,]))
# 0.749664 ***


#----
plot(x=zoodata$yield[1:236,],y=zoodata$lgdp[1:236,], xlab="Yield (%)", ylab="Log of Real GDP",  
     main="Train", col="blue",lwd=4)
fit <- locpoly(x=zoodata$yield[1:236,],y=zoodata$lgdp[1:236,], bandwidth = 0.25)
lines(fit, lwd=4, col="maroon")


summary(lm(zoodata$lgdp[1:236,]~zoodata$yield[1:236,]))
# 0.034389 ***
temp=zoodata$yield[1:236,]
temp2=zoodata$yield[1:236,]*zoodata$yield[1:236,]

summary(lm(zoodata$lgdp[1:236,]~temp+temp2))
#Concave Quadratic shape 

par(mfrow=c(1,1))

#-------------------------------------------------------------------------------
# Diagnostics of ARIMA Structure

par(mfrow=c(1,1))
par(mfrow=c(2,2))

#Plot the ACF and PACF for yt in the first subsample and find the best model
library(forecast)
Acf(zoodata$lgdp.p1, type="correlation", lag=48, main="ACF: Log[Real GDP]",lwd=4,col="blue")

#PACF
Acf(zoodata$lgdp.p1, type="partial", lag=48, main="PACF: Log(Real GDP)", lwd=4,col="blue")
# AR(1) or AR(5) or AR(9)

#Differenced
dlGDP <- diff(log(zoodata$gdp))
dlGDP.p1 <- window(dlGDP, start="1960 Q2", end="2018 Q4")
dlGDP.p2 <- window(dlGDP, start="2019 Q1")
#ACF
Acf(dlGDP.p1, type="correlation",
    lag=48, main="ACF: First Diff [Log(GDP)]",lwd=4,col="blue")

#PACF
Acf(dlGDP.p1, type="partial", lag=48, 
    main="PACF: First Diff [Log(GDP)]", lwd=4,col="blue")
# AR(2) or AR(3) or AR(6) or ARMA(4,4)

par(mfrow=c(1,1))


#------------------------------------------------
#Second Differencing
par(mfrow=c(1,2))
#ACF
Acf(diff(dlGDP.p1), type="correlation",
    lag=48, main="ACF: 2nd Diff [Log(GDP)]",lwd=4,col="blue")

#PACF
Acf(diff(dlGDP.p1), type="partial", lag=48, 
    main="PACF: 2nd Diff [Log(GDP)]", lwd=4,col="blue")
# AR(2) or AR(3) or AR(6) or ARMA(4,4)

par(mfrow=c(1,1))

#------------------------------------------------
# Unit Root Tests]=
library(urca)
summary(ur.kpss(lGDP.p1))
#The test statistic is much bigger than the 1% critical value,
#indicating that the null hypothesis is rejected. That is, the data are not stationary.

summary(ur.kpss(diff(lGDP.p1)))


#-------------------------------------------------
#Model Specification
library(astsa)

lGDP <- (log(zoodata$gdp))
lGDP.p1 <- window(lGDP, start="1960 Q1", end="2018 Q4")
lGDP.p2 <- window(lGDP, start="2019 Q1")

#-----------------------------------------------------------
#Model 1: ARIMA

#non-seasonal: ARIMA(2,2,3) 
a1 <- auto.arima(lGDP.p1, seasonal = FALSE, stationary = FALSE, stepwise = FALSE, ic ="aicc")
a1
summary(a1)


cat("\n\n RESIDUAL>>The real Logged GDP: ARMA(2,2,3) \n")
tsdiag(a1, gof.lag=36, lwd=3, col="blue") #NB

# Model 2: Growth Model
#Differenced
#Model 3

cat("\n\n The mean quarterly growth rate (1960Q1-2018Q4) is: ", round(mean(dlGDP.p1),7), "\n")
#non-seasonal: ARIMA (3,0,2)
da1 <- auto.arima(dlGDP.p1, seasonal = FALSE, stationary = TRUE, stepwise = FALSE, ic ="aicc")
da1
summary(da1)


cat("\n\n RESIDUAL>>The real GDP growth rate: ARMA(3,0,2) \n")

tsdiag(da1, gof.lag=36, lwd=3, col="blue") #NB

# Model Adequacy
#Check the estimated model for adequacy
library(forecast)
#Differenced
ar5 <- Arima(dlGDP.p1, order=c(3,0,2))
ar5
cat("\n\n The real GDP growth rate: ARMA(3,0,2) \n")
plot(ar5)

# ARIMA Forecasts
#MODEL: ARIMA(2,2,3)
arma22 <- Arima(lGDP.p1, order=c(2, 2, 3))
arma22.f <- forecast::forecast(arma22, length(lGDP.p2))
print(arma22.f) #Multistep


plot(arma22.f, type="o", pch=16, xlim=c(2008.00, 2022.00),
     ylim=c(15.00,15.65),main="ARIMA(2,2,3) Model Multistep Forecasts", ylab="Log of GDP", xlab="Quarter")
lines(arma22.f$mean, type="p", pch=16, lty="dashed", col="blue")
lines(lGDP.p2, type="o", pch=16, lty="dotted")

#Differenced

#Change growth rates to percentages
dlGDPperc=dlGDP*100
dlGDP.p2perc=dlGDP.p2*100
dlGDP.p1perc=dlGDP.p1*100
#Remember that RMSE and MASE will have to be divided by 100

#non-seasonal: ARIMA (3,0,2)
da1perc <- auto.arima(dlGDP.p1perc, seasonal = FALSE, stationary = TRUE, stepwise = FALSE, ic ="aicc")
da1perc
summary(da1perc)

cat("\n\n Real GDP Growth Rate: ARMA(3,0,2) \n")
#forecasts
da1.fperc <- forecast::forecast(da1perc, length(dlGDP.p2))
summary(da1.fperc)


plot(da1.fperc, type="o", pch=16,
     xlim=c(2008.00, 2022.00),
     main="ARMA(3,0,2) Model Multistep Forecasts",
     xlab="Quarter", ylab="Growth Rate(%)")
lines(da1.fperc$mean, type="p", pch=16, lty="dashed", col="blue")
lines(dlGDP.p2perc, type="o", pch=16, lty="dotted")

## The one-step rolling forecats
lgdp<-zoodata$lgdp
lgdp.p1 <- window(zoodata$lgdp, end=last.q)
lgdp.p2 <- window(zoodata$lgdp, start=last.q+0.25)

#ARMA(2, 2, 3)
rol.f <-zoo()
for(i in 1: length(lgdp.p2))
{
  y <- window(lgdp, end = last.q+(i-1)/4)
  rol.updt <- arima(y, order=c(2, 2, 3))
  rol.f <- c(rol.f, forecast::forecast(rol.updt, 1)$mean)
}  
rol.f <-as.ts(rol.f)

plot(rol.f, type="o", pch=15, xlim=c(2008.50, 2022.00), main="ARIMA(2,2,3) One Step Rolling Scheme Forecasts",
     ylab="Log of GDP", xlab="Quarter",col="green")
lines(rol.f, type="p", pch=15, lty="solid", col="red")
lines(lgdp, type="o", pch=16, lty="dotted")
lines(lgdp.p1, type="o", pch=16, lty="solid")


#Differenced
cat("\n\n Real GDP Growth Rate: ARIMA(3,0,2) \n")
drol5.f <-zoo()
for(i in 1: length(dlGDP.p2perc))
{
  y <- window(dlGDPperc, end = last.q+(i-1)/4)
  drol5.updt <- arima(y, order=c(3,0,2))
  drol5.f <- c(drol5.f, forecast::forecast(drol5.updt, 1)$mean)
}  
drol5.f <-as.ts(drol5.f)

plot(drol5.f, type="o", pch=15, xlim=c(2008.50, 2022.00), main="One Step Rolling Scheme Forecasts", 
     ylab="GDP growth rate (%)", xlab="Quarter",col="green")
lines(drol5.f, type="p", pch=15, lty="solid", col="red")
lines(dlGDPperc, type="o", pch=16, lty="dotted")
lines(dlGDP.p1perc, type="o", pch=16, lty="solid")


#------
drol5.fc <-zoo()
for(i in 1: length(dlGDP.p2))
{
  y <- window(dlGDP, end = last.q+(i-1)/4)
  drol5.updtc <- arima(y, order=c(3,0,2))
  drol5.fc <- c(drol5.fc, forecast::forecast(drol5.updtc, 1)$mean)
}  
drol5.fc <-as.ts(drol5.fc)
#------
### Comparison: Multistep forecast using ARMA(2,2) vs. One step rolling scheme forecast

plot(arma22.f, type="o", pch=16, xlim=c(2008.00, 2022.00),
     ylim=c(15.00,15.65),main="ARIMA(2,2,3) Model Multistep Forecasts vs. One Step Rolling", ylab="Log of GDP", xlab="Quarter")
lines(arma22.f$mean, type="p", pch=16, lty="dashed", col="blue")
lines(rol.f, type="b", pch=15, lty="solid", lwd=2, col="red")
lines(lGDP, type="o", pch=16, lty="dotted")

### Growth Rate: Multi-Step versus One Step

#Differenced
cat("\n\n Real GDP Growth Rate:  ARIMA(3,0,2) \n")

plot(da1.fperc, type="o", pch=16,
     xlim=c(2008.00, 2022.00), ylim=c(-10.80,2.50),
     main="ARMA(3,0,2) Model Multistep Forecasts vs. One Step Rolling",
     xlab="Quarter", ylab="Growth Rate(%)")
lines(da1.fperc$mean, type="p", pch=16, lty="dashed", col="blue")
lines(drol5.f, type="b", pch=15, lty="solid", lwd=2, col="red")
lines(dlGDPperc, type="o", pch=16, lty="dotted")

#------ARIMA ACCURACY CHECKS

#ARIMA(2,2,3) multi-step
cat("\n\n ARIMA(2,2,3) Multi-Step forecasts:\n")
accuracy(arma22.f$mean, lGDP.p2)

#One step rolling
cat("\n\n ARIMA(2,2,3)One Step rolling:\n")
accuracy(rol.f, lGDP.p2)


#DIFFERENCED

#ARIMA(3,0,2) multi-step
cat("\n\n ARIMA(3,0,2) Multi-Step forecasts:\n")
accuracy(da1.fperc$mean, dlGDP.p2perc)

#One step rolling
cat("\n\n ARIMA(3,0,2) One Step rolling:\n")
accuracy(drol5.f, dlGDP.p2perc)

#----------------

#-----------------------ARDL MODELS--------------------------------------------
## THE ARDL MODEL 
#-------------------------------------------------
#Model based on growth rate
#Convert to time series objects
#GDP
xtsdata<-as.xts(zoodata,order.by=mydata$quarter)
lgdp <- log(xtsdata$gdp)
lgdp.ts<-as.ts(lgdp)
lgdp.ts<-ts(lgdp.ts,frequency=4, start=c(1960,1))

dlgdp.ts <-diff(lgdp.ts)

dlgdp.ts_1<- ts(dplyr::lag(as.vector(dlgdp.ts),1),frequency=4, start=c(1960,2))
dlgdp.ts_2<- ts(dplyr::lag(as.vector(dlgdp.ts),2),frequency=4, start=c(1960,2))
dlgdp.ts_3<- ts(dplyr::lag(as.vector(dlgdp.ts),3),frequency=4, start=c(1960,2))
dlgdp.ts_4<- ts(dplyr::lag(as.vector(dlgdp.ts),4),frequency=4, start=c(1960,2))

#------------------------------------
# Consumption
lcons <- log(xtsdata$cons)
lcons.ts<-as.ts(lcons)
lcons.ts<-ts(lcons.ts,frequency=4, start=c(1960,1))

dlcons.ts <-diff(lcons.ts)

dlcons.ts_1<- ts(dplyr::lag(as.vector(dlcons.ts),1),frequency=4, start=c(1960,2))
dlcons.ts_2<- ts(dplyr::lag(as.vector(dlcons.ts),2),frequency=4, start=c(1960,2))
dlcons.ts_3<- ts(dplyr::lag(as.vector(dlcons.ts),3),frequency=4, start=c(1960,2))
dlcons.ts_4<- ts(dplyr::lag(as.vector(dlcons.ts),4),frequency=4, start=c(1960,2))

#---------------------------------------------
# Investment
linv <- log(xtsdata$inv)
linv.ts<-as.ts(linv)
linv.ts<-ts(linv.ts,frequency=4, start=c(1960,1))

dlinv.ts <-diff(linv.ts)

dlinv.ts_1<- ts(dplyr::lag(as.vector(dlinv.ts),1),frequency=4, start=c(1960,2))
dlinv.ts_2<- ts(dplyr::lag(as.vector(dlinv.ts),2),frequency=4, start=c(1960,2))
dlinv.ts_3<- ts(dplyr::lag(as.vector(dlinv.ts),3),frequency=4, start=c(1960,2))
dlinv.ts_4<- ts(dplyr::lag(as.vector(dlinv.ts),4),frequency=4, start=c(1960,2))

#---------------------------------------------
# government 
lgov <- log(xtsdata$gov)
lgov.ts<-as.ts(lgov)
lgov.ts<-ts(lgov.ts,frequency=4, start=c(1960,1))

dlgov.ts <-diff(lgov.ts)

dlgov.ts_1<- ts(dplyr::lag(as.vector(dlgov.ts),1),frequency=4, start=c(1960,2))
dlgov.ts_2<- ts(dplyr::lag(as.vector(dlgov.ts),2),frequency=4, start=c(1960,2))
dlgov.ts_3<- ts(dplyr::lag(as.vector(dlgov.ts),3),frequency=4, start=c(1960,2))
dlgov.ts_4<- ts(dplyr::lag(as.vector(dlgov.ts),4),frequency=4, start=c(1960,2))

#---------------------------------------------------
# Yield
yield <- log(xtsdata$yield)
yield.ts<-as.ts(yield)
yield.ts<-ts(yield.ts,frequency=4, start=c(1960,1))

dyield.ts <-diff(yield.ts)

dyield.ts_1<- ts(dplyr::lag(as.vector(dyield.ts),1),frequency=4, start=c(1960,2))
dyield.ts_2<- ts(dplyr::lag(as.vector(dyield.ts),2),frequency=4, start=c(1960,2))
dyield.ts_3<- ts(dplyr::lag(as.vector(dyield.ts),3),frequency=4, start=c(1960,2))
dyield.ts_4<- ts(dplyr::lag(as.vector(dyield.ts),4),frequency=4, start=c(1960,2))

time<-ts(1:248,frequency=4, start=c(1960,1))

ardsample<-cbind(dlgdp.ts,dlgdp.ts_1,dlgdp.ts_2,dlgdp.ts_3,dlgdp.ts_4,               dlcons.ts,dlcons.ts_1,dlcons.ts_2,dlcons.ts_3,dlcons.ts_4,     dlinv.ts,dlinv.ts_1,dlinv.ts_2,dlinv.ts_3,dlinv.ts_4,
                 dlgov.ts,dlgov.ts_1,dlgov.ts_2,dlgov.ts_3,dlgov.ts_4,           dyield.ts,dyield.ts_1,dyield.ts_2,dyield.ts_3,dyield.ts_4,time)

#------- AR(4) check
acf(dlgdp.ts[1:236])
pacf(dlgdp.ts[1:236])

ar4 <-lm(dlgdp.ts ~ dlgdp.ts_1 + dlgdp.ts_2 + dlgdp.ts_3 +
           dlgdp.ts_4, data = ardsample, subset=time<237)

summary(ar4)
stargazer(ar4, type = "text")

# Let's test for serial correlation
ar4_sc <- lm(ar4$residuals ~ dplyr::lag(ar4$residuals,1))
stargazer(ar4_sc, type = "text")
#Not significant: No first order serial correlation

ar4_sc_summary <- summary(ar4_sc)

# Calculate t-statistic

ar4_sc_summary$coefficients[2,1]/ar4_sc_summary$coefficients[2,2]

# Great! Small t-stat, no serial correlation
# Unsurprising, given the first difference

#Compute BIC
BIC <- function(model) {
  
  ssr <- sum(model$residuals^2)
  t <- length(model$residuals)
  npar <- length(model$coef)
  BIC <- log(ssr/t) + npar * log(t)/t
  return(BIC)
}

BIC(ar4)



#------------------------------------------------
pacf(dlgdp.ts)
#2,3,4,5
pacf(dlcons.ts)
#1 or 4, 5
pacf(dlinv.ts)
#2 or 3
pacf(dlgov.ts)
#1 or 2
pacf(dyield.ts)
#1 or 2

#Chose the significant ones
# ARDL (2,2,2)

ardl222 <- lm(dlgdp.ts ~ dlgdp.ts_1 + dlgdp.ts_2+
                dlcons.ts_1 + dlcons.ts_2+
                dlgov.ts_1+dlgov.ts_2 ,
              data = ardsample, subset=time<237)


#----------------

summary(ardl222 )
stargazer(ardl222 , type = "text")

# Let's test for serial correlation
ardl222_sc <- lm(ardl222$residuals ~ dplyr::lag(ardl222$residuals,1))
stargazer(ardl222_sc, type = "text")

ardl222_sc_summary <- summary(ardl222_sc)

# Calculate t-statistic

ardl222_sc_summary$coefficients[2,1]/ardl222_sc_summary$coefficients[2,2]

#Again not significant. No presence of serial correlation
#----------------


ardl232 <- lm(dlgdp.ts ~ dlgdp.ts_1 + dlgdp.ts_2+
                dlcons.ts_1 + dlcons.ts_2+ dlcons.ts_3+
                dlgov.ts_1+dlgov.ts_2 ,
              data = ardsample, subset=time<237)

#----------------

summary(ardl232 )
stargazer(ardl232 , type = "text")

# Let's test for serial correlation
ardl232_sc <- lm(ardl232$residuals ~ dplyr::lag(ardl232$residuals,1))
stargazer(ardl232_sc, type = "text")

ardl232_sc_summary <- summary(ardl232_sc)

# Calculate t-statistic

ardl232_sc_summary$coefficients[2,1]/ardl232_sc_summary$coefficients[2,2]

#Again not significant. No presence of serial correlation
#----------------

#IN SAMPLE RMSE
cat("\n\n The In Sample RMSE: AR(4) or ARDL(4,0,0): \n")
accuracy(ar4)

cat("\n\n The In Sample RMSE: AR(4) or ARDL(4,0,0): \n")
accuracy(ardl222)

cat("\n\n The In Sample RMSE: AR(4) or ARDL(4,0,0): \n")
accuracy(ardl232)

################################################################################
#
# Creating AR(4), ARDL(2,2,2) and ARDL(2,3,2) forecasts
#
################################################################################
#Test sample data
testsample<-ardsample[which(ardsample[,"time"] >= 237),]
testsample<-ts(testsample, frequency=4, start=c(2019,1))
head(testsample)

#Forecasting
#*AR(4)
forecast_ar4 <- predict(ar4, newdata = testsample, interval = "prediction")
head(forecast_ar4)
#* ARDL 222
forecast_ardl222 <- predict(ardl222, newdata = testsample, interval = "prediction")
head(forecast_ardl222)
#* ARDL 232
forecast_ardl232 <- predict(ardl232, newdata = testsample, interval = "prediction")
head(forecast_ardl232 )

#COMPUTE the OUT of SAMPLE RMSE
#* AR(4)
forecast_error_ar4 <- testsample[,"dlgdp.ts"] - forecast_ar4[,1]
head(forecast_error_ar4)
# Save RMSE
rmse_ar4 <- sqrt(mean(forecast_error_ar4^2))
rmse_ar4

# Calculate MAE
mean(abs(forecast_error_ar4))



#* ARDL(2,2,2)
forecast_error_ardl222 <- testsample[,"dlgdp.ts"] - forecast_ardl222[,1]
head(forecast_error_ardl222)
#RMSE
rmse_ardl222 <- sqrt(mean(forecast_error_ardl222^2))
rmse_ardl222
# Calculate MAE
mean(abs(forecast_error_ardl222))

#* ARDL(2,3,2)
forecast_error_ardl232 <- testsample[,"dlgdp.ts"] - forecast_ardl232[,1]
head(forecast_error_ardl232)
#RMSE
rmse_ardl232 <- sqrt(mean(forecast_error_ardl232^2))
rmse_ardl232
# Calculate MAE
mean(abs(forecast_error_ardl232))

#ARDL(2,3,2) has the lowest RMSE and MAE

################################################################################
#
# ARDL Plots
#
################################################################################
forecast_ar4<-ts(forecast_ar4,frequency = 4, start=c(2019,1))
forecast_ardl222<-ts(forecast_ardl222,frequency = 4, start=c(2019,1))
forecast_ardl232<-ts(forecast_ardl232,frequency = 4, start=c(2019,1))

plot(testsample[,"dlgdp.ts"]*100,
     ylim = c(-20, 15),
     main = "",
     ylab = "GDP growth rate (%)",
     xlab = "Quarter", lwd=4,
     type = "l")

# Adding a line in red for the forecast
lines( forecast_ardl232[,1]*100,  type="b", pch=15, lty="solid", lwd=2, col="red")

# Adding a dashed line in blue for the 95% lower bound

lines(forecast_ardl232[,2]*100,
      col = "blue",
      lty=2)


# Adding a dashed line in blue for the 95% upper bound
lines(forecast_ardl232[,3]*100,
      col = "blue",
      lty=2)

#Add the ARDL(2,2,2) green
lines(forecast_ardl222[,1]*100,  type="b", pch=15, lty="solid", lwd=2, col="green")

#Add the ARDL(4,0,0) gold
lines(forecast_ar4[,1]*100,  type="b", pch=15, lty="solid", lwd=2, col="gold")



# Adding a legend
legend("bottomleft", legend=c("GDP", "ARDL(2,3,2) Forecast","ARDL(2,2,2) Forecast", "AR(4) Forecast","ARDL(2,3,2) 95% Confdence Interval"),
       col=c("black", "red", "green","gold","blue"), lty=c(1,1,2), cex=0.8)

#-------------------------------------------------

#-----------------------------------------------------------------------------




#
###############################################################
# END SCRIPT: Yaseen Alli
#
###############################################################