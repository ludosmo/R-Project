' CALL OF PACKAGES'

pack<-c("openxlsx","quantmod","car","readxl","ggplot2", "vtable","tidyverse", "jtools", "e1071", "tseries", 
        "ggplot2", "plotly", "fRegression","lmtest", "fGarch", "vars", "FinTS", "moments", "rugarch", "sandwich", "rmgarch",
         "urca", "xts") # Here we list the packages we want to install or add to the library  

install.packages(pack) #activate this line if you want to install the packages.

lapply(pack, require, character.only = TRUE) #Always recall the packages to be used once installed

'LOAD DATA'
#TICKER SYMBOLS
symbols<-c("^GSPC","^DJI","^IXIC") 

#DATE RANGE
start_date <- as.Date("2013-12-01")
end_date <- as.Date("2023-12-02") # here we put 2023-12-02 otherwise it will not take the 1st jan 23

getSymbols(symbols, src = "yahoo", from = start_date, to = end_date)

# Convert Data into Datafram 
GSPC <- data.frame(Date=index(GSPC), coredata(GSPC))
DJI <- data.frame(Date=index(DJI), coredata(DJI))
IXIC <- data.frame(Date=index(IXIC), coredata(IXIC))

# Create the workbook
wb <- createWorkbook()

# Split the indexe into severals sheets
addWorksheet(wb, "GSPC")
writeData(wb, "GSPC", GSPC)

addWorksheet(wb, "DJI")
writeData(wb, "DJI", DJI)

addWorksheet(wb, "IXIC")
writeData(wb, "IXIC", IXIC)

# CHANGE THE PATH !!! 
save_path <- "C:\\Users\\ludos\\Documents\\Econometrics\\databaseUSIndices.xlsx"

# SaVE WORKBOOK
saveWorkbook(wb, save_path, overwrite = TRUE)


#TASK 1.1 and 1.2

####################### (Re)Naming Variables##
##DJI##
names(DJI)[1] = "DateDJI"
names(DJI)[5] = "ClosingPriceDJI"

DateDJI<-DJI$Date
ClosingPriceDJI<-DJI$ClosingPrice

##GSPC##
names(GSPC)[1] = "DateGSPC"
names(GSPC)[5] = "ClosingPriceGSPC"

DateGSPC<-GSPC$Date
ClosingPriceGSPC<-GSPC$ClosingPrice

##IXIC##
names(IXIC)[1] = "DateIXIC"
names(IXIC)[5] = "ClosingPriceIXIC"

DateIXIC<-IXIC$Date
ClosingPriceIXIC<-IXIC$ClosingPrice
##END OF naming Variables##

#Ploting the Series 

plot(DateDJI, ClosingPriceDJI, type = 'l', xlab =" Date ",ylab =" Closing Prices",main = " Closing Prices of Dow Jones Industrial Average over time")
plot(DateGSPC, ClosingPriceGSPC, type = 'l', xlab =" Date ",ylab =" Closing Prices",main = " Closing Prices of the S&P500 Average over time")
plot(DateIXIC, ClosingPriceIXIC, type = 'l', xlab =" Date ",ylab =" Closing Prices",main = " Closing Prices of the Nasdaq Average over time")

#PLOT THE SERIES ON THE SAME GRAPH
plot(DateDJI, ClosingPriceDJI, type = 'l', col = 'blue', 
     xlab ="Date", ylab ="Closing Prices for diferrent US INDEX", 
     main = "Closing Prices over time",
     ylim = range(c(ClosingPriceDJI, ClosingPriceGSPC, ClosingPriceIXIC)))

lines(DateGSPC, ClosingPriceGSPC, type = 'l', col = 'red')
lines(DateIXIC, ClosingPriceIXIC, type = 'l', col = 'green')
legend("topright", legend = c("Dow Jones", "S&P 500", "NASDAQ"), 
       col = c("blue", "red", "green"), lty = 1, cex = 0.8)


#Calculate somes metrics for the dynamics
#Compute the returns over the last 10 years
Return_DJI <- (DJI$ClosingPriceDJI[nrow(DJI)] / DJI$ClosingPriceDJI[1] - 1) * 100
Return_GSPC <- (GSPC$ClosingPriceGSPC[nrow(GSPC)] / GSPC$ClosingPriceGSPC[1] - 1) * 100
Return_IXIC <- (IXIC$ClosingPriceIXIC[nrow(IXIC)] / IXIC$ClosingPriceIXIC[1] - 1) * 100

print(paste("Return of the Dow Jones over the last 10 years is :" ,Return_DJI, "%"))

print(paste("Return of the S&P500 over the last 10 years is :" ,Return_GSPC, "%"))

print(paste("Return of the Nasdaq over the last 10 years is :" ,Return_IXIC, "%"))

# Check
start_Date <- DateDJI[1]
end_Date <- DateDJI[length(DateDJI)]  
par(cex.axis = 1.5, cex.lab = 1.5, lwd = 5)

plot(DateDJI, ClosingPriceDJI, type = 'l', xlab =" Date ",ylab =" ClosingPriceDJI ",
     xlim=c(start_Date, end_Date), ylim=c(15000,45000))

############ TASK 1.2 ###########################
#Create log returns DJI
delDJI <- log(DJI$ClosingPriceDJI)
lClosingPriceDJI<- log(ClosingPriceDJI)
R_ClosingPriceDJI <- diff(log(ClosingPriceDJI),lag=1)
rm(delDJI)

#Baic descriptive statistics DJI 
sd(R_ClosingPriceDJI) # standard deviation
summary(R_ClosingPriceDJI,1) # summary statistics

#Plot DJI
plot(DateDJI[2:length(DateDJI)], R_ClosingPriceDJI, type = 'l', xlab =" Date ",ylab ="DJI RETURNS",
     xlim=c(start_date, end_date), ylim=c(-0.15, 0.15))

hist(R_ClosingPriceDJI,breaks=40,main = "Distribution of the logs returns of the Dow Jones Indistrial Average",xlab = "Log Returns") # histogram

boxplot(R_ClosingPriceDJI,main ="logs returns of the Dow Jones Indistrial Average",ylab = "log returns",col = "lightblue",border = "blue" )#Box plot


#Create log returns GSPC
delGSPC <- log(GSPC$ClosingPriceGSPC)
lClosingPriceGSPC<- log(ClosingPriceGSPC)
R_ClosingPriceGSPC <- diff(log(ClosingPriceGSPC),lag=1)
rm(delGSPC)

#Baic descriptive statistics GSPC
sd(R_ClosingPriceGSPC) # standard deviation
summary(R_ClosingPriceGSPC,1) # summary statistics

#Plot GSPC
plot(DateGSPC[2:length(DateGSPC)], R_ClosingPriceGSPC, type = 'l', xlab =" Date ",ylab ="GSPC RETURNS",
     xlim=c(start_date, end_date), ylim=c(-0.15, 0.15))

hist(R_ClosingPriceGSPC,breaks=40,main = "Distribution of the logs returns of the S&P500",xlab = "Log Returns") # histogram

boxplot(R_ClosingPriceGSPC,main ="logs returns of S&P500",ylab = "log returns",col = "purple",border = "red" )#Box plot

#Create log returns IXIC
delIXIC <- log(IXIC$ClosingPriceIXIC)
lClosingPriceIXIC<- log(ClosingPriceIXIC)
R_ClosingPriceIXIC <- diff(log(ClosingPriceIXIC),lag=1)
rm(delIXIC)

#Baic descriptive statistics IXIC
sd(R_ClosingPriceIXIC) # standard deviation
summary(R_ClosingPriceIXIC,1) # summary statistics

#Plot GSPC
plot(DateIXIC[2:length(DateIXIC)], R_ClosingPriceIXIC, type = 'l', xlab =" Date ",ylab ="IXIC RETURNS",
     xlim=c(start_date, end_date), ylim=c(-0.15, 0.15))

hist(R_ClosingPriceIXIC,breaks=40,main = "Distribution of the logs returns of the Nasdaq Composite",xlab = "Log Returns") # histogram

boxplot(R_ClosingPriceIXIC,main ="logs returns of Nasdaq Composite",ylab = "log returns",col = "grey",border = "black" )#Box plot


# Plot the series to compare 
'STORE DATA TOGETHER'
data_combined <- data.frame(
  Returns = c(R_ClosingPriceDJI, R_ClosingPriceGSPC, R_ClosingPriceIXIC),
  Series = factor(rep(c("DJI", "GSPC", "IXIC"), each = length(R_ClosingPriceDJI)))
)

#BOX PLOT COMPARISON
boxplot(Returns ~ Series, data = data_combined, 
        main = "Log Returns of DJI,GSPC,IXIC",
        xlab = "INDEX", 
        ylab = "Log Returns",
        col = c("red", "green", "blue"), 
        ylim = c(-0.15, 0.15))


##TASK 2.1
# check if the series are : s are: stationary, serially correlated, homoscedastic, and normally distributed. 
pack2<-c("lmtest", "fGarch", "vars", "FinTS", "moments", "rugarch", "sandwich", "rmgarch",
         "urca", "xts","forecast") 
install.packages(pack2)
lapply(pack2, require, character.only = TRUE) 

#Test of Stationnarity

print(adf.test(R_ClosingPriceDJI))
print(adf.test(R_ClosingPriceGSPC))
print(adf.test(R_ClosingPriceIXIC))

print("all the series are stationary at alpha = 0.05")

#Serial Correlation 

print(Box.test(R_ClosingPriceDJI, type = "Ljung-Box"))
print(Box.test(R_ClosingPriceGSPC, type = "Ljung-Box"))
print(Box.test(R_ClosingPriceIXIC, type = "Ljung-Box"))

print("all series are auto correlated at alpha = 0.05")

#ar_model <- ar(R_ClosingPriceDJI, order.max = 1, method = "mle")
#summary(ar_model)

#test of homoscedasticity

model_DJI = lm(R_ClosingPriceDJI ~ lag(R_ClosingPriceDJI, 1), data = data.frame(R_ClosingPriceDJI))
model_GSPC = lm(R_ClosingPriceGSPC ~ lag(R_ClosingPriceGSPC, 1), data = data.frame(R_ClosingPriceGSPC))
model_IXIC = lm(R_ClosingPriceIXIC ~ lag(R_ClosingPriceIXIC, 1), data = data.frame(R_ClosingPriceIXIC))

print(bptest(model_DJI))
print(bptest(model_GSPC))
print(bptest(model_IXIC))

print("In all 3 models we don't see Homoscedasticity since P-value isn't greater than 0.05")

#Normally distributed ?

#jarque.test(R_ClosingPriceDJI)
#skewness(R_ClosingPriceDJI)
#kurtosis(R_ClosingPriceDJI)
#jarque.test(R_ClosingPriceGSPC)
#jarque.test(R_ClosingPriceIXIC)

print(paste("Skewness:", skewness(R_ClosingPriceDJI)))
print(paste("Kurtosis:", kurtosis(R_ClosingPriceDJI)))
print(jarque.bera.test(R_ClosingPriceDJI))

print(paste("Skewness:", skewness(R_ClosingPriceGSPC)))
print(paste("Kurtosis:", kurtosis(R_ClosingPriceGSPC)))
print(jarque.bera.test(R_ClosingPriceGSPC))

print(paste("Skewness:", skewness(R_ClosingPriceIXIC)))
print(paste("Kurtosis:", kurtosis(R_ClosingPriceIXIC)))
print(jarque.bera.test(R_ClosingPriceIXIC))

#Task2.2

par(mfrow=c(1,2))

acf(R_ClosingPriceDJI, lag.max=NULL, cex.axis=1.5, cex.main=1.5,cex.lab=1.5,lwd=5, 
    main = 'DJI log return', col="red")
pacf(R_ClosingPriceDJI, lag.max=NULL, cex.axis=1.5, cex.main=1.5,cex.lab=1.5,lwd=5, 
     main = 'DJI log return', col="red")
acf(R_ClosingPriceGSPC, lag.max=NULL, cex.axis=1.5, cex.main=1.5,cex.lab=1.5,lwd=5, 
    main = 'GSPC log return', col="red")
pacf(R_ClosingPriceGSPC, lag.max=NULL, cex.axis=1.5, cex.main=1.5,cex.lab=1.5,lwd=5, 
     main = 'GPSC Log-return', col="red")
acf(R_ClosingPriceIXIC, lag.max=NULL, cex.axis=1.5, cex.main=1.5,cex.lab=1.5,lwd=5, 
    main = 'IXIC log return', col="red")
pacf(R_ClosingPriceIXIC, lag.max=NULL, cex.axis=1.5, cex.main=1.5,cex.lab=1.5,lwd=5, 
     main = 'IXIC Log-Return', col="red")


#we use auto arima function to fit the best information critera that we chose here -> bic
#its iterate between a lot of model to give us the best based on the ic and the datas

# AutoArma for DJI log reutnrs
ArmaDJI <- auto.arima(R_ClosingPriceDJI, approximation=FALSE, trace=TRUE,ic = "bic")
# AutoArma for GSPC log reutnrs
ArmaGSPC <- auto.arima(R_ClosingPriceGSPC, approximation=FALSE, trace=TRUE,ic = "bic")
# AutoArma for IXIC log reutnrs
ArmaIXIC <- auto.arima(R_ClosingPriceIXIC, approximation=FALSE, trace=TRUE, ic = "bic")

summary(ArmaDJI)#deeper check
summary(ArmaGSPC)
summary(ArmaIXIC)

#Task 2.3 Implementation of univeriate GARCH models

#BEFORE RUNNING GARCH MODEL WE HAVE TO CHECK GARCH EFFECT
#TEST ARCH EFFECT 
#ar_model <- ar(R_ClosingPriceDJI, order.max = 1, method = "mle")

#DJI
model_DJI = lm(R_ClosingPriceDJI ~ lag(R_ClosingPriceDJI, 1), data = data.frame(R_ClosingPriceDJI))
ErrorTerms<-model_DJI$residuals # Take the residuals
ArchTest(ErrorTerms, lags=60, demean = TRUE) # ARCH test for the null hypothesis of homoscedasticity 

#GSPC
model_GSPC = lm(R_ClosingPriceGSPC ~ lag(R_ClosingPriceGSPC, 1), data = data.frame(R_ClosingPriceGSPC))
ErrorTerms<-model_GSPC$residuals # Take the residuals
ArchTest(ErrorTerms, lags=60, demean = TRUE) # ARCH test for the null hypothesis of homoscedasticity 

#IXIC
model_IXIC = lm(R_ClosingPriceIXIC ~ lag(R_ClosingPriceIXIC, 1), data = data.frame(R_ClosingPriceIXIC))
ErrorTerms<-model_IXIC$residuals # Take the residuals
ArchTest(ErrorTerms, lags=60, demean = TRUE) # ARCH test for the null hypothesis of homoscedasticity 

#First we check for the DJI.

######## GARCH DJI ######

# Create our ARMA-GARCH model structure
arma.garch= ugarchspec(mean.model=list(armaOrder=c(2,2)),
                       variance.model=list(garchOrder=c(1,1)),
                       distribution.model = "norm")

# Estimate our GARCH model
DJI.arma.garch = ugarchfit(data=R_ClosingPriceDJI, spec=arma.garch)
show(DJI.arma.garch)

# Check if the standardized residuals are normally distributed and not serially correlated
StandRes <- ts(residuals(DJI.arma.garch, standardize=TRUE))
plot(DateDJI[2:2518], StandRes,type="l", xlab =" Date ")

#Plot the volatility
volatility_estimates <- sigma(DJI.arma.garch)
plot(DateDJI[2:2518], volatility_estimates, type="l", xlab="Date", ylab="Volatility",main = "Volatiliy captured by GARCH MODEL for the Dow Jones",col = "red")



# Create our Asymmetric ARMA-GARCH model structure to CHECK FOR LEVERAGE AND ASYMETRI
arma.gjr= ugarchspec(mean.model=list(armaOrder=c(2,2)),
                     variance.model = list(model = "gjrGARCH",
                                           garchOrder = c(1,1)), distribution.model = "norm")

# Estimate our GARCH model
DJI.arma.gjr = ugarchfit(data=R_ClosingPriceDJI, arma.gjr)
show(DJI.arma.gjr)

# Check if the standardized residuals are normally distributed and not serially correlated
StandRes <- ts(residuals(DJI.arma.gjr, standardize=TRUE))
plot(DateDJI[2:2518], StandRes,type="l", xlab =" Date ")

#Plot the volatility
volatility_estimates <- sigma(DJI.arma.gjr)
plot(DateDJI[2:2518], volatility_estimates, type="l", xlab="Date", ylab="Volatility",main = "Volatiliy captured by GARCH MODEL for the Dow Jones",col = "red")


###### GARCH GSPC #####

# Create our Asymmetric ARMA-GARCH model structure to CHECK FOR LEVERAGE AND ASYMETRI
arma.gjr= ugarchspec(mean.model=list(armaOrder=c(2,2)),
                     variance.model = list(model = "gjrGARCH",
                                           garchOrder = c(1,1)), distribution.model = "norm")

# Estimate our GARCH model
GSPC.arma.gjr = ugarchfit(data=R_ClosingPriceGSPC, arma.gjr)
show(GSPC.arma.gjr)

# Check if the standardized residuals are normally distributed and not serially correlated
StandRes <- ts(residuals(GSPC.arma.gjr, standardize=TRUE))
plot(DateDJI[2:2518], StandRes,type="l", xlab =" Date ")

#Plot the volatility
volatility_estimates <- sigma(GSPC.arma.gjr)
plot(DateGSPC[2:2518], volatility_estimates, type="l", xlab="Date", ylab="Volatility",main = "Volatiliy captured by GARCH MODEL for the S&P 500",col = "blue")


###### GARCH IXIC #####

# Create our Asymmetric ARMA-GARCH model structure to CHECK FOR LEVERAGE AND ASYMETRI
arma.gjr= ugarchspec(mean.model=list(armaOrder=c(1,0)),
                     variance.model = list(model = "gjrGARCH",
                                           garchOrder = c(1,1)), distribution.model = "norm")

# Estimate our GARCH model
IXIC.arma.gjr = ugarchfit(data=R_ClosingPriceIXIC, arma.gjr)
show(IXIC.arma.gjr)

# Check if the standardized residuals are normally distributed and not serially correlated
StandRes <- ts(residuals(IXIC.arma.gjr, standardize=TRUE))
plot(DateDJI[2:2518], StandRes,type="l", xlab =" Date ")

#Plot the volatility
volatility_estimates <- sigma(IXIC.arma.gjr)
plot(DateIXIC[2:2518], volatility_estimates, type="l", xlab="Date", ylab="Volatility",main = "Volatiliy captured by GARCH MODEL for the Nasdaq Composite",col = "Purple")


#Task2.4
#ESTIMATION OF AN GARCH-DCC MODEL

#we define our GARCH MODEL
model1=ugarchspec(mean.model = list(armaOrder=c(2,2)),variance.model = list(garchOrder = c(1,1),model="sGARCH"),distribution.model = "norm")
#implement DCC GARCH MODEL
#each variable in the system is modelled separately as a univariate GARCH process
modelspec = dccspec(uspec=multispec(replicate(3,model1)),dccOrder=c(1,1),distribution = "mvnorm") 
#FIT THE DATAS IN OUR MODEL
modelfit = dccfit(modelspec,data=data.frame(R_ClosingPriceDJI,R_ClosingPriceGSPC,R_ClosingPriceIXIC))
show(modelfit)

#extract correlation and voariance metrix
correlation = rcor(modelfit)
dim(correlation)

#view the matrix correlation
correlation[,,dim(correlation)[3]]

#we create our correlation relationship - first DJI/GSPC

DJI_GSPC_Correlation = correlation[2,1,] 
DJI_IXIC_Correlation = correlation[3,1,]
GSPC_IXIC_Correlation = correlation[2,3,]


#plot the correlation
plot(DateDJI[2:2518],DJI_GSPC_Correlation,main="correlation between Dow Jones and S&p 500",col = "red",xlab = "Date",ylab="Conditional correlation",type = "l")
plot(DateDJI[2:2518],DJI_IXIC_Correlation,main="correlation between Dow Jones and Nasdaq Composite",col = "blue",xlab = "Date",ylab="Conditional correlation",type = "l")
plot(DateIXIC[2:2518],GSPC_IXIC_Correlation,main="correlation between S&P500 and Nasdaq Composite",col = "purple",xlab = "Date",ylab="Conditional correlation",type = "l")

plot(modelfit, which=4)


#Task 3.1
# we are going to test the index in pairs using Engle-Granger Approach.
logDJI <- log(ClosingPriceDJI) # transform our variables into log price
logGSPC <- log(ClosingPriceGSPC)
logIXIC <- log(ClosingPriceIXIC)

#PLOT THE SERIES ON THE SAME GRAPH
plot(DateDJI, logDJI, type = 'l', col = 'blue', 
     xlab ="Date", ylab ="log Prices for diferrent US INDEX", 
     main = "Log Prices over time",
     ylim = range(c(logDJI, logGSPC, logIXIC)))

lines(DateGSPC, logGSPC, type = 'l', col = 'red')
lines(DateIXIC, logIXIC, type = 'l', col = 'green')
legend("topright", legend = c("Dow Jones", "S&P 500", "NASDAQ"), 
       col = c("blue", "red", "green"), lty = 1, cex = 0.8)




# HERE WE WANT TO CHECK THE TYPE OF NON-STATIONARITY OF OUR SERIES 
par(mfrow=c(3,1))
plot(logDJI)
acf(logDJI, lag.max=NULL, cex.axis=1.5, cex.main=1.5,cex.lab=1,lwd=5, 
    main = 'DJI log prices', col="red")
pacf(logDJI, lag.max=NULL, cex.axis=1.5, cex.main=1.5,cex.lab=1.5,lwd=5, 
     main = 'DJI log prices', col="red")

par(mfrow=c(3,1))
plot(logGSPC)
acf(logGSPC, lag.max=NULL, cex.axis=1.5, cex.main=1.5,cex.lab=1.5,lwd=5, 
   main = 'GSPC log price', col="red")
pacf(logGSPC, lag.max=NULL, cex.axis=1.5, cex.main=1.5,cex.lab=1.5,lwd=5, 
     main = 'GPSC Log-price', col="red")

par(mfrow=c(3,1))
plot(logIXIC)
acf(logIXIC, lag.max=NULL, cex.axis=1.5, cex.main=1.5,cex.lab=1.5,lwd=5, 
    main = 'IXIC Log-Price', col="red")
pacf(logIXIC, lag.max=NULL, cex.axis=1.5, cex.main=1.5,cex.lab=1.5,lwd=5, 
     main = 'IXIC Log-Price', col="red")

"before doing the coingreation, we tet if our vriable aare l(1)"


UnitRootTest<-ur.df(logDJI, type = "drift", selectlags = c("BIC")) # We are performi a ADF tets
summary(UnitRootTest)
UnitRootTest<-ur.df(logGSPC, type = "drift", selectlags = c("BIC")) # We are performi a ADF tets
summary(UnitRootTest)
UnitRootTest<-ur.df(logIXIC, type = "drift", selectlags = c("BIC")) # We are performi a ADF tets
summary(UnitRootTest)

# we gonna tet for eac Pair of indexes : 

################### DJI/GPSPC ###########

########      1.2 Estimating the cointegrating relationship using OLS
Log_Prices<-data.frame(logDJI, logGSPC)
Coint_Eq = lm( logDJI ~ logGSPC , data = Log_Prices)
summary(Coint_Eq)
########      1.3 Save the residuals of the cointegrating relationship
Resids <-  Coint_Eq$residuals
par(mfrow=c(1,1))
plot(Resids,main ="Reidual for DJI/GSPC")
########      1.4 Test the residuals if they are I(0)
UnitRootTest<-ur.df(Resids, type = "none", selectlags = c("BIC"))
summary(UnitRootTest)
print("Residuals are  I(1) aat 5 % confindence level")



# we cange te independent and dependend variable : GSPC/DJI
########      1.2 Estimating the cointegrating relationship using OLS
Log_Prices<-data.frame(logGSPC, logDJI)
Coint_Eq = lm( logGSPC ~ logDJI , data = Log_Prices)
summary(Coint_Eq)
########      1.3 Save the residuals of the cointegrating relationship
Resids <-  Coint_Eq$residuals
par(mfrow=c(1,1))
plot(Resids)
########      1.4 Test the residuals if they are I(0)
UnitRootTest<-ur.df(Resids, type = "none", selectlags = c("BIC"))
summary(UnitRootTest)
print("Residuals are  I(1) aat 5 % confindence level")




################## TEST BETWEEN DJI AND IXIC ##################
# DJI/IXIC
########      1.2 Estimating the cointegrating relationship using OLS
Log_Prices<-data.frame(logDJI, logIXIC)
Coint_Eq = lm( logDJI ~ logIXIC , data = Log_Prices)
summary(Coint_Eq)
########      1.3 Save the residuals of the cointegrating relationship
Resids <-  Coint_Eq$residuals
par(mfrow=c(1,1))
plot(Resids)
########      1.4 Test the residuals if they are I(0)
UnitRootTest<-ur.df(Resids, type = "none", selectlags = c("BIC"))
summary(UnitRootTest)
print("Residual are I(0)")
#here we have cointegrated pairs so we do the 3.3 task : ECM'''
######## Step 2 Estimate the ECM
DL_DJI=diff(logDJI)
DL_IXIC=diff(logIXIC)
ResidsAdj <-Resids[1:length(DL_IXIC)]
ECM <- lm(DL_DJI~DL_IXIC+ResidsAdj)
summary(ECM)

#change dependent and independant variable : IXIC/DJI
########      1.2 Estimating the cointegrating relationship using OLS
Log_Prices<-data.frame(logIXIC, logDJI)
Coint_Eq = lm( logIXIC ~ logDJI , data = Log_Prices)
summary(Coint_Eq)
########      1.3 Save the residuals of the cointegrating relationship
Resids <-  Coint_Eq$residuals
par(mfrow=c(1,1))
plot(Resids)
########      1.4 Test the residuals if they are I(0)
UnitRootTest<-ur.df(Resids, type = "none", selectlags = c("BIC"))
summary(UnitRootTest)
print("Here we find that the residual are I(0)")
#here we have cointegrated pairs so we do the 3.3 task : ECM'''
######## Step 2 Estimate the ECM
DL_DJI=diff(logDJI)
DL_IXIC=diff(logIXIC)
ResidsAdj <-Resids[1:length(DL_DJI)]
ECM <- lm(DL_IXIC~DL_DJI+ResidsAdj)
summary(ECM)

############## GSPC/IXIC ################
########      1.2 Estimating the cointegrating relationship using OLS
Log_Prices<-data.frame(logGSPC, logIXIC)
Coint_Eq = lm( logGSPC ~ logIXIC , data = Log_Prices)
summary(Coint_Eq)
########      1.3 Save the residuals of the cointegrating relationship
Resids <-  Coint_Eq$residuals
par(mfrow=c(1,1))
plot(Resids)
########      1.4 Test the residuals if they are I(0)
UnitRootTest<-ur.df(Resids, type = "none", selectlags = c("BIC"))
summary(UnitRootTest)
print("Residual are I(0)")
#here we have cointegrated pairs so we do the 3.3 task : ECM'''
DL_GSPC=diff(logGSPC)
DL_IXIC=diff(logIXIC)
ResidsAdj <-Resids[1:length(DL_IXIC)]
ECM <- lm(DL_GSPC~DL_IXIC+ResidsAdj)
summary(ECM)

# change dependent and independent variable
# IXIC/GSPC
########      1.2 Estimating the cointegrating relationship using OLS
Log_Prices<-data.frame(logIXIC, logGSPC)
Coint_Eq = lm( logIXIC ~ logGSPC , data = Log_Prices)
summary(Coint_Eq)
########      1.3 Save the residuals of the cointegrating relationship
Resids <-  Coint_Eq$residuals
par(mfrow=c(1,1))
plot(Resids)
########      1.4 Test the residuals if they are I(0)
UnitRootTest<-ur.df(Resids, type = "none", selectlags = c("BIC"))
summary(UnitRootTest)
print("Residual are I(0)")
#here we have cointegrated pairs so we do the 3.3 task : ECM'''
DL_GSPC=diff(logGSPC)
DL_IXIC=diff(logIXIC)
ResidsAdj <-Resids[1:length(DL_GSPC)]
ECM <- lm(DL_IXIC~DL_GSPC+ResidsAdj)
summary(ECM)


#Task 4
# we assign the volumes of GSPC
names(GSPC)[6] = "Volume"
Volume_GSPC<-GSPC$Volume # take the volume from the dataframe
log_volume <- log(Volume_GSPC) #comput log volumes
Volatility <- abs(R_ClosingPriceGSPC) # Take the absolut values

#Plot Vol
plot(DateGSPC[2:length(DateGSPC)], Volatility, type = 'l', xlab =" Date ",ylab ="Volatility", main = "Volatility of the S&P500 over the last 10 years",col = "red",
     xlim=c(start_date, end_date), ylim=c(0, 0.15))
par(mfrow=c(3,1))
plot(log_volume)
plot(Volatility)
plot(R_ClosingPriceGSPC)

#Task 4.1

#Before doing this test we have to check if our series are stationnary
#we already now for Log Returns of GSPC
adf.test(R_ClosingPriceGSPC)#ADF TEST FOR LOG RETURNS
adf.test(log_volume)#ADF TEST FOR LOG Volumes
adf.test(Volatility)#ADF TEST FOR Volatility


############################ Reduced Form VAR ##############################

# to perform this test, we delete the first data of log_volume. THen we can created REduced Form VAR
log_volume <- log_volume[-1] # To fit our lenght

VARData <- data.frame(log_volume,Volatility,R_ClosingPriceGSPC)
VARselect(VARData, lag.max=5) #Apply the Information Criteria to select the order of the VAR even if we have to chosse 5 
# optimal lag is 5
VAR_Reduced <- VAR(VARData, p = 5, type = "const") # APPLY THE OPTIMAL LAG
summary(VAR_Reduced)#RESULTS OF OUR VAR MODEL 


#4.2 Granger causality

#test between Volume and Returns
grangertest(log_volume ~ R_ClosingPriceGSPC, order = 5)
grangertest(R_ClosingPriceGSPC ~ log_volume, order = 5)
#Same for volume and Volatility
grangertest(log_volume ~ Volatility, order = 5)
grangertest(Volatility ~ log_volume, order = 5)
#last for Returns and Volatility
grangertest(Volatility ~ R_ClosingPriceGSPC, order = 5)
grangertest(R_ClosingPriceGSPC ~ Volatility, order = 5)


#4.3
# load a package
library(strucchange)

# 1st: extract the rediduals from ou var model
residuals_VAR <- residuals(VAR_Reduced)

# For each series in our var model, we perform CUSUM test
for(i in 1:ncol(residuals_VAR)) {
  # Convert into time serries
  ts_data <- ts(residuals_VAR[, i])
   # CUSUM
  cusum_test <- efp(ts_data ~ 1, type = "OLS-CUSUM")
  # Plot results
  plot(cusum_test, xlab = 'Time', main = paste("CUSUM Test for Variable", colnames(residuals_VAR)[i]))
}


#4.4
#order
VARData <- data.frame(log_volume, R_ClosingPriceGSPC,Volatility)

############### Structural VAR #####################################
VAR_Struct <- VAR(VARData, p = 5, type = "const")
summary(VAR_Struct)

# Identify structural shocks using the Cholesky decomposition #################
# To do this we use the Impulse Response Function (IRF) #######################
var_identified <- irf(VAR_Struct, impulse = "log_volume", response = c("log_volume","R_ClosingPriceGSPC", "Volatility"), n.ahead = 20, boot = FALSE)
plot(var_identified)
############################ Impulse Response Function (IRF) ##################
vd = fevd (VAR_Struct ,n.ahead = 20)
plot (vd)

#CHANGE ORDER IRF
var_identified <- irf(VAR_Struct, impulse = "R_ClosingPriceGSPC", response = c("R_ClosingPriceGSPC","log_volume", "Volatility"), n.ahead = 20, boot = FALSE)
plot(var_identified)

#CHANGE ORDER FOR IRF
var_identified <- irf(VAR_Struct, impulse = "Volatility", response = c("Volatility","log_volume", "R_ClosingPriceGSPC"), n.ahead = 20, boot = FALSE)
plot(var_identified)

#CHANGE CAUSAL ORDER 
VARData <- data.frame(R_ClosingPriceGSPC, log_volume,Volatility)
VAR_Struct <- VAR(VARData, p = 5, type = "const")
var_identified <- irf(VAR_Struct, impulse = "R_ClosingPriceGSPC", response = c("R_ClosingPriceGSPC","log_volume", "Volatility"), n.ahead = 20, boot = FALSE)
plot(var_identified)

#change order for IRF
var_identified <- irf(VAR_Struct, impulse = "log_volume", response = c("log_volume","R_ClosingPriceGSPC", "Volatility"), n.ahead = 20, boot = FALSE)
plot(var_identified)

#change order for IRF
var_identified <- irf(VAR_Struct, impulse = "Volatility", response = c("log_volume","Volatility", "R_ClosingPriceGSPC"), n.ahead = 20, boot = FALSE)
plot(var_identified)



vd = fevd (VAR_Struct ,n.ahead = 20)
plot (vd)