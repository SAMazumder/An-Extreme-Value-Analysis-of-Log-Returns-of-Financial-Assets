# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())

require(quantmod)
require(fpp2)
require(PerformanceAnalytics)
require(fBasics)
require(xts)
require(taRifx)#used to remove "N/A N/A" strings 

data <- read.csv("data.csv",header=T, sep=",") #Data has headers and seperated by ","
str(data) ##Notice GOOG is a factor and has bunch of ""#N/A N/A""


#Clean data
data$GOOG.US.Equity <- destring(data$GOOG.US.Equity) ## destring GOOG.US.EQUITY
str(data) ## now all data is numeric

# data[data == "#N/A N/A"] <- NA
# data$GOOG.US.Equity <- as.numeric(levels(data$GOOG.US.Equity))[data$GOOG.US.Equity]

attach(data)




require(xts)
DATE <- as.Date(as.character(data$DATE), "%Y-%m-%d")
AAPL <- xts(AAPL.US.Equity, DATE)
GOOG <- xts(GOOG.US.Equity, DATE)
MSFT <- xts(MSFT.US.Equity, DATE)
IBM <- xts(IBM.US.Equity, DATE)
GE <- xts(GE.US.Equity, DATE)
FTSE <- xts(UKX.Index,DATE)
SP <- xts(SPX.Index, DATE)
TSX <- xts(SPTSX.Index, DATE)
DJI <- xts(DJI.Index,DATE)
DAX <- xts(DAX.Index, DATE)

#Check where NA is
sum(is.na(AAPL))

sum(is.na(GOOG))  
## Google has 3712 NAs
sum(!is.na(GOOG)) == sum(na.contiguous(GOOG))  ##Check if largest contiguous non NA is the same as all NON NA data
GOOG <- na.contiguous(GOOG)
sum(is.na(GOOG))  

sum(is.na(MSFT))
sum(is.na(IBM))
sum(is.na(GE))
sum(is.na(FTSE))
sum(is.na(SP))
sum(is.na(TSX))
sum(is.na(DJI))
sum(is.na(DAX))








#####APPLE######
logAAPLDaily <- xts(log(AAPL), OHLC=FALSE)

#Create Daily TS
AAPLDaily <- na.contiguous(diff(logAAPLDaily, lag = 1))
#Create Weekly TS
AAPLWeekly <- na.contiguous(diff(to.weekly(logAAPLDaily, OHLC=FALSE), lag = 1))
#Create Monthly TS
AAPLMonthly <- na.contiguous(diff(to.monthly(logAAPLDaily,OHLC=FALSE), lag =1))

#####MICROSOFT######
logMSFTDaily <-  xts(log(MSFT), OHLC=FALSE)

#Create Daily TS
MSFTDaily <- diff(logMSFTDaily, lag = 1)
#Create Weekly TS
MSFTWeekly <- diff(logMSFTDaily, lag = 5)
#Create Monthly TS
MSFTMonthly <- diff(logMSFTDaily, lag =25)

#####GOOGLE######
logGOOGDaily <- xts(log(GOOG), OHLC=FALSE)

#Create Daily TS
GOOGDaily <- diff(logGOOGDaily, lag = 1)
#Create Weekly TS
GOOGWeekly <- diff(to.weekly(logGOOGDaily, OHLC=FALSE), lag = 1)
#Create Monthly TS
GOOGMonthly <- diff(to.monthly(logGOOGDaily,OHLC=FALSE), lag =1)

#####IBM######
logIBMDaily <-  xts(log(IBM), OHLC=FALSE)
#Create Daily TS
IBMDaily <- diff(logIBMDaily, lag = 1)
#Create Weekly TS
IBMWeekly <- diff(to.weekly(logIBMDaily,OHLC=FALSE), lag =1)
#Create Monthly TS
IBMMonthly <- diff(to.monthly(logIBMDaily,OHLC=FALSE), lag =1)

#####GE######
logGEDaily <- xts(log(GE), OHLC=FALSE)
#Create Daily TS
GEDaily <- diff(logGEDaily, lag = 1)
#Create Weekly TS
GEWeekly <- diff(to.weekly(logGEDaily,OHLC=FALSE), lag =1)
#Create Monthly TS
GEMonthly <- diff(to.monthly(logGEDaily,OHLC=FALSE), lag =1)

#####FTSE######
logFTSEDaily <- xts(log(FTSE), OHLC=FALSE)
#Create Daily TS
FTSEDaily <-  diff(logFTSEDaily, lag = 1)
#Create Weekly TS
FTSEWeekly <- diff(to.weekly(logFTSEDaily,OHLC=FALSE), lag =1)
#Create Monthly TS
FTSEMonthly <- diff(to.monthly(logFTSEDaily,OHLC=FALSE), lag =1)

#####S&P######
logSPDaily <- xts(log(SP), OHLC=FALSE)
#Create Daily TS
SPDaily <-  diff(logSPDaily, lag = 1)
#Create Weekly TS
SPWeekly <- diff(to.weekly(logSPDaily,OHLC=FALSE), lag =1)
#Create Monthly TS
SPMonthly <- diff(to.monthly(logSPDaily,OHLC=FALSE), lag =1)

#####TSX######
logTSXDaily <- xts(log(TSX), OHLC=FALSE)
#Create Daily TS
TSXDaily <-  diff(logTSXDaily, lag = 1)
#Create Weekly TS
TSXWeekly <- diff(to.weekly(logTSXDaily,OHLC=FALSE), lag =1)
#Create Monthly TS
TSXMonthly <- diff(to.monthly(logTSXDaily,OHLC=FALSE), lag =1)

#####DJI######
logDJIDaily <- xts(log(DJI), OHLC=FALSE)

#Create Daily TS
DJIDaily <- diff(logDJIDaily, lag = 1)
#Create Weekly TS
DJIWeekly <- diff(to.weekly(logDJIDaily,OHLC=FALSE), lag =1)
#Create Monthly TS
DJIMonthly <- diff(to.monthly(logDJIDaily,OHLC=FALSE), lag =1)

#####DAX######
logDAXDaily <- xts(DAX, OHLC=FALSE)

#Create Daily TS
DAXDaily <- diff(logDAXDaily, lag = 1)
#Create Weekly TS
DAXWeekly <- diff(to.weekly(logDAXDaily,OHLC=FALSE), lag =1)
#Create Monthly TS
DAXMonthly <- diff(to.monthly(logDAXDaily,OHLC=FALSE), lag =1)


Hillfunction <- function(x,k){
  # Compute the Hill estimate of the shape parameter.
  # x: data and k: the number of order statistics used
  sx=sort(x)
  T=length(x)
  ist=T-k
  y=log(sx[ist:T])
  hill=sum(y[2:length(y)])/k
  hill=hill-y[1]
  sd=sqrt(hill^2/k)
  cat("Hill estimate & std-err:",c(hill,sd),"\n")
}



##Coredata#############
#Apple Daily
nAAPLDaily <- coredata(-AAPLDaily[AAPLDaily<0])
hill(nAAPLDaily, option = c("xi"),  end = 0.2*length(nAAPLDaily))
Hillfunction(nAAPLDaily, 226)

pAAPLDaily <- coredata(AAPLDaily[AAPLDaily>0])
hill(pAAPLDaily, option = c("xi"),  end = 0.2*length(pAAPLDaily))
Hillfunction(nAAPLDaily, 300)

#Apple Weekly
nAAPLWeekly <- coredata(-AAPLWeekly[AAPLWeekly<0])
hill(nAAPLWeekly, option = c("xi"),  end = 0.2*length(nAAPLWeekly))
Hillfunction(nAAPLWeekly, 71)

pAAPLWeekly <- coredata(AAPLWeekly[AAPLWeekly>0])
hill(pAAPLWeekly, option = c("xi"),  end = 0.2*length(pAAPLWeekly))
Hillfunction(nAAPLWeekly, 65)

#Apple Monthly
nAAPLMonthly <- coredata(-AAPLMonthly[AAPLMonthly<0])
hill(nAAPLMonthly, option = c("xi"),  end = 0.2*length(nAAPLMonthly))
Hillfunction(nAAPLMonthly, 16)

pAAPLMonthly <- coredata(AAPLMonthly[AAPLMonthly>0])
hill(pAAPLMonthly, option = c("xi"),  end = 0.3*length(pAAPLMonthly))
Hillfunction(nAAPLMonthly, 28)

####################################################

#GOOG Daily
nGOOGDaily <- coredata(-GOOGDaily[GOOGDaily<0])
hill(nGOOGDaily, option = c("xi"),  end = 0.2*length(nGOOGDaily))
Hillfunction(nGOOGDaily, 63)

pGOOGDaily <- coredata(GOOGDaily[GOOGDaily>0])
hill(pGOOGDaily, option = c("xi"),  end = 0.2*length(pGOOGDaily))
Hillfunction(pGOOGDaily, 105)

#GOOG Weekly
nGOOGWeekly <- coredata(-GOOGWeekly[GOOGWeekly<0])
hill(nGOOGWeekly, option = c("xi"),  end = 0.2*length(nGOOGWeekly))
Hillfunction(nGOOGWeekly, 21)

pGOOGWeekly <- coredata(GOOGWeekly[GOOGWeekly>0])
hill(pGOOGWeekly, option = c("xi"),  end = 0.2*length(pGOOGWeekly))
Hillfunction(pGOOGWeekly, 25)

#GOOG Monthly
nGOOGMonthly <- coredata(-GOOGMonthly[GOOGMonthly<0])
hill(nGOOGMonthly, option = c("xi"),  end = 0.2*length(nGOOGMonthly))
Hillfunction(nGOOGMonthly, 11)

pGOOGMonthly <- coredata(GOOGMonthly[GOOGMonthly>0])
hill(pGOOGMonthly, option = c("xi"),  end = 0.3*length(pGOOGMonthly))
Hillfunction(pGOOGMonthly, 13)

####################################################

nCoredataHill <- function(x){
nx <-coredata(-x[x<0])
hillresults <- hill(nx, option = c("xi"),  end = 0.2*length(nx))
out <<-nx 
}


pCoredataHill <- function(x){
  nx <-coredata(x[x>0])
  hillresults <- hill(nx, option = c("xi"),  end = 0.2*length(nx))
  out <<-nx 
}

#############Above is a function created to speed up this nonsense####

#######Microsoft####
nhillMSFTDaily <- nCoredataHill(MSFTDaily)
Hillfunction(nhillMSFTDaily, 320)

phillMSFTDaily <- pCoredataHill(MSFTDaily)
Hillfunction(phillMSFTDaily, 130)
##
nhillMSFTWeekly <- nCoredataHill(MSFTWeekly)
Hillfunction(nhillMSFTWeekly, 197)

phillMSFTWeekly<- pCoredataHill(MSFTWeekly)
Hillfunction(phillMSFTWeekly, 254)
##
nhillMSFTMonthly <- nCoredataHill(MSFTMonthly)
Hillfunction(nhillMSFTMonthly , 150)

phillMSFTMonthly<- pCoredataHill(MSFTMonthly)
Hillfunction(phillMSFTMonthly, 275)


#######IBM####
nhillIBMDaily <- nCoredataHill(IBMDaily)
Hillfunction(nhillIBMDaily, 321)

phillIBMDaily <- pCoredataHill(IBMDaily)
Hillfunction(phillIBMDaily, 229)
##
nhillIBMWeekly <- nCoredataHill(IBMWeekly)
Hillfunction(nhillIBMWeekly, 56)

phillIBMWeekly<- pCoredataHill(IBMWeekly)
Hillfunction(phillIBMWeekly, 65)
##
nhillIBMMonthly <- nCoredataHill(IBMMonthly)
Hillfunction(nhillIBMMonthly , 21)

phillIBMMonthly<- pCoredataHill(IBMMonthly)
Hillfunction(phillIBMMonthly, 17)

#######GE####
nhillGEDaily <- nCoredataHill(GEDaily)
Hillfunction(nhillGEDaily, 144)

phillGEDaily <- pCoredataHill(GEDaily)
Hillfunction(phillGEDaily, 197)
##

nhillGEWeekly <- nCoredataHill(GEWeekly)
Hillfunction(nhillGEWeekly, 60)

phillGEWeekly<- pCoredataHill(GEWeekly)
Hillfunction(phillGEWeekly, 60)
##

nhillGEMonthly <- nCoredataHill(GEMonthly)
Hillfunction(nhillGEMonthly , 21)

phillGEMonthly<- pCoredataHill(GEMonthly)
Hillfunction(phillGEMonthly, 17)

#######FTSE####
nhillFTSEDaily <- nCoredataHill(FTSEDaily)
Hillfunction(nhillFTSEDaily, 139)

phillFTSEDaily <- pCoredataHill(FTSEDaily)
Hillfunction(phillFTSEDaily, 240)
##
nhillFTSEWeekly <- nCoredataHill(FTSEWeekly)
Hillfunction(nhillFTSEWeekly, 59)

phillFTSEWeekly<- pCoredataHill(FTSEWeekly)
Hillfunction(phillFTSEWeekly, 45)
##
nhillFTSEMonthly <- nCoredataHill(FTSEMonthly)
Hillfunction(nhillFTSEMonthly , 17)

phillFTSEMonthly<- pCoredataHill(FTSEMonthly)
Hillfunction(phillFTSEMonthly, 18)

#######SP####
nhillSPDaily <- nCoredataHill(SPDaily)
Hillfunction(nhillSPDaily, 145)

phillSPDaily <- pCoredataHill(SPDaily)
Hillfunction(phillSPDaily, 151)
##
nhillSPWeekly <- nCoredataHill(SPWeekly)
Hillfunction(nhillSPWeekly, 51)

phillSPWeekly<- pCoredataHill(SPWeekly)
Hillfunction(phillSPWeekly, 85)
##
nhillSPMonthly <- nCoredataHill(SPMonthly)
Hillfunction(nhillSPMonthly , 16)

phillSPMonthly<- pCoredataHill(SPMonthly)
Hillfunction(phillSPMonthly, 24)


#######TSX####
nhillTSXDaily <- nCoredataHill(TSXDaily)
Hillfunction(nhillTSXDaily, 106)

phillTSXDaily <- pCoredataHill(TSXDaily)
Hillfunction(phillTSXDaily, 251)
##
nhillTSXWeekly <- nCoredataHill(TSXWeekly)
Hillfunction(nhillTSXWeekly, 63)

phillTSXWeekly<- pCoredataHill(TSXWeekly)
Hillfunction(phillTSXWeekly, 85)
##
nhillTSXMonthly <- nCoredataHill(TSXMonthly)
Hillfunction(nhillTSXMonthly , 18)

phillTSXMonthly<- pCoredataHill(TSXMonthly)
Hillfunction(phillTSXMonthly, 23)


#######DJI####
nhillDJIDaily <- nCoredataHill(DJIDaily)
Hillfunction(nhillDJIDaily, 193)

phillDJIDaily <- pCoredataHill(DJIDaily)
Hillfunction(phillDJIDaily, 215)
##
nhillDJIWeekly <- nCoredataHill(DJIWeekly)
Hillfunction(nhillDJIWeekly, 79)

phillDJIWeekly<- pCoredataHill(DJIWeekly)
Hillfunction(phillDJIWeekly, 65)
##
nhillDJIMonthly <- nCoredataHill(DJIMonthly)
Hillfunction(nhillDJIMonthly , 16)

phillDJIMonthly<- pCoredataHill(DJIMonthly)
Hillfunction(phillDJIMonthly, 23)

#######DAX####
nhillDAXDaily <- nCoredataHill(DAXDaily)
Hillfunction(nhillDAXDaily, 160)

phillDAXDaily <- pCoredataHill(DAXDaily)
Hillfunction(phillDAXDaily, 218)
##
nhillDAXWeekly <- nCoredataHill(DAXWeekly)
Hillfunction(nhillDAXWeekly, 63)

phillDAXWeekly<- pCoredataHill(DAXWeekly)
Hillfunction(phillDAXWeekly, 60)
##
nhillDAXMonthly <- nCoredataHill(DAXMonthly)
Hillfunction(nhillDAXMonthly , 18)

phillDAXMonthly<- pCoredataHill(DAXMonthly)
Hillfunction(phillDAXMonthly, 20)
