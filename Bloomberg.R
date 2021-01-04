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

###################################################

# Many statistical tests are used to try to reject some null hypothesis. In this particular case the Ljung-Box test tries to reject the independence of some values. What does it mean?
# 
# If p-value < 0.051: You can reject the null hypothesis assuming a 5% chance of making a mistake. So you can assume that your values are showing dependence on each other.
# 
# If p-value > 0.051: You don't have enough statistical evidence to reject the null hypothesis. So you can not assume that your values are dependent. This could mean that your values are dependent anyway or it can mean that your values are independent. But you are not proving any specific possibility, what your test actually said is that you can not assert the dependence of the values, neither can you assert the independence of the values.


rawPValues <- data.frame("AAPL"= c(Box.test(AAPLDaily, lag = 20, type="Ljung-Box")$p.value, 
                                   Box.test(AAPLWeekly, lag = 20, type="Ljung-Box")$p.value, 
                                   Box.test(AAPLMonthly, lag = 20, type="Ljung-Box")$p.value),
                     
                         "MSFT" = c(Box.test(MSFTDaily, lag = 20, type="Ljung-Box")$p.value, 
                                Box.test(MSFTWeekly, lag = 20, type="Ljung-Box")$p.value, 
                                Box.test(MSFTMonthly, lag = 20, type="Ljung-Box")$p.value),
                     
                         "GOOG" = c(Box.test(GOOGDaily, lag = 20, type="Ljung-Box")$p.value, 
                                Box.test(GOOGWeekly, lag = 20, type="Ljung-Box")$p.value, 
                                Box.test(GOOGMonthly, lag = 20, type="Ljung-Box")$p.value),
                     
                         "GE" = c(Box.test(GEDaily, lag = 20, type="Ljung-Box")$p.value, 
                              Box.test(GEWeekly, lag = 20, type="Ljung-Box")$p.value, 
                              Box.test(GEMonthly, lag = 20, type="Ljung-Box")$p.value),
                     
                         "IBM" = c(Box.test(IBMDaily, lag = 20, type="Ljung-Box")$p.value, 
                               Box.test(IBMWeekly, lag = 20, type="Ljung-Box")$p.value, 
                               Box.test(IBMMonthly, lag = 20, type="Ljung-Box")$p.value),
                     
                         "FTSE" = c(Box.test(FTSEDaily, lag = 20, type="Ljung-Box")$p.value, 
                                Box.test(FTSEWeekly, lag = 20, type="Ljung-Box")$p.value, 
                                Box.test(FTSEMonthly, lag = 20, type="Ljung-Box")$p.value),
                     
                         "DJI" = c(Box.test(DJIDaily, lag = 20, type="Ljung-Box")$p.value, 
                               Box.test(DJIWeekly, lag = 20, type="Ljung-Box")$p.value, 
                               Box.test(DJIMonthly, lag = 20, type="Ljung-Box")$p.value),
                     
                         "S&P" = c(Box.test(SPDaily, lag = 20, type="Ljung-Box")$p.value, 
                               Box.test(SPWeekly, lag = 20, type="Ljung-Box")$p.value, 
                               Box.test(SPMonthly, lag = 20, type="Ljung-Box")$p.value),
                     
                         "TSX" = c(Box.test(TSXDaily, lag = 20, type="Ljung-Box")$p.value, 
                               Box.test(TSXWeekly, lag = 20, type="Ljung-Box")$p.value, 
                               Box.test(TSXMonthly, lag = 20, type="Ljung-Box")$p.value),
                     
                         "DAX" = c(Box.test(DAXDaily, lag = 20, type="Ljung-Box")$p.value, 
                               Box.test(DAXWeekly, lag = 20, type="Ljung-Box")$p.value, 
                               Box.test(DAXMonthly, lag = 20, type="Ljung-Box")$p.value))
row.names(rawPValues) <- c("Daily", "Weekly", "Monthly")
View(rawPValues)

mydata <- merge(AAPLDaily, AAPLWeekly,AAPLMonthly, 
                        GOOGDaily, GOOGWeekly, GOOGMonthly,
                        MSFTDaily, MSFTWeekly, MSFTMonthly,
                        IBMDaily, IBMWeekly, IBMMonthly,
                        GEDaily, GEWeekly, GEMonthly,
                        FTSEDaily, FTSEWeekly, FTSEMonthly,
                        SPDaily, SPWeekly, SPMonthly,
                        TSXDaily, TSXWeekly, TSXMonthly,
                        DJIDaily, DJIWeekly, DJIMonthly,
                        DAXDaily, DAXWeekly, DJIMonthly)

colnames(mydata) <- c("AAPLDaily", "AAPLWeekly","AAPLMonthly", 
                      "GOOGDaily", "GOOGWeekly", "GOOGMonthly",
                      "MSFTDaily", "MSFTWeekly", "MSFTMonthly",
                      "IBMDaily", "IBMWeekly", "IBMMonthly",
                      "GEDaily", "GEWeekly", "GEMonthly",
                      "FTSEDaily", "FTSEWeekly", "FTSEMonthly",
                      "SPDaily", "SPWeekly", "SPMonthly",
                      "TSXDaily", "TSXWeekly", "TSXMonthly",
                      "DJIDaily", "DJIWeekly", "DJIMonthly",
                      "DAXDaily", "DAXWeekly", "DJIMonthly")
means <- colMeans(mydata, na.rm = TRUE)



absPValues <- data.frame("AAPL"= c(Box.test(abs(AAPLDaily), lag = 20, type="Ljung-Box")$p.value, 
                                   Box.test(abs(AAPLWeekly), lag = 20, type="Ljung-Box")$p.value, 
                                   Box.test(abs(AAPLMonthly), lag = 20, type="Ljung-Box")$p.value),
                         
                         "MSFT" = c(Box.test(abs(MSFTDaily), lag = 20, type="Ljung-Box")$p.value, 
                                    Box.test(abs(MSFTWeekly), lag = 20, type="Ljung-Box")$p.value, 
                                    Box.test(abs(MSFTMonthly), lag = 20, type="Ljung-Box")$p.value),
                         
                         "GOOG" = c(Box.test(abs(GOOGDaily), lag = 20, type="Ljung-Box")$p.value, 
                                    Box.test(abs(GOOGWeekly), lag = 20, type="Ljung-Box")$p.value, 
                                    Box.test(abs(GOOGMonthly), lag = 20, type="Ljung-Box")$p.value),
                         
                         "GE" = c(Box.test(abs(GEDaily), lag = 20, type="Ljung-Box")$p.value, 
                                  Box.test(abs(GEWeekly), lag = 20, type="Ljung-Box")$p.value, 
                                  Box.test(abs(GEMonthly), lag = 20, type="Ljung-Box")$p.value),
                         
                         "IBM" = c(Box.test(abs(IBMDaily), lag = 20, type="Ljung-Box")$p.value, 
                                   Box.test(abs(IBMWeekly), lag = 20, type="Ljung-Box")$p.value, 
                                   Box.test(abs(IBMMonthly), lag = 20, type="Ljung-Box")$p.value),
                         
                         "FTSE" = c(Box.test(abs(FTSEDaily), lag = 20, type="Ljung-Box")$p.value, 
                                    Box.test(abs(FTSEWeekly), lag = 20, type="Ljung-Box")$p.value, 
                                    Box.test(abs(FTSEMonthly), lag = 20, type="Ljung-Box")$p.value),
                         
                         "DJI" = c(Box.test(abs(DJIDaily), lag = 20, type="Ljung-Box")$p.value, 
                                   Box.test(abs(DJIWeekly), lag = 20, type="Ljung-Box")$p.value, 
                                   Box.test(abs(DJIMonthly), lag = 20, type="Ljung-Box")$p.value),
                         
                         "S&P" = c(Box.test(abs(SPDaily), lag = 20, type="Ljung-Box")$p.value, 
                                   Box.test(abs(SPWeekly), lag = 20, type="Ljung-Box")$p.value, 
                                   Box.test(abs(SPMonthly), lag = 20, type="Ljung-Box")$p.value),
                         
                         "TSX" = c(Box.test(abs(TSXDaily), lag = 20, type="Ljung-Box")$p.value, 
                                   Box.test(abs(TSXWeekly), lag = 20, type="Ljung-Box")$p.value, 
                                   Box.test(abs(TSXMonthly), lag = 20, type="Ljung-Box")$p.value),
                         
                         "DAX" = c(Box.test(abs(DAXDaily), lag = 20, type="Ljung-Box")$p.value, 
                                   Box.test(abs(DAXWeekly), lag = 20, type="Ljung-Box")$p.value, 
                                   Box.test(abs(DAXMonthly), lag = 20, type="Ljung-Box")$p.value))
row.names(absPValues) <- c("Daily", "Weekly", "Monthly")

View(absPValues)

sqPValues <- data.frame("AAPL"= c(Box.test(abs(AAPLDaily)*abs(AAPLDaily), lag = 20, type="Ljung-Box")$p.value,
                                   Box.test(abs(AAPLWeekly)*abs(AAPLWeekly), lag = 20, type="Ljung-Box")$p.value,
                                   Box.test(abs(AAPLMonthly)*abs(AAPLMonthly), lag = 20, type="Ljung-Box")$p.value),
                         
                        "MSFT" = c(Box.test(abs(MSFTDaily)*abs(MSFTDaily), lag = 20, type="Ljung-Box")$p.value,
                                    Box.test(abs(MSFTWeekly)*abs(MSFTWeekly), lag = 20, type="Ljung-Box")$p.value,
                                    Box.test(abs(MSFTMonthly)*abs(MSFTMonthly), lag = 20, type="Ljung-Box")$p.value),
                        
                        "GOOG" = c(Box.test(abs(GOOGDaily)*abs(GOOGDaily), lag = 20, type="Ljung-Box")$p.value,
                                    Box.test(abs(GOOGWeekly)*abs(GOOGWeekly), lag = 20, type="Ljung-Box")$p.value,
                                    Box.test(abs(GOOGMonthly)*abs(GOOGMonthly), lag = 20, type="Ljung-Box")$p.value),
                         
                        "GE" = c(Box.test(abs(GEDaily)*abs(GEDaily), lag = 20, type="Ljung-Box")$p.value,
                                  Box.test(abs(GEWeekly)*abs(GEWeekly), lag = 20, type="Ljung-Box")$p.value,
                                  Box.test(abs(GEMonthly)*abs(GEMonthly), lag = 20, type="Ljung-Box")$p.value),
                        
                        "IBM" = c(Box.test(abs(IBMDaily)*abs(IBMDaily), lag = 20, type="Ljung-Box")$p.value,
                                   Box.test(abs(IBMWeekly)*abs(IBMWeekly), lag = 20, type="Ljung-Box")$p.value,
                                   Box.test(abs(IBMMonthly)*abs(IBMMonthly), lag = 20, type="Ljung-Box")$p.value),
                         
                        "FTSE" = c(Box.test(abs(FTSEDaily)*abs(FTSEDaily), lag = 20, type="Ljung-Box")$p.value,
                                    Box.test(abs(FTSEWeekly)*abs(FTSEWeekly), lag = 20, type="Ljung-Box")$p.value,
                                    Box.test(abs(FTSEMonthly)*abs(FTSEMonthly), lag = 20, type="Ljung-Box")$p.value),
                         
                        "DJI" = c(Box.test(abs(DJIDaily)*abs(DJIDaily), lag = 20, type="Ljung-Box")$p.value,
                                   Box.test(abs(DJIWeekly)*abs(DJIWeekly), lag = 20, type="Ljung-Box")$p.value,
                                   Box.test(abs(DJIMonthly)*abs(DJIMonthly), lag = 20, type="Ljung-Box")$p.value),
                         
                        "S&P" = c(Box.test(abs(SPDaily)*abs(SPDaily), lag = 20, type="Ljung-Box")$p.value,
                                   Box.test(abs(SPWeekly)*abs(SPWeekly), lag = 20, type="Ljung-Box")$p.value,
                                   Box.test(abs(SPMonthly)*abs(SPMonthly), lag = 20, type="Ljung-Box")$p.value),
                         
                        "TSX" = c(Box.test(abs(TSXDaily)*abs(TSXDaily), lag = 20, type="Ljung-Box")$p.value,
                                   Box.test(abs(TSXWeekly)*abs(TSXWeekly), lag = 20, type="Ljung-Box")$p.value,
                                   Box.test(abs(TSXMonthly)*abs(TSXMonthly), lag = 20, type="Ljung-Box")$p.value),
                         
                        "DAX" = c(Box.test(abs(DAXDaily)*abs(DAXDaily), lag = 20, type="Ljung-Box")$p.value,
                                   Box.test(abs(DAXWeekly)*abs(DAXWeekly), lag = 20, type="Ljung-Box")$p.value,
                                   Box.test(abs(DAXMonthly)*abs(DAXMonthly), lag = 20, type="Ljung-Box")$p.value))

row.names(sqPValues) <- c("Daily", "Weekly", "Monthly")
View(sqPValues)


require(fBasics)
kmoment <- data.frame("AAPL"= c(kurtosis(na.contiguous(AAPLDaily))-3, 
                                kurtosis(na.contiguous(AAPLWeekly))-3, 
                                kurtosis(na.contiguous(AAPLMonthly))-3),
                      
                        "MSFT" = c(kurtosis(na.contiguous(MSFTDaily)-3), 
                                   kurtosis(na.contiguous(MSFTWeekly)-3), 
                                   kurtosis(na.contiguous(MSFTMonthly))-3),
                                   
                        "GOOG" = c(kurtosis(na.contiguous(GOOGDaily))-3, 
                                   kurtosis(na.contiguous(GOOGWeekly))-3, 
                                   kurtosis(na.contiguous(GOOGMonthly))-3),
                      
                        "GE" = c(kurtosis(na.contiguous(GEDaily))-3, 
                                 kurtosis(na.contiguous(GEWeekly))-3, 
                                 kurtosis(na.contiguous(GEMonthly))-3),
                      
                        "IBM" = c(kurtosis(na.contiguous(IBMDaily))-3, 
                                  kurtosis(na.contiguous(IBMWeekly))-3, 
                                  kurtosis(na.contiguous(IBMMonthly))-3),
                      
                        "FTSE" = c(kurtosis(na.contiguous(FTSEDaily))-3, 
                                   kurtosis(na.contiguous(FTSEWeekly))-3, 
                                   kurtosis(na.contiguous(FTSEMonthly))-3),
                      
                        "DJI" = c(kurtosis(na.contiguous(DJIDaily))-3, 
                                  kurtosis(na.contiguous(DJIWeekly))-3, 
                                  kurtosis(na.contiguous(DJIMonthly)))-3,
                      
                        "S&P" = c(kurtosis(na.contiguous(SPDaily)), 
                                  kurtosis(na.contiguous(SPWeekly)), 
                                  kurtosis(na.contiguous(SPMonthly))),
                      
                        "TSX" = c(kurtosis(na.contiguous(TSXDaily)), 
                                  kurtosis(na.contiguous(TSXWeekly)), 
                                  kurtosis(na.contiguous(TSXMonthly))),
                      
                        "DAX" = c(kurtosis(na.contiguous(DAXDaily)), 
                                  kurtosis(na.contiguous(DAXWeekly)), 
                                  kurtosis(na.contiguous(DAXMonthly))))
row.names(kmoment) <- c("Daily", "Weekly", "Monthly")

View(kmoment)

require(lmtest)
