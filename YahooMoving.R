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


tickers <- c('AAPL','MSFT','GOOG','GE','IBM','^GSPC','^DJI','^GSPTSE','^FTSE', '^GDAXI')
getSymbols(tickers, from="1990-01-01", src = "yahoo", na.action = na.omit)
tickers <- gsub("\\^", "",tickers)

##Get Daily Close###
AAPL <- Cl(AAPL)
GOOG <- Cl(GOOG)
MSFT <- Cl(MSFT)
IBM <- Cl(IBM)
GE <- Cl(GE)
FTSE <- Cl(FTSE)
SP <-  Cl(GSPC)
TSX <-  Cl(GSPTSE)
DJI <-  Cl(DJI)
DAX <-  Cl(GDAXI)

chartSeries(SP)

#Check where NA is
sum(is.na(AAPL))
sum(is.na(GOOG))
sum(is.na(MSFT))
sum(is.na(IBM))
sum(is.na(GE))

sum(is.na(FTSE)) ##FTSE has 91 NAs
index(FTSE)[is.na(FTSE)]
FTSE <- FTSE[!is.na(FTSE)]

sum(is.na(SP))

sum(is.na(TSX))##TSX has 57 NAs
TSX <- TSX[!is.na(TSX)]

sum(is.na(DJI))

sum(is.na(DAX))##TSX has 57 NAs
DAX <- DAX[!is.na(DAX)]




# # #Log Closing prices from Tickers
# # dailyClose <- do.call(merge, lapply(tickers, function(x) Cl(get(x))))
# # weeklyClose <- do.call(merge, lapply(tickers, function(x) (Cl(to.weekly(get(x))))))
# # 
# # logDailyClose <- do.call(merge, lapply(tickers, function(x) log(Cl(get(x)))))
# # 
# # logDaily <- diff(na.pass(logClose), lag = 1)
# # logWeekly  <- diff(na.pass(logClose), lag = 1 )
# # logMonthly <- diff(na.pass(logClose), lag = 1)
# # 
# # for (i in 1:ncol(logDaily))
# # { acf(logDaily[,i], na.action = na.pass)
# # }
# # 
# # for (i in 1:ncol(logWeekly))
# # { acf(logWeekly[,i], na.action = na.pass)
# # }
# # 
# # for (i in 1:ncol(logMonthly))
# # { acf(logMonthly[,i], na.action = na.pass)
# # }
# 
# 
# 
# #qqnorm(coredata(log_daily), main = "GSPC Log Daily Close QQ-plot")
# #qqline(coredata(log_daily), col = "red")
#                      
# #qqnorm(coredata(log_weekly), main = "GSPC Log Weekly QQ-plot")
# #qqline(coredata(log_weekly), col = "red")
#                      
# #qqnorm(coredata(log_monthly), main = "GSPC Log 30 Day Returns QQ-plot")
# #qqline(coredata(log_monthly), col = "red")
# 
# LBrawp <- data.frame("AAPL"= c(1, 2, 3),
#                   "MSFT" = c(1, 2, 3),
#                   "GOOG" = c(1, 2, 3),
#                   "GE" = c(1, 2, 3),
#                   "IBM" = c(1, 2, 3),
#                   "GSPC" = c(1, 2, 3),
#                   "DJI" = c(1, 2, 3),
#                   "GSPTSE" = c(1, 2, 3),
#                   "IXIC" = c(1, 2, 3),
#                   "FTSE" = c(1, 2, 3))
# 
# for (i in 1:ncol(logDaily)) 
# { LBrawp[1,i] <- Box.test(coredata(logDaily[,i]), lag = 20, type="Ljung-Box")$p.value
# }
# 
# for (i in 1:ncol(logWeekly)) 
# { LBrawp[2,i] <- Box.test(coredata(logWeekly[,i]), lag = 20, type="Ljung-Box")$p.value
# }
# 
# for (i in 1:ncol(logMonthly)) 
# { LBrawp[3,i] <- Box.test(coredata(logMonthly[,i]), lag = 20, type="Ljung-Box")$p.value
# }
# 
# View(LBp)
# 
# 
#                      
# test <- Box.test(coredata(logDaily$GOOG.Close), lag = min(20, length(logDaily$GOOG.Close)-1), type="Ljung-Box")
# Box.test(coredata(log_weekly), lag = min(20, length(log_weekly)-1), type="Ljung-Box")
# Box.test(coredata(log_monthly), lag = min(20, length(log_monthly)-1), type="Ljung-Box")
# 
# LBp <- data.frame("AAPL"= c(1, 2, 3),
#                   "MSFT" = c(1, 2, 3),
#                   "GOOG" = c(1, 2, 3),
#                   "GE" = c(1, 2, 3),
#                   "IBM" = c(1, 2, 3),
#                   "GSPC" = c(1, 2, 3),
#                   "DJI" = c(1, 2, 3),
#                   "GSPTSE" = c(1, 2, 3),
#                   "IXIC" = c(1, 2, 3),
#                   "FTSE" = c(1, 2, 3))
#                      
#                     
# 
# 
# 
# 
# 
#  #Basic Stats:
#                      T <- length(log_daily)
#                      m1 <- mean(log_daily)
#                      m2 <- var(log_daily)
#                      
# 
# chartSeries(log_daily$GOOG, type="line", subset="last 15 years")




# ##MOVING###
# ##################################################

#####APPLE######
logAAPLDaily <- log(AAPL)

#Create Daily TS
AAPLDaily <- diff(logAAPLDaily, lag = 1)
acf(AAPLDaily, na.action = na.omit, lag.max = length(AAPLDaily))
acf(abs(AAPLDaily), na.action = na.omit,lag.max = length(AAPLDaily))


#####APPLE######
AAPLWeekly <- diff(logAAPLDaily, lag = 5)
acf(AAPLWeekly, na.action = na.omit, lag.max = length(AAPLWeekly))
acf(abs(AAPLWeekly), na.action = na.omit,lag.max = length(AAPLWeekly))

AAPLMonthly <- diff(logAAPLDaily, lag = 20)
acf(AAPLMonthly, na.action = na.omit, lag.max = length(AAPLMonthly))
acf(abs(AAPLMonthly), na.action = na.omit,lag.max = length(AAPLMonthly))

plot(density(AAPLDaily))


#####MICROSOFT######
logMSFTDaily <-  log(MSFT)

#Create Daily TS
MSFTDaily <- diff(logMSFTDaily, lag = 1)
acf(MSFTDaily, na.action = na.omit, lag.max = length(MSFTDaily))
acf(abs(MSFTDaily), na.action = na.omit,lag.max = length(MSFTDaily))

#####MICROSOFT######
MSFTWeekly<- diff(logMSFTDaily, lag = 5)
acf(MSFTWeekly, na.action = na.omit, lag.max = length(MSFTWeekly))
acf(abs(MSFTWeekly), na.action = na.omit,lag.max = length(MSFTWeekly))

MSFTMonthly <- diff(logMSFTDaily, lag =20)
acf(MSFTMonthly, na.action = na.omit, lag.max = length(MSFTMonthly))
acf(abs(MSFTMonthly), na.action = na.omit,lag.max = length(MSFTMonthly))


#####GOOGLE######
logGOOGDaily <- log(GOOG)

#Create Daily TS
GOOGDaily <- diff(logGOOGDaily, lag = 1)
acf(GOOGDaily, na.action = na.omit, lag.max = length(GOOGDaily))
acf(abs(GOOGDaily), na.action = na.omit,lag.max = length(GOOGDaily))

#####GOOGLE######
GOOGWeekly <- diff(logGOOGDaily, lag = 5)
acf(GOOGWeekly, na.action = na.omit, lag.max = length(GOOGWeekly))
acf(abs(GOOGWeekly), na.action = na.omit,lag.max = length(GOOGWeekly))

GOOGMonthly <- diff(logGOOGDaily, lag =20)
acf(GOOGMonthly, na.action = na.omit, lag.max = length(GOOGMonthly))
acf(abs(GOOGMonthly), na.action = na.omit,lag.max = length(GOOGMonthly))




#####IBM######
logIBMDaily <-  log(IBM)

#Create Daily TS
IBMDaily <- diff(logIBMDaily, lag = 1)
acf(IBMDaily, na.action = na.omit, lag.max = length(IBMDaily))
acf(abs(IBMDaily), na.action = na.omit,lag.max = length(IBMDaily))

#####IBM######
IBMWeekly <- diff(logIBMDaily, lag =5)
acf(IBMWeekly, na.action = na.omit, lag.max = length(IBMWeekly))
acf(abs(IBMWeekly), na.action = na.omit,lag.max = length(IBMWeekly))


IBMMonthly <- diff(logIBMDaily, lag =20)
acf(IBMMonthly, na.action = na.omit, lag.max = length(IBMMonthly))
acf(abs(IBMMonthly), na.action = na.omit,lag.max = length(IBMMonthly))


#####GE######
logGEDaily <- log(GE)

#Create Daily TS
GEDaily <- diff(logGEDaily, lag = 1)
acf(GEDaily, na.action = na.omit, lag.max = length(GEDaily))
acf(abs(GEWeekly), na.action = na.omit,lag.max = length(GEWeekly))

#####GE######
GEWeekly <- diff(logGEDaily, lag = 5)
acf(GEWeekly, na.action = na.omit, lag.max = length(GEWeekly))
acf(abs(GEWeekly), na.action = na.omit,lag.max = length(GEWeekly))

GEMonthly <- diff(logGEDaily, lag =20)
acf(GEMonthly, na.action = na.omit, lag.max = length(GEDaily))
acf(abs(GEMonthly), na.action = na.omit,lag.max = length(GEMonthly))


#####FTSE######
logFTSEDaily <- log(FTSE)

#Create Daily TS
FTSEDaily <-  diff(logFTSEDaily, lag = 1)
acf(FTSEDaily, na.action = na.omit, lag.max = length(FTSEDaily))
acf(abs(FTSEDaily), na.action = na.omit,lag.max = length(FTSEDaily))

#####FTSE######
FTSEWeekly <- diff(logFTSEDaily, lag =5)
acf(FTSEWeekly, na.action = na.omit, lag.max = length(FTSEWeekly))
acf(abs(FTSEWeekly), na.action = na.omit,lag.max = length(FTSEWeekly))

#Create Monthly TS
FTSEMonthly <- diff(logFTSEDaily, lag = 20)
acf(FTSEMonthly, na.action = na.omit, lag.max = length(FTSEMonthly))
acf(abs(FTSEMonthly), na.action = na.omit,lag.max = length(FTSEMonthly))


#####S&P######
logSPDaily <- log(SP)

#Create Daily TS
SPDaily <-  diff(logSPDaily, lag = 1)
acf(SPDaily, na.action = na.omit, lag.max = length(SPDaily))
acf(abs(SPDaily), na.action = na.omit,lag.max = length(SPDaily))

#####S&P######
#Create Weekly TS
SPWeekly <- diff(logSPDaily, lag =5)
acf(SPWeekly, na.action = na.omit, lag.max = length(SPWeekly))
acf(abs(SPWeekly), na.action = na.omit,lag.max = length(SPWeekly))

#Create Monthly TS
SPMonthly <- diff(logSPDaily, lag =20)
acf(SPMonthly, na.action = na.omit, lag.max = length(SPMonthly))
acf(abs(SPMonthly), na.action = na.omit,lag.max = length(SPMonthly))



#####TSX######
logTSXDaily <- log(TSX)

#Create Daily TS
TSXDaily <-  diff(logTSXDaily, lag = 1)
acf(TSXDaily, na.action = na.omit, lag.max = length(TSXDaily))
acf(abs(TSXDaily), na.action = na.omit,lag.max = length(TSXDaily))

#####TSX######
TSXWeekly <- diff(logTSXDaily, lag =5)
acf(TSXWeekly, na.action = na.omit, lag.max = length(TSXWeekly))
acf(abs(TSXWeekly), na.action = na.omit,lag.max = length(TSXWeekly))

TSXMonthly <- diff(logTSXDaily, lag =20)
acf(TSXMonthly, na.action = na.omit, lag.max = length(TSXMonthly))
acf(abs(TSXMonthly), na.action = na.omit,lag.max = length(TSXMonthly))


#####DJI######
logDJIDaily <- log(DJI)

#Create Daily TS
DJIDaily <- diff(logDJIDaily, lag = 1)
acf(DJIDaily, na.action = na.omit, lag.max = length(DJIDaily))
acf(abs(DJIDaily), na.action = na.omit,lag.max = length(DJIDaily))


#####DJI######
DJIWeekly <- diff(logDJIDaily, lag =5)
acf(DJIWeekly, na.action = na.omit, lag.max = length(DJIWeekly))
acf(abs(DJIWeekly), na.action = na.omit,lag.max = length(DJIWeekly))

DJIMonthly <- diff(logDJIDaily, lag =20)
acf(DJIMonthly, na.action = na.omit, lag.max = length(DJIMonthly))
acf(abs(DJIMonthly), na.action = na.omit,lag.max = length(DJIMonthly))


#####DAX######
logDAXDaily <- DAX

#Create Daily TS
DAXDaily <- diff(logDAXDaily, lag = 1)
acf(DAXDaily, na.action = na.omit, lag.max = length(DAXDaily))
acf(abs(DAXDaily), na.action = na.omit,lag.max = length(DAXDaily))

#####DAX######
DAXWeekly <- diff(logDAXDaily, lag =5)
acf(DAXWeekly, na.action = na.omit, lag.max = length(DAXWeekly))
acf(abs(DAXWeekly), na.action = na.omit)

DAXMonthly <- diff(logDAXDaily, lag =20)
acf(DAXMonthly, na.action = na.omit, lag.max = length(DAXMonthly))
acf(abs(DAXMonthly), na.action = na.omit)






















# ###################################################

##If the p value is greater than 0.05 then the residuals are independent which we want for the model to be correct


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
kmoment <- data.frame("AAPL"= c(kurtosis(na.contiguous(AAPLDaily)), 
                                kurtosis(na.contiguous(AAPLWeekly)), 
                                kurtosis(na.contiguous(AAPLMonthly))),
                      
                      "MSFT" = c(kurtosis(na.contiguous(MSFTDaily)), 
                                 kurtosis(na.contiguous(MSFTWeekly)), 
                                 kurtosis(na.contiguous(MSFTMonthly))),
                      
                      "GOOG" = c(kurtosis(na.contiguous(GOOGDaily)), 
                                 kurtosis(na.contiguous(GOOGWeekly)), 
                                 kurtosis(na.contiguous(GOOGMonthly))),
                      
                      "GE" = c(kurtosis(na.contiguous(GEDaily)), 
                               kurtosis(na.contiguous(GEWeekly)), 
                               kurtosis(na.contiguous(GEMonthly))),
                      
                      "IBM" = c(kurtosis(na.contiguous(IBMDaily)), 
                                kurtosis(na.contiguous(IBMWeekly)), 
                                kurtosis(na.contiguous(IBMMonthly))),
                      
                      "FTSE" = c(kurtosis(na.contiguous(FTSEDaily)), 
                                 kurtosis(na.contiguous(FTSEWeekly)), 
                                 kurtosis(na.contiguous(FTSEMonthly))),
                      
                      "DJI" = c(kurtosis(na.contiguous(DJIDaily)), 
                                kurtosis(na.contiguous(DJIWeekly)), 
                                kurtosis(na.contiguous(DJIMonthly))),
                      
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

