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

attach(data)
DATE <- as.Date(as.character(data$DATE), "%Y-%m-%d")
SP <- xts(SPX.Index, DATE)
#####S&P######
logSPDaily <- xts(log(SP), OHLC=FALSE)

#Create Daily TS
SPDaily <-  diff(logSPDaily, lag = 1)
#Create Weekly TS
SPWeekly <- diff(to.weekly(logSPDaily,OHLC=FALSE), lag =1)
#Create Monthly TS
SPMonthly <- diff(to.monthly(logSPDaily,OHLC=FALSE), lag =1)

#Create Daily TS
SPDaily <- diff(logSPDaily , lag = 1)
qqnorm(coredata(SPDaily), main = "S&P Log Daily Close QQ-plot")
qqline(coredata(SPDaily), col = "red")

#Create Weekly TS
SPWeekly <- diff(logSPDaily, lag = 5)
qqnorm(coredata(SPWeekly), main = "S&P Log Weekly Close QQ-plot")
qqline(coredata(SPWeekly), col = "red")


#Create Monthly TS
SPMonthly <- diff(logSPDaily, lag = 25)
qqnorm(coredata(SPMonthly), main = "S&P Log Monthly Close QQ-plot")
qqline(coredata(SPMonthly), col = "red")


set.seed(1234)
m1 = mean(coredata(SPDaily[-1]))
s1 = sd(coredata(SPDaily[-1]))

m2 = mean(coredata(SPWeekly[-(1:5)]))
s2 = sd(coredata(SPWeekly[-(1:5)]))

m3 = mean(coredata(SPMonthly[-(1:25)]))
s3 = sd(coredata(SPMonthly[-(1:25)]))



dat1<- rnorm(10000000, mean = (m1 - s1*s1/2), sd = s1)
dat2<- rnorm(10000000, mean = (m2 - s2*s2/2), sd = s2)
dat3<- rnorm(10000000, mean = (m3 - s3*s3/2), sd = s3)

#Create 100 day TS
SP100 <- diff(logSPDaily , lag = 100)
dat4<- rnorm(10000000, mean = (m1 - s1*s1/2)*100, sd = s1*sqrt(100))
             
hist(SP100, # histogram
     breaks = 100,
    col="peachpuff", # column color
    border="black",
    freq = FALSE, # show densities instead of frequencies
    xlab = "log returns", 
    main = "S&P 500 Daily Log Returns")
lines(density(dat4), # density plot
     lwd = 2, # thickness of line
     col = "chocolate3")


layout(matrix(c(1:6), 3, 2,
              byrow = FALSE))

hist(SPDaily, # histogram
     breaks = 50,
     col="peachpuff", # column color
     border="black",
     freq = FALSE, # show densities instead of frequencies
     xlab = "log returns",
     main = "S&P 500 Daily Log Returns")
lines(density(dat1), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")

hist(SPWeekly, # histogram
     breaks = 50,
     col="peachpuff", # column color
     border="black",
     freq = FALSE, # show densities instead of frequencies
     xlab = "log returns",
     main = "S&P 500 Weekly Log Returns")
lines(density(dat2), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")

hist(SPMonthly, # histogram
     breaks = 50,
     col="peachpuff", # column color
     border="black",
     freq = FALSE, # show densities instead of frequencies
     xlab = "log returns",
     main = "S&P 500 Monthly Log Returns")
lines(density(dat3), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")


chartSeries(SPDaily, type="line",name = "Daily Log Returns of S&P 500",TA=NULL, theme=chartTheme('white'), subset="last 15 years")

plot(SP, type="l",main = "Daily Closing Price of the S&P 500")

plot(SPDaily, type="l",main = "Daily Return of the S&P 500")

layout(matrix(c(1:10), 5, 2,
              byrow = FALSE))
plot(AAPLDaily, type="l",main = "Apple")
plot(GOOGDaily, type="l",main = "Google")
plot(MSFTDaily, type="l",main = "Microsoft")
plot(IBMDaily, type="l",main = "IBM")
plot(GEDaily, type="l",main = "GE")
plot(FTSEDaily, type="l",main = "FTSE")
plot(SPDaily, type="l",main = "S&P 500")
plot(TSXDaily, type="l",main = "TSX")
plot(DJIDaily, type="l",main = "DJI")
plot(DAXDaily, type="l",main = "DAX")

chartSeries(SP, type="line",name = "Daily Log Returns of S&P 500",TA=NULL, theme=chartTheme('white'), subset="last 15 years")




layout(matrix(c(1:1), 1, 1,
              byrow = FALSE))


# returns <- mydata
# returns_upper <- lapply(returns, hillPlot, doplot=FALSE, na.action = na.omit) #set doplot to false to speed up calculation
# returns_upper <- sapply(returns_upper, function(x) x$y) #extract the hill estimators
# returns_upper <- sapply(returns_upper, '[', seq(max(sapply(returns_upper,length)))) #create a data frame where each column has equal length (easier analysis)
# 
# returns_upper[100,]

