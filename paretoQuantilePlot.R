install.packages("evir")
library('evir')

data <- read.csv("data.csv",header=T, sep=",") #Data has headers and seperated by ","
attach(data)

X <- diff(log(SPX.Index),lag = 1)

Xs <- X[X > 0]
Xs <- rev(sort(Xs))
n = n=length(Xs)


layout(matrix(c(1:2), 2, 1,
              byrow = FALSE))
plot(log((n+1)/(1:n)), log(Xs), 
     xlab = "Standard exponential quantiles",
     ylab = expression("lnX"[(n-j+1)]))

Xp <- Xs[1:floor(0.1*length(Xs))]
n=length(Xp)

plot(log((n+1)/(1:n)), log(Xp), 
     xlab = "Standard exponential quantiles",
     ylab = expression("lnX"[(n-j+1)]))
abline(lm(log(Xp)~log((n+1)/(1:n))), col = "red")


