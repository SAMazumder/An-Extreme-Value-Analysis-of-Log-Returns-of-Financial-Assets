# Clear plots
if(!is.null(dev.list())) dev.off()

library(qrmtools)

n <- 2e5 # sample size = number of iid random variables
th <- 2 # parameter theta
set.seed(271) # set seed for reproducibility
X <- rPar(n, shape = th) # generate data

Y <- rnorm(n, mean = 0, sd = 1) # generate data


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

######Pareto################

Xs <- rev(sort(X))
n=length(Xs)


p1<-plot(log((n+1)/(1:n)), log(Xs), 
     main = "PP Plot for Complete Data Set",
     xlab = "Standard exponential quantiles",
     ylab = expression("lnX"[(n-j+1)]))

Xp <- Xs[1:floor(0.1*length(Xs))]
n=length(Xp)

p2<- plot(log((n+1)/(1:n)), log(Xp), 
     main = "PP Plot for Largest 10% of Data Set",
     xlab = "Standard exponential quantiles",
     ylab = expression("lnX"[(n-j+1)]))
abline(lm(log(Xp)~log((n+1)/(1:n))), col = "red")

library('evir')
P <- hill(X, option = c("xi"),  end = 10000)



Hillfunction(X, 2000)
Hillfunction(X, 3000)
Hillfunction(X, 4000)


#########NORMAL#############

Ys <- Y[Y > 0]
Ys <- rev(sort(Y))
n=length(Ys)


pn1<-plot(log((n+1)/(1:n)), log(Ys), 
         main = "PP Plot for Complete Data Set",
         xlab = "Standard exponential quantiles",
         ylab = expression("lnX"[(n-j+1)]))

Yp <- Ys[1:floor(0.1*length(Ys))]
n=length(Yp)

pn2<- plot(log((n+1)/(1:n)), log(Yp), 
          main = "PP Plot for Largest 10% of Data Set",
          xlab = "Standard exponential quantiles",
          ylab = expression("lnX"[(n-j+1)]))
abline(lm(log(Yp)~log((n+1)/(1:n))), col = "red")

G <- hill(Y[Y > 0], option = c("xi"),  end = 5000)


Hillfunction(Ys, 2000)
Hillfunction(Ys, 3000)
Hillfunction(Ys, 4000)