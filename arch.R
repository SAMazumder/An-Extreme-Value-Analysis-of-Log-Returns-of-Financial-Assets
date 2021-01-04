# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())




n=1000

beta = 1
lambda = 0.5


set.seed(1)
X = rnorm(n)
epsilon=rnorm(n)

sigma2=rep(beta,n)

for(t in 3:n){
  sigma2[t]= beta + lambda*epsilon[t-1]^2
  epsilon[t]=X[t]*sqrt(sigma2[t])
  }


layout(matrix(c(1:2), 2, 1,
              byrow = FALSE))


plot(epsilon,type="l",ylim=c(min(epsilon)-.5,max(epsilon)),  ylab = "Epsilon",
     main = "Simulated ARCH(1)")
hist(epsilon, # histogram
     breaks = 50,
     col="peachpuff", # column color
     border="black",
     freq = FALSE, # show densities instead of frequencies
     xlab = "Epsilon",
     main = "Simulated ARCH(1)")



# require('fGarch')
# 
# spec = garchSpec(model = list(alpha=0.5, beta = 0), cond.dist = "norm")
# x<-garchSim(spec, n = 5000)
# 
# 
# layout(matrix(c(1:2), 2, 1,
#               byrow = FALSE))
# plot(x,type='l',ylab=expression(r[t]),xlab='t', main = "Simulated ARCH(1)")
# 
# hist(x, ylab="density",xlab=expression(r[t]), main = "Histogram of Simulated ARCH(1) Process ")
# 
# 
# 
stats::acf(epsilon, na.action = na.omit, lag.max = length(epsilon), main ="ACF of Simulated ARCH(1)")
stats::acf(abs(epsilon)*abs(epsilon), na.action = na.omit, lag.max = length(epsilon), main ="ACF of Squared Absolue Simulated ARCH(1)" )
# 
# 
library('fExtremes')
library('evir')
G <- hill(epsilon[epsilon>0], option = c("xi"),  end = 250)
G <- hill(-epsilon[epsilon<0], option = c("xi"),  end = 250)

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

m1 = Hillfunction(epsilon[epsilon>0], 45)



G <- hillplot(epsilon[epsilon>0], hill.type = "SmooHill")
