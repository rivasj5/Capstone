# Libraries
library(quantmod)
library(lubridate)
library(RSNNS)
library(plotrix)

getSymbols(stock,src='google' , to = as.Date(today(tzone = Sys.timezone(location = TRUE))-days.back))

x <- log(to.weekly(get(stock[a])[,4])[,4]/to.weekly(lag(get(stock[a])[,4],k=1))[,4])
x <- cbind(to.weekly(get(stock[a])[,4])[,4],round(x,4))
x[,2] <- x[,2]*sqrt(52)
x <- x[-1,]

len.x <- length(x[,2])
for(i in 1:len.x){
  x$HV.10[i] <- sqrt(var(x[abs(i-num):i,2]))
}
#par(mfrow=c(3,1))
#plot(x[((len.x-180):len.x),'HV.10'])

x <- na.exclude(x)

source("/Users/johnrivas/OneDrive - University of Houston Downtown/Quant Investing System/R - Quantstrat/Historical Volatility/Weekly/HV Weekly Max Min.R")

# Scaling function
range_data <- function(x){
  (x-min.x)/(max.x-min.x)
}
# Unscale function
unscale <- function(x, min.AMZN,max.AMZN){
  x * (max.AMZN - min.AMZN ) + min.AMZN
}

# Scale data, lags, min & max ####
x.sc <- range_data(x[,3])

x1 <- lag(x.sc, k=1)
x2 <- lag(x.sc, k=2)
x3 <- lag(x.sc, k=3)
x4 <- lag(x.sc, k=4)
x5 <- lag(x.sc, k=5)
x6 <- lag(x.sc, k=6)
x7 <- lag(x.sc, k=7)
x8 <- lag(x.sc, k=8)
x9 <- lag(x.sc, k=9)
x10 <- lag(x.sc, k=10)

xx <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
xx <- cbind(x.sc,xx)
xx <- xx[-(1:10),]
xx <- na.exclude(xx)
n = nrow(xx)

# Model ####
set.seed(seed = seed)
n_train <- round(n*n_train_percent)
train <- sample(1:n, n_train, FALSE)
inputs <- xx[,2:11]
outputs <- xx[,1]
fit <- elman(inputs[train],outputs[train],
             size = size,
             maxit = maxit)

################## Prediction

getSymbols(stock[a],src='google' ,
           from = as.Date(today(tzone = Sys.timezone(location = TRUE))-days.back),
           to = as.Date(today(tzone = Sys.timezone(location = TRUE))-1))
x <- log(to.weekly(get(stock[a])[,4])[,4]/to.weekly(lag(get(stock[a])[,4],k=1))[,4])
x <- cbind(to.weekly(get(stock[a])[,4])[,4],round(x,4))
x[,2] <- x[,2]*sqrt(52)
x <- x[-1,]

len.x <- length(x[,2])
for(i in 1:len.x){
  x$HV.10[i] <- sqrt(var(x[abs(i-num):i,2]))
}

x <- na.exclude(x)

# Scaling function
range_data <- function(x){
  (x-min.x)/(max.x-min.x)
}
# Unscale function
unscale <- function(x, min.AMZN,max.AMZN){
  x * (max.AMZN - min.AMZN ) + min.AMZN
}

# Scale data, lags, min & max ####
x.sc <- range_data(x[,3])

x1 <- lag(x.sc, k=1)
x2 <- lag(x.sc, k=2)
x3 <- lag(x.sc, k=3)
x4 <- lag(x.sc, k=4)
x5 <- lag(x.sc, k=5)
x6 <- lag(x.sc, k=6)
x7 <- lag(x.sc, k=7)
x8 <- lag(x.sc, k=8)
x9 <- lag(x.sc, k=9)
x10 <- lag(x.sc, k=10)

xx <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
xx <- cbind(x.sc,xx)
xx <- xx[-(1:10),]
xx <- na.exclude(xx)

pred1 <- predict(fit, xx[length(xx[,1]),1:10])
new1 <- cbind(t(pred1),xx[length(xx[,1]),1:9])
pred2 <- predict(fit, new1)
new2 <- cbind(t(pred2),new1[,1:9])
pred3 <- predict(fit, new2)
new3 <- cbind(t(pred3),new2[,1:9])
pred4 <- predict(fit, new3)
new4 <- cbind(t(pred4),new3[,1:9])
pred5 <- predict(fit, new4)
new5 <- cbind(t(pred5),new3[,1:9])

# Combine all predicted values ####
final <- c(
  unscale(pred1,min.x,max.x),
  unscale(pred2,min.x,max.x),
  unscale(pred3,min.x,max.x),
  unscale(pred4,min.x,max.x),
  unscale(pred5,min.x,max.x))

assign(paste("final",stock[a]),final)

yy <- paste("final",stock[a])
y[,yy] <- max(get(paste("final",stock[a]))) - min(get(paste("final",stock[a])))

#D <- as.Date(today(tzone = Sys.timezone(location = TRUE))-days.back2)
#D <- seq.Date(D, D+4,"days")
#final <- as.xts(final, order.by = D)

getSymbols(stock[a],src='google' , to = as.Date(today(tzone = Sys.timezone(location = TRUE))))

x <- log(to.weekly(get(stock[a])[,4])[,4]/to.weekly(lag(get(stock[a])[,4],k=1))[,4])
x <- cbind(to.weekly(get(stock[a])[,4])[,4],round(x,4))
x[,2] <- x[,2]*sqrt(52)
x <- x[-1,]

len.x <- length(x[,2])
for(i in 1:len.x){
  x$HV.10[i] <- sqrt(var(x[abs(i-num):i,2]))
}

#par(mfrow=c(2,1))
#plot(x[((len.x-4):len.x-9),'HV.10'])
#plot(final, type = "l")

par(mfrow=c(1,1))
getSymbols(stock[a],src='google')

x <- log(to.weekly(get(stock[a])[,4])[,4]/to.weekly(lag(get(stock[a])[,4],k=1))[,4])
x <- cbind(to.weekly(get(stock[a])[,4])[,4],round(x,4))
x[,2] <- x[,2]*sqrt(52)
x <- x[-1,]

len.x <- length(x[,2])
for(i in 1:len.x){
  x$HV.10[i] <- sqrt(var(x[abs(i-num):i,2]))
}
#plot(x[((len.x-30):len.x),'HV.10'])


