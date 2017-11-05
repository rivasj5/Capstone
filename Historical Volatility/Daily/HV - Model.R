# Libraries
library(quantmod)
library(lubridate)
library(RSNNS)
library(plotrix)

getSymbols(stock[a],src='google' , to = as.Date(today(tzone = Sys.timezone(location = TRUE))-days.back))

# Calculating the volatility
x <- log(get(stock[a])[,4]/lag(get(stock[a])[,4],k=1))
x <- cbind(get(stock[a])[,4],round(x,4))
x[,2] <- x[,2]*sqrt(252)
x <- x[-1,]


getSymbols(stock = "DOW", src = 'google')


head(get(stock[a])[,4])
head(lag(get(stock[a])[,4],k=1))

len.x <- length(x[,1])
for(i in 1:len.x){
  x$HV.10[i] <- sqrt(var(x[abs(i-num):i,2]))
}

x <- na.exclude(x)

source("/Users/johnrivas/OneDrive - University of Houston Downtown/Quant Investing System/R - Quantstrat/Historical Volatility/Daily/HV - Model Max Min.R")

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
n <- nrow(xx)

# Model ####
set.seed(seed = seed)
n_train <- round(n*n_train_percent)
train <- sample(1:n, n_train, FALSE)
inputs <- xx[,2:11]
outputs <- xx[,1]
fit <- elman(inputs[train],outputs[train],
             size = size,
             maxit = maxit,
             learnFuncParams=c(0.1,0.2))

?elman

AAPL

pred <- predict(fit, inputs[-train])
round(cor(outputs[-train],pred)^2,4)

par(mfrow=c(3,2))
plot(x[1:(len.x-days.back),'HV.10'], main = paste(num, " day Volatility"))
plotIterativeError(fit, main = paste("Neural Net Structure - ",list(size), " hidden nodes"))

plotRegressionError(as.numeric(outputs[-train]),pred, main = paste("Fitted Value vs. Actual Value"))
text(0.9, 0.1,round(cor(outputs[-train],pred)^2,4) ,
     cex = 1.5)

getSymbols(stock[a],src='google')

x <- log(get(stock[a])[,4]/lag(get(stock[a])[,4],k=1))
x <- cbind(get(stock[a])[,4],round(x,4))
x[,2] <- x[,2]*sqrt(252)
x <- x[-1,]

len.x <- length(x[,2])
for(i in 1:len.x){
  x$HV.10[i] <- sqrt(var(x[abs(i-num):i,2]))
}
plot(x[((len.x-30):len.x),'HV.10'])



