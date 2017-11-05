

# Scale function
range_data <- function(x){
  (x-min(x))/(max(x)-min(x))
}

AMZN.sc <- range_data(AMZN)

len1.1 <- length(AMZN)-19
len1.2 <- length(AMZN)

AMZN.sc <- range_data(AMZN[,4])
len.1 <- length(AMZN.sc)-19
len.2 <- length(AMZN.sc)

AMZN.sc <- AMZN.sc[len.2:len.1,]

min.AMZN <- min(AMZN[,4])
max.AMZN <- max(AMZN[,4])

x1 <- lag(AMZN.sc, k=1)
x2 <- lag(AMZN.sc, k=2)
x3 <- lag(AMZN.sc, k=3)
x4 <- lag(AMZN.sc, k=4)
x5 <- lag(AMZN.sc, k=5)

x <- cbind(x1,x2,x3,x4,x5)

x <- x[-(1:5),]

pred <- predict(fit, x)

unscale <- function(x, min_x,max_x){
  x * (max_x - min_x ) + min_x
}

unscale(pred,min.AMZN,max.AMZN)

par(mfrow=c(2,1))
plot(unscale(pred,min.AMZN,max.AMZN), type = "l")
getSymbols('AMZN',src='google',from = '2017-06-01')
plot(AMZN)
