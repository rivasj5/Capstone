getSymbols(stock[a],src='google' ,
           from = as.Date(today(tzone = Sys.timezone(location = TRUE))-days.back),
           to = as.Date(today(tzone = Sys.timezone(location = TRUE))-days.back2))
x <- log(get(stock[a])[,4]/lag(get(stock[a])[,4],k=1))
x <- cbind(get(stock[a])[,4],round(x,4))
x[,2] <- x[,2]*sqrt(252)
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

D <- as.Date(today(tzone = Sys.timezone(location = TRUE))-days.back2)
D <- seq.Date(D, D+4,"days")
final <- as.xts(final, order.by = D)

getSymbols(stock[a],src='google' , to = as.Date(today(tzone = Sys.timezone(location = TRUE))))

x <- log(get(stock[a])[,4]/lag(get(stock[a])[,4],k=1))
x <- cbind(get(stock[a])[,4],round(x,4))
x[,2] <- x[,2]*sqrt(252)
x <- x[-1,]

len.x <- length(x[,2])
for(i in 1:len.x){
  x$HV.10[i] <- sqrt(var(x[abs(i-num):i,2]))
}

plot(x[((len.x-4):len.x-9),'HV.10'], main = paste("Actual of Predicted 5 Day Period"))
plot(final, type = "l", main = paste("Predicted 5 Day Period"))