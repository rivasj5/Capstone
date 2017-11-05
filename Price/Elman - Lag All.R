# Libraries and data ####
library(quantmod)
library(lubridate)
library(RSNNS)
library(plotrix)

stock <- "AMZN"

getSymbols(stock,src='google')

# decomp <- decompose(AMZN.log, type = "multiplicative")
# plot(decomp)
# decomp2 <- decompose(AMZN.log, type = "additive")
# plot(decomp2)
# plot (decomp$trend, ylab=" Value ")
# plot(density(na.omit(decomp$random)))
# qqnorm(na.omit(decomp$random))
# acf(AMZN.log)
# pacf(AMZN.log)

# Scaling functions####
# Scale function
range_data <- function(x){
  (x-min(x))/(max(x)-min(x))
}
# Unscale function
unscale <- function(x, min.stock,max.stock){
  x * (max.stock - min.stock ) + min.stock
}

# Scale data, lags, min & max ####
stock.scale <- range_data(get(stock))
min.stock <- min(get(stock)[,4])
max.stock <- max(get(stock)[,4])

#stock.scale <- as.zoo(stock.scale)
x1 <- lag(stock.scale, k=1)
x2 <- lag(stock.scale, k=2)
x3 <- lag(stock.scale, k=3)
x4 <- lag(stock.scale, k=4)
x5 <- lag(stock.scale, k=5)
x6 <- lag(stock.scale, k=6)
x7 <- lag(stock.scale, k=7)
x8 <- lag(stock.scale, k=8)
x9 <- lag(stock.scale, k=9)
x10 <- lag(stock.scale, k=10)

x <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)

x <- cbind(stock.scale,x)

x <- x[-(1:10),]

x <- as.zoo(x)

n = nrow(x)

# Model ####
set.seed(998)
n_train <- round(n*0.75)
train <- sample(1:n, n_train, FALSE)

inputs <- x[,c(1:3,5)]
outputs <- x[,4]

fit <- elman(inputs[train],outputs[train],
             size = c(10,10),
             maxit = 100)

plotIterativeError(fit)
 
plotRegressionError(outputs[train],fit$fitted.values)
 
round(cor(outputs[train],fit$fitted.values)^2,4)

x.len <- length(x[,4])
pred <- predict(fit, x[,-4])

# round(cor(outputs[-train],pred)^2,4)

# Unscale function ####
unscale <- function(x, min.AMZN,max.AMZN){
  x * (max.AMZN - min.AMZN ) + min.AMZN
}

pred.un <- unscale(pred,min.stock,max.stock)

# Predicted Values 10 days out ####
new.1 <- cbind(t(pred[length(pred),]),x[length(x[,1]),3:11])
pred2 <- predict(fit, new.1)
new.2 <- cbind(t(pred2), new.1[,1:9])
pred3 <- predict(fit, new.2)
new.3 <- cbind(t(pred3), new.2[,1:9])
pred4 <- predict(fit, new.3)
new.4 <- cbind(t(pred4), new.3[,1:9])
pred5 <- predict(fit, new.4)
new.5 <- cbind(t(pred4), new.4[,1:9])
pred6 <- predict(fit, new.5)
new.6 <- cbind(t(pred5), new.5[,1:9])
pred7 <- predict(fit, new.6)
new.7 <- cbind(t(pred6), new.6[,1:9])
pred8 <- predict(fit, new.7)
new.8 <- cbind(t(pred7), new.7[,1:9])
pred9 <- predict(fit, new.8)
new.9 <- cbind(t(pred8), new.8[,1:9])
pred10 <- predict(fit, new.9)
new.10 <- cbind(t(pred9), new.9[,1:9])


# Combine all predicted values ####
final <- c(
unscale(pred,min.AMZN,max.AMZN)[length(pred)],
unscale(pred2,min.AMZN,max.AMZN),
unscale(pred3,min.AMZN,max.AMZN),
unscale(pred4,min.AMZN,max.AMZN),
unscale(pred5,min.AMZN,max.AMZN),
unscale(pred6,min.AMZN,max.AMZN),
unscale(pred7,min.AMZN,max.AMZN),
unscale(pred8,min.AMZN,max.AMZN),
unscale(pred9,min.AMZN,max.AMZN),
unscale(pred10,min.AMZN,max.AMZN))

final.max <- max(final)
final.min <- min(final)
final.matrix <- as.matrix(final)
final.matrix <- cbind(final.matrix,seq(10))

return.stock <- abs((as.numeric(get(stock)[length(get(stock)[,4]),4]-final.matrix[1,1])/final.matrix[1,1])*100+
                      (final.matrix[2,1]-final.matrix[1,1])/final.matrix[1,1]+
                      (final.matrix[3,1]-final.matrix[2,1])/final.matrix[2,1]+
                      (final.matrix[4,1]-final.matrix[3,1])/final.matrix[3,1]+
                      (final.matrix[5,1]-final.matrix[4,1])/final.matrix[4,1]+
                      (final.matrix[6,1]-final.matrix[5,1])/final.matrix[5,1])

# Plot last 30 days and future 10 predicted days ####
par(mfrow=c(1,1))
xy <- get(stock)[,4]
xy1 <- length(xy)-30
xy2 <- length(xy)
xy <- xy[xy1:xy2,]
plot(xy, main = paste(stock, " - Actual 30 days"),sub = get(stock)[length(get(stock)[,1]),4])
plot(final, type = "l", main = paste(stock, " - Predicted 10 days - ",today()),
     ylim = c(final.min,final.max), sub = return.stock)

text(final.matrix[,2], 
     final.matrix[,1], 
     round(final.matrix[,1],2), 
     cex=0.6, pos=4, col="red") 

