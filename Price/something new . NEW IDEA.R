# Clear all and libraries ####
# rm(list=ls())
# Clear plot
# dev.off(dev.list()["RStudioGD"])
# Clear console
# cat("\014")

library(quantmod)
library(lubridate)
library(RSNNS)
library(plotrix)

getSymbols(stock,src='google')

#plot(AZO) ####


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
unscale <- function(x, min.AMZN,max.AMZN){
  x * (max.AMZN - min.AMZN ) + min.AMZN
}


# Scale data, lags, min & max ####
AMZN.sc <- range_data(get(stock)[,4])
min.AMZN <- min(get(stock)[,4])
max.AMZN <- max(get(stock)[,4])

#AMZN.sc <- as.zoo(AMZN.sc)
x1 <- lag(AMZN.sc, k=1)
x2 <- lag(AMZN.sc, k=2)
x3 <- lag(AMZN.sc, k=3)
x4 <- lag(AMZN.sc, k=4)
x5 <- lag(AMZN.sc, k=5)
x6 <- lag(AMZN.sc, k=6)
x7 <- lag(AMZN.sc, k=7)
x8 <- lag(AMZN.sc, k=8)
x9 <- lag(AMZN.sc, k=9)
x10 <- lag(AMZN.sc, k=10)

x <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)

x <- cbind(AMZN.sc,x)

x <- x[-(1:10),]

n = nrow(x)



# Model ####
set.seed(2017)
n_train <- round(n*0.75)
train <- sample(1:n, n_train, FALSE)

inputs <- x[,2:11]
outputs <- x[,1]

fit <- elman(inputs[train],outputs[train],
             size = c(10,10,5),
             maxit = 1000)

#plotIterativeError(fit)
# 
# plotRegressionError(outputs,fit$fitted.values)
# 
# round(cor(outputs[train],fit$fitted.values)^2,4)

pred <- predict(fit, inputs)

# round(cor(outputs[-train],pred)^2,4)


# Unscale function ####
unscale <- function(x, min.AMZN,max.AMZN){
  x * (max.AMZN - min.AMZN ) + min.AMZN
}


# Predicted Values 10 days out ####
new.1 <- cbind(pred[length(pred),],x[length(x[,1]),3:11])
pred2 <- predict(fit, new.1)
new.2 <- cbind(pred2, new.1[,1:9])
pred3 <- predict(fit, new.2)
new.3 <- cbind(pred3, new.2[,1:9])
pred4 <- predict(fit, new.3)
new.4 <- cbind(pred4, new.3[,1:9])
pred5 <- predict(fit, new.4)
new.5 <- cbind(pred4, new.4[,1:9])
pred6 <- predict(fit, new.5)
new.6 <- cbind(pred5, new.5[,1:9])
pred7 <- predict(fit, new.6)
new.7 <- cbind(pred6, new.6[,1:9])
pred8 <- predict(fit, new.7)
new.8 <- cbind(pred7, new.7[,1:9])
pred9 <- predict(fit, new.8)
new.9 <- cbind(pred8, new.8[,1:9])
pred10 <- predict(fit, new.9)
new.10 <- cbind(pred9, new.9[,1:9])


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
par(mfrow=c(2,1))
xy <- get(stock)[,4]
xy1 <- length(xy)-30
xy2 <- length(xy)
xy <- xy[xy1:xy2,]
plot(xy, main = paste(stock, " - Actual 30 days"))
plot(final, type = "l", main = paste(stock, " - Predicted 10 days - ",today()-1),
     ylim = c(final.min,final.max), sub = return.stock)

text(final.matrix[,2], 
     final.matrix[,1], 
     round(final.matrix[,1],2), 
     cex=0.6, pos=4, col="red") 


return.stock <- abs((as.numeric(get(stock)[length(get(stock)[,4]),4]-final.matrix[1,1])/final.matrix[1,1])*100+
                    (final.matrix[2,1]-final.matrix[1,1])/final.matrix[1,1]+
                    (final.matrix[3,1]-final.matrix[2,1])/final.matrix[2,1]+
                    (final.matrix[4,1]-final.matrix[3,1])/final.matrix[3,1]+
                    (final.matrix[5,1]-final.matrix[4,1])/final.matrix[4,1]+
                    (final.matrix[6,1]-final.matrix[5,1])/final.matrix[5,1])
  


