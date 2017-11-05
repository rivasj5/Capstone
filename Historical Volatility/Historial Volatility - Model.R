# Scaling function
range_data <- function(x){
  (x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))
}
# Unscale function
unscale <- function(x, min.AMZN,max.AMZN){
  x * (max.AMZN - min.AMZN ) + min.AMZN
}

# Scale data, lags, min & max ####
x.sc <- range_data(x[,4])
min.x <- min(x[,4], na.rm = TRUE)
max.x <- max(x[,4], na.rm = TRUE)

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
set.seed(123)
n_train <- round(n*0.70)
train <- sample(1:n, n_train, FALSE)
inputs <- xx[,2:11]
outputs <- xx[,1]
fit <- elman(inputs[train],outputs[train],
             size = c(25,25),
             maxit = 1000)

plotIterativeError(fit)
plotRegressionError(as.numeric(outputs[train]),fit$fitted.values)

pred <- predict(fit, inputs[-train])
round(cor(outputs[-train],pred)^2,4)

getSymbols(stock,src='google' , from = as.Date(today(tzone = Sys.timezone(location = TRUE))-20))
x <- log(get(stock[a])[,4]/lag(get(stock[a])[,4],k=1))
x <- cbind(get(stock[a])[,4],round(x,4))
x[,2] <- x[,2]*sqrt(252)
x <- x[-1,]




new.inputs <- x[length(x[,1]),]
pred1 <- predict(fit, new.inputs[,2:11])
unscale(xx[length(xx[,1]),2],min.x,max.x)



