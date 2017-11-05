getSymbols(stock[a],src='google')
x <- log(to.weekly(get(stock[a])[,4])[,4]/to.weekly(lag(get(stock[a])[,4],k=1))[,4])
x <- cbind(to.weekly(get(stock[a])[,4])[,4],round(x,4))
x[,2] <- x[,2]*sqrt(52)
x <- x[-1,]

len.x <- length(x[,2])
for(i in 1:len.x){
  x$HV.10[i] <- sqrt(var(x[abs(i-num):i,2]))
}

x <- na.exclude(x)

min.x <- min(x[,3], na.rm = TRUE)
max.x <- max(x[,3], na.rm = TRUE)
