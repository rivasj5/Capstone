# Libraries  ####
library(quantmod)
library(lubridate)
library(RSNNS)
library(plotrix)

# Stock to research ####
stock <- c("AZO","CALI","AMZN")

# Create plots for stock variable
for(a in 1:length(stock)){

today(tzone = Sys.timezone(location = TRUE))
getSymbols(stock,src='google' , to = as.Date(today(tzone = Sys.timezone(location = TRUE))-20))

# Create the log() of prior periods return
x <- log(get(stock[a])[,4]/lag(get(stock[a])[,4],k=1))
x <- cbind(get(stock[a])[,4],round(x,4))
x[,2] <- x[,2]*sqrt(252)
x <- x[-1,]

name <- paste(stock[a],"pdf")
pdf(name)
par(mfrow=c(3,2))
len.x <- length(x[,2])
# Plots ####
num <- 1
for(i in 1:len.x){
 x$HV.5[i] <- sqrt(var(x[abs(i-num):i,2]))
}
plot(x[((len.x-252):len.x),'HV.5'], main = paste(num," Day Volatility -", stock[a]))

len <- length(get(stock[a])[,1])
plot(get(stock[a])[(len-252):len,4], main = paste("Price of ", stock[a]))

num <- 5
for(i in 1:len.x){
  x$HV.10[i] <- sqrt(var(x[abs(i-num):i,2]))
}
plot(x[((len.x-252):len.x),'HV.10'], main = paste(num," Day Volatility -", stock[a]))

plot(get(stock[a])[(len-252):len,4], main = paste("Price of ", stock[a]))
num <- 10
for(i in 1:len.x){
  x$HV.20[i] <- sqrt(var(x[abs(i-num):i,2]))
}
plot(x[((len.x-252):len.x),'HV.20'], main = paste(num," Day Volatility -", stock[a]))

plot(get(stock[a])[(len-252):len,4], main = paste("Price of ", stock[a]))
dev.off()
}

