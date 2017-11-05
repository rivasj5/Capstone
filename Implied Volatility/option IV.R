
stock <- getSymbols(stock, src = 'google')
option <- getOptionChain(stock, Exp = NULL)
option.links <- names(option)
option.links <- gsub("[.]"," ",option.links) 
option.links <- format(as.Date(option.links, "%b %d %Y"), "%d %B %Y")
strike.1 <- option[op.chn]
strike.1 <- unlist(strike.1, rec = F)
strike.1.call <- strike.1[1]
strike.1.call <- as.data.frame(strike.1.call[1])

underlying <- get(stock)[length(get(stock)[,1]),4]

iv <- seq(length(strike.1.call[,1]))
n <- length(strike.1.call[,1])

# IV for all strike prices ####
for(i in 1:n){
  tryCatch({
    iv[i] <- AmericanOptionImpliedVolatility(type= "call", value=as.numeric(strike.1.call[i,2]), underlying=as.numeric(underlying),
                                strike=as.numeric(strike.1.call[i,1]), dividendYield=0.10, riskFreeRate=0.03,
                                maturity=(days.tm/365), volatility=0.4, timeSteps=timeSteps)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

iv <- as.data.frame(iv)
strike.1.call$IV <- iv
colnames(strike.1.call) <- c("strike","last","chg","bid","ask","vol","OI","IV")


