more.stocks <- c("AAPL", "CALI", "ADI", "ADP", "ADSK", "AKAM", "ALXN","AMAT", "AMGN", "AMZN",
                  "ATVI", "AVGO", "BBBY", "BIDU", "BIIB", "CA", "CELG", "CERN",  "CHKP","CHRW",
                  "CHTR", "CMCSA","COST", "CSCO", "CTSH", "CTXS", "DISCA", "DISCK","DISH","DLTR",
                  "EBAY", "EQIX", "ESRX", "EXPD", 
                  "EXPE", "FAST","FB", "FFIV", "FISV", 
                  "FOXA", "GILD", "GOOG", "GOOGL",
                  "GRMN", "HSIC", "ILMN", "INTC", "INTU",
                  "ISRG", "KLAC", "LBTYA",  
                  "LMCK", "MAR", "MAT", 
                  "MDLZ", "MNST", "MSFT", "MU", "MXIM",
                  "MYL", "NFLX", "NTAP", "NVDA", "NXPI",
                  "ORLY", "PAYX", "PCAR", "PCLN", "QCOM", 
                  "QVCA", "REGN", "ROST",  "SBUX",
                  "SIAL", "SIRI", "SNDK", "SPLS", "SRCL",
                  "STX", "SYMC", "TRIP", "TSCO", "TSLA",
                  "TXN", "VIAB", "VIP", "VOD", "VRSK",
                  "VRTX", "WDC", "WFM", "WYNN", "XLNX", "YHOO")

stock <- more.stocks[1:75]

# "ALTR", "BRCM","CTRX", "GMCR","KRFT", "LLTC", "LMCA", "LVNTA","SBAC","DTV",

y <- 0
y <- as.data.frame(y)

# End of more Stocks####

len <- length(stock)

for(i in 1: len){
  a <- i
  num <- 10
  size <- c(25)
  maxit <- 1000
  n_train_percent <- 0.70
  seed <- 123
  days.back <- 200
  days.back2 <- 100
  source("/Users/johnrivas/OneDrive - University of Houston Downtown/Quant Investing System/R - Quantstrat/Historical Volatility/Weekly/HV all Weekly Current.R")
}

t(sort(y,decreasing = TRUE))


