library(RQuantLib)
require(quantmod)

stock <- "AMZN"
days.tm <- 60
timeSteps <- 100
op.chn <- 3

source("/Users/johnrivas/OneDrive - University of Houston Downtown/Quant Investing System/R - Quantstrat/Implied Volatility/option IV.R")

as.numeric(underlying)
n <- strike.1.call$IV < 1
strike.1.call[n,]
