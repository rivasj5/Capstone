########### Library and data ####
setwd("/Users/johnrivas/OneDrive - University of Houston Downtown/Quant Investing System/Oct 2017")
library(quantmod)
library(lubridate)
library(RSNNS)
library(caret)
library(reshape)
dow <- read.csv("dow.csv")

########### Scaling and lagging the data ####
# Scale function
range_data <- function(x){
  (x-min(x))/(max(x)-min(x))
}
# Unscale function
unscale <- function(x, min.dow,max.dow){
  x * (max.dow - min.dow ) + min.dow
}

# Scale data, lags, min & max
min.dow <- min(dow[,2])
max.dow <- max(dow[,2])
dow.scale <- range_data(dow[,2])

# Create table with 10 columns. One for each day of lag. 
# For example, column x3 is a three day lag from yesters close price
x1 <- lag(dow.scale, k=1)
x2 <- lag(dow.scale, k=2)
x3 <- lag(dow.scale, k=3)
x4 <- lag(dow.scale, k=4)
x5 <- lag(dow.scale, k=5)
x6 <- lag(dow.scale, k=6)
x7 <- lag(dow.scale, k=7)
x8 <- lag(dow.scale, k=8)
x9 <- lag(dow.scale, k=9)
x10 <- lag(dow.scale, k=10)

# Bind all laggged columns. Remove top ten to remove NA's. 
x <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
x <- cbind(dow.scale,x)
x <- x[-(1:10),]
n <- nrow(x)
x <- na.omit(x) # Added this because i was getting an error about NA's when creating a model

########### Elman Neural Net ####
#Make training and test data sets global
set.seed(123)
intrain <- createDataPartition(y = x[,1],p = 0.8,list = FALSE) #split data
assign("training", x[intrain,] , envir = .GlobalEnv)
assign("testing",  x[-intrain,] , envir = .GlobalEnv)

# Create a matrix of all parameters
maxit <- as.data.frame(seq(from = 50, to = 100, by = 10))
size <- expand.grid.df(as.data.frame(seq(from =5, to = 40, by = 5)),as.data.frame(seq(from = 2, to = 20, by = 2)))
new.size <- c(size[1,1],size[1,2])

for(i in 1:80){
  x <- c(size[i,1],size[i,2])
  new.size <- rbind(new.size,x)
}

new.size <- new.size[-1,]
learnFunc <- rbind("JE_BP","JE_Rprop")
learnFuncParams <- as.data.frame(seq(from = 0.1, to = 0.9, by = 0.1))
grid <- expand.grid.df(maxit, learnFunc,learnFuncParams,new.size)

# Create models for all parameteres in "grid" (matrix of all parameters)
for(i in 1:length(grid[,1])){
  set.seed(123)
  assign(paste("elman.",i,sep = ""), elman(y = training[,1], x = training[,2:11],
                                  size = grid[i,4],
                                  maxit = grid[i,1],
                                  learnFunc = as.character(grid[i,2]),
                                  learnFuncParams = grid[i,3]))
}

# Error results for all models
error.min <- NA
for(i in 1:length(grid[,1])){
  error.min <- c(error.min,min(get(paste("elman.",i,sep = ""))$IterativeFitError))
}

error.min <- error.min[-1]

# Accuracy on testing data
acc <- NA
for(i in 1:length(grid[,1])){
  acc <- c(acc,cor(predict(get(paste("elman.",i,sep = "")),testing[,-1]),testing[,1]))
}

acc <- acc[-1]

# Combine parameters grid with results
results.grid <- cbind(grid[1:length(grid[,1]),],error.min,acc)
colnames(results.grid) <- c("maxit","learnFunc","learnFuncParms","size1","size2","error","acc")

#  the printed table below is ordered to have the most accurate model on top. Only top ten parameter combinations
print(results.grid[order(results.grid[1 : 10,6], decreasing = FALSE),])

#  the printed table below is ordered to have the most accurate model on top. Only top ten parameter combinations
print(results.grid[order(results.grid[1 : 10,7], decreasing = TRUE),])

