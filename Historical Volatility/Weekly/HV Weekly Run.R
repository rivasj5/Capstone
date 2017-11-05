stock <- c("CALI","AMZN",
           "GAZ","GRN",
           "AAPL","AZO",
           "GOOGL","ADSK")
a <- 1
num <- 10
size <- c(25)
maxit <- 1000
n_train_percent <- 0.70
seed <- 123
days.back <- 200
days.back2 <- 100
#y <- 1
#y <- as.data.frame(y)

# Hide ####
source("/Users/johnrivas/OneDrive - University of Houston Downtown/Quant Investing System/R - Quantstrat/HV Weekly.R")

#plotIterativeError(fit)
#plotRegressionError(as.numeric(outputs[train]),fit$fitted.values)
#pred <- predict(fit, inputs[-train])
#round(cor(outputs[-train],pred)^2,4)

source("/Users/johnrivas/OneDrive - University of Houston Downtown/Quant Investing System/R - Quantstrat/HV Weekly Pred.R")

# CALI 25
# AMZN 25
# GAZ 27
# ####

t(sort(y, decreasing = TRUE))
