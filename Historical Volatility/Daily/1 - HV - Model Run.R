# Create list of desired stocks to analyze ####
stock <- c("AMZN","ADI",
           "AAPL")
a <- 3

# Parameters for research ####

num <- 10                   # Number of days in time period to calculate volatility
size <- c(10,10)               # Structure of the neural net
maxit <- 500                # Number of Iterations 
n_train_percent <- 0.80     # Percentage for trainig set
seed <- 123                 # Seed # 
days.back <- 40             # End date for traning and testing
days.back2 <- 20            # End date for predicted period NOTE: The begging date for prediction is 20 days prior

# (Source Files) Hide ####
source("/Users/johnrivas/OneDrive - University of Houston Downtown/Quant Investing System/R - Quantstrat/Historical Volatility/Daily/HV - Model.R")
source("/Users/johnrivas/OneDrive - University of Houston Downtown/Quant Investing System/R - Quantstrat/Historical Volatility/Daily/HV - Model Pred.R")


