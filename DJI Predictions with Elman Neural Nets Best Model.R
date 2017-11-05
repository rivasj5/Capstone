########### Individual Elman Neural Net Performance ####
# See index of best model from "DJI Predictions with Elman Neural Nets Best Model.R"
options(warn=-1)

# # # # # # Select model to see # # # # # # # # # # # # # # # # 
model <- elman.1

# # # # # # Run the rest of the file. # # # # # # # # # # # # #
# # # # # # Four plots should display in one window # # # # # #

par(mfrow=c(2,2))
# Error for iterations
plotIterativeError(model, main = "Error over Iterations")

# Scatter plot of testing vs. predicted
plotRegressionError(training[,1],model$fitted.values, main = "Training vs. Fitted Value")
text(0.6,0.2, 
     round(cor(training[,1],model$fitted.values),6), 
     cex=2, pos=4, col="red") 

# Performance on testing
pred <- predict(model,testing[,2:11])

# Unscale data and bind together
unscale(pred,min.dow,max.dow)
unscale(testing[,1],min.dow,max.dow)
final <- cbind(unscale(pred,min.dow,max.dow),unscale(testing[,1],min.dow,max.dow))
final <- as.data.frame(final)

# Plot final actual and predicted values
plot(final[,2],type="l",col="red", ylab = "DOW", main = "Actuals vs Predicted")
lines(final[,1],col="green")
text(5100,1000, 
     round(cor(testing[,1],pred),6), 
     cex=2, pos=4, col="red") 

###########################################################
########### Predicted Values future values as of today ####
###########################################################

# Last ten days
getSymbols("DJI")
new.final <- DJI[(length(DJI[,4])-9):length(DJI[,4]),4]
new.final <- rev(new.final)
new.final <- t(new.final)
new.final <- ((new.final - min.dow)/(max.dow - min.dow))

# New day prediction and new prediction set
new.pred <- predict(model,new.final)
new.set <- c(new.pred,new.final[,1:9])
new.pred2 <- predict(model, new.set)
new.set2 <- c(new.pred2,new.set[1:9])
new.pred3 <- predict(model, new.set2)
new.set3 <- c(new.pred3,new.set2[1:9])
new.pred4 <- predict(model, new.set3)
new.set4 <- c(new.pred4,new.set3[1:9])
new.pred5 <- predict(model, new.set4)
new.set5 <- c(new.pred5,new.set4[1:9])

# Combine final predictions
final.future <- c(
  unscale(new.pred,min.dow,max.dow),
  unscale(new.pred,min.dow,max.dow),
  unscale(new.pred2,min.dow,max.dow),
  unscale(new.pred3,min.dow,max.dow),
  unscale(new.pred4,min.dow,max.dow),
  unscale(new.pred5,min.dow,max.dow)
)

# Plot future values
plot(final.future, main = "Future Predicted Value as of Today", xlab = "Days in Future", ylab = "DOW",type = "l")

