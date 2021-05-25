# load the required libraries
library(rpart)
suppressPackageStartupMessages(library(Metrics)) # Used for calculating root mean squared log error
suppressPackageStartupMessages(library(caret))  # For data partition
suppressPackageStartupMessages(library(rpart.plot)) # For plotting the model

selected$V1 = as.factor(selected$V1)
selected$V1 = as.integer(selected$V1)
train_set = selected[1:5917,]
test_set = selected[5917:6574,]

model_rf <- rpart(V1 ~ V2,data=train_set)

prediction_set <- predict(model_rf,test_set)

rmse <- sqrt(mean((test_set$V1 - prediction_set) ^ 2))
rmsel <- rmse(test_set$V1, prediction_set)
paste("The root mean square error is",rmse,sep=" ")

log_error <- rmsle(test_set$V1, prediction_set)
paste("The root mean square log error is",log_error,sep=" ")

rpart.plot(model_rf)

# save the results
test_set_predictions <- predict(model_rf,test_set,header=FALSE)
test_set_predictions <- cbind(ID=test_set$Id,SalePrice=test_set_predictions)

write.csv(test_set_predictions,"test_set_predictions.csv",row.names=FALSE)

# Random Forest classifier
library(randomForest)

#applying Random Forest
model_rf <- randomForest(V1 ~ V2, data = train_set)

preds <- predict(model_rf,test_set)

table(preds)
accuracy(preds, test_set$V1) #checking accuracy
preds

