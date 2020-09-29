install.packages("caret", dependencies = TRUE)
install.packages("randomForest")

library(rpart.plot)
data = read.csv("london_merged.csv")
summary(data)
data$timestamp <- NULL
data$is_holiday = as.factor(data$is_holiday)
data$is_weekend = as.factor(data$is_weekend)
data$season = as.factor(data$season)
data$weather_code = as.factor(data$weather_code)
summary(data)

inTrain = sort(sample(1:nrow(data), nrow(data)*.7, replace=F))
trainData = data.frame(data[inTrain,]) 
testData = data.frame(data[-inTrain,])

# Linear regression
fit = glm(cnt~., data=trainData)
summary(fit)
# test correlation
cor(data$cnt, data$t1)
cor(data$cnt, data$t2)
cor(data$cnt, data$hum)
cor(data$cnt, data$wind_speed)

scatter.smooth(x=trainData$wind_speed, y=trainData$cnt)
t2 = data$cnt < 3000
plot(density(data$cnt), main="Density Plot: New Bike Share", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(data$cnt), 2)))
polygon(density(data$cnt), col="red")

lm_prediction = predict(fit, newdata=testData)
lm_actuals_preds = data.frame(cbind(actuals=testData$cnt, predicted=lm_prediction))
lm_correlation_accuracy <- cor(lm_actuals_preds)
lm_min_max_accuracy <- mean(apply(lm_actuals_preds, 1, min) / apply(lm_actuals_preds, 1, max))  
lm_mape <- mean(abs((lm_actuals_preds$predicted - lm_actuals_preds$actuals))/lm_actuals_preds$actuals)  

# Decision Tree
best_model = rpart(cnt~t1+t2+hum+wind_speed+weather_code+is_holiday+is_weekend+season, data=trainData, maxdepth=10, minsplit=10, minbucket=3)
rpart.plot(best_model)
overfitted_model = rpart(cnt~t1+t2+hum+wind_speed+weather_code+is_holiday+is_weekend+season, data=trainData, maxdepth=2, minsplit=10, minbucket=3)
rpart.plot(overfitted_model)
summary(best_model)
dt_prediction = predict(best_model, newdata=testData)
dt_actuals_preds = data.frame(cbind(actuals=testData$cnt, predicted=dt_prediction))
dt_correlation_accuracy <- cor(dt_actuals_preds)
dt_min_max_accuracy <- mean(apply(dt_actuals_preds, 1, min) / apply(dt_actuals_preds, 1, max))  
dt_mape <- mean(abs((dt_actuals_preds$predicted - dt_actuals_preds$actuals))/dt_actuals_preds$actuals) 

# Random forest
library(caret)
library(randomForest)
rf_model <- train(cnt ~ t1 + t2 + hum + wind_speed + weather_code + is_holiday+ is_weekend+ season, data = trainData, method = 'rf', trControl = trainControl(method = 'cv', number = 5))
rf_model
rf_model2 <- randomForest(cnt~.,data=trainData,mtry=13,importance=TRUE)
rf_model3 <- randomForest(cnt~.,data=trainData,mtry=6,importance=TRUE)


#graphs
plot(data$weather_code, data$cnt, type="p")
plot(data$season, data$cnt, type="p")
plot(data$is_weekend, data$cnt, type="p")
plot(data$is_holiday, data$cnt, type="p")
plot(data$t1, data$t2, type="l")
hist(data$cnt)
hist(data$wind_speed)
hist(data$t1)
hist(data$t2)
hist(data$hum)
scatter.smooth(data$hum, data$t1)
scatter.smooth(data$weather_code, data$cnt)
scatter.smooth(data$t1, data$t2, main="Difference between actual temp and feels like (C)", xlab="Actual", ylab="Feels like")



