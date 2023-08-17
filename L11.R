# --------------------------------------------------------------
# L11 Bagging, Random Forest, Grid Search
# --------------------------------------------------------------

# Load data & tidyverse package
library(tidyverse)
load(file = "data.RData")

# Divide data into train & test set
train_data <- data[1:7000,]
test_data <- data[-(1:7000), ]

# Create class variable y_logit

# Method 1
y_logit <- rep(0, dim(train_data)[1]) # 7000 rows
y_logit[train_data$y >= 0] <- 1

data_train <- train_data %>%
  mutate(y = y_logit) %>%
  mutate(y = factor(y))

# Method 2
data_test <- test_data %>%
  mutate(y = ifelse(y >= 0, 1,0)) %>%
  mutate(y = factor(y)) # Same results, shorter code! 
  
#Bagging classifier

# Load packages
library(vip)
library(ipred)
library(rpart)
library(caret)

bagging_model <- bagging(y ~ ., 
                         data = data_train, 
                         coob=TRUE)
print(bagging_model)

# Predict y values
bagging_prediction <- predict(bagging_model,
                              data_train[, !(colnames(data_train) %in% c("y"))])
bagging_prediction

# Bagging with cross-validation
bagging_model2 <- train(y ~ ., 
                        data=data_train,
                        method = "treebag",
                        trControl = trainControl(method = "cv", number = 10),
                        nbagg = 50)

vip::vip(bagging_model2, num_features = 10)

# Draw graph to determine optimum iteration of bagging
# Create 91 bagging models
for (i in 10:100) {
  bagging_model <- bagging(y ~ ., data=data_train, nbagg = i, coob=TRUE)
  result <- c(i, bagging_model$err)
  
  if (i == 10) {bagging_result <- result} 
  else (bagging_result <- rbind(bagging_result, result))
}

plot(bagging_result, type = 'l')

#Random Forest
library(randomForest)

# Random Forest using default conditions
rf_model <- randomForest(y ~ ., 
                         data=data_train)

rf_model <- randomForest(y ~ ., 
                         data=data_train, 
                         mtry = 3, # number of features to select
                         maxnode = 20, # final number of leaf nodes
                         nodesize=5, # minimum number of data included in leaf nodes
                         
                         classwt = c(0.45, 0.55), # priors of classes 
                         ntree=500) # number of trees 
rf_model
head(rf_model$err.rate)
plot(rf_model, main = "Random Forest Model Errors", legend = "topright" )

# Plot rf_model
layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(rf_model)
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("bottom", colnames(rf_model$err.rate),col=1:3,cex=0.8,fill=1:3)

rf_training_y_pred <- predict(rf_model, 
                              data_train[,!(colnames(data_train) %in% c("y"))], 
                              type="response")
rf_test_y_pred <- predict(rf_model,  
                          data_test[,!(colnames(data_test) %in% c("y"))], 
                          type="response")

rf_training_table <- table(data_train$y, rf_training_y_pred)
rf_training_table

rf_test_table <- table(data_test$y, rf_test_y_pred)
rf_test_table

# Grid Search for Random Forest 

# Load package
library(e1071)

rf_grid_model <- tune(randomForest, 
                      data_train[,!(colnames(data_train) %in% c("y"))], # Features
                      as.factor(data_train$y), # Target variable
                      ranges = list(mtry = c(3,4), 
                                    maxnodes = c(20,23), 
                                    ntree = c(400,550), 
                                    nodesize=c(1,2)),
                      tunecontrol = tune.control(cross=5)) # 5-fold cross validation

rf_grid_model

save(data_train, file = "data_train.RData")
save(data_test, file = "data_test.RData")



