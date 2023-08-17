# --------------------------------------------------------------
# L13 Ensemble
# --------------------------------------------------------------

# Load data
load(file = "data_train.RData")
load(file = "data_test.RData")

#Classifier 1 - Logistic Regression
logistic_fit = glm(y ~.,
                   family = "binomial",
                   data = data_train)

# predict calculates probability an observation belongs to group 1
logistic_probs = predict(logistic_fit, 
                         newdata = data_train, 
                         type = "response")
contrasts(data_train$y)

#Classifier 2 - Classification Tree
library(rpart) 

tree_fit = rpart(y ~ ., 
                 data = data_train, 
                 minbucket = 200, 
                 minsplit = 3000, 
                 cp=0.0001, 
                 maxdepth = 10,
                 method = "class")

# First column of predict contains probability an observation belongs to class 1
tree_probs <- predict(tree_fit, 
                      as.data.frame(data_train[,!(colnames(data_train) %in% c("y"))]),
                      type = "prob")[,1] 

#Classifier 3 - Random Forest
library(randomForest)

rf_fit <- randomForest(y ~ ., 
                       data=data_train, 
                       mtry = 3, 
                       maxnode = 20, 
                       classwt = c(0.075, 0.925), 
                       ntree=500, 
                       nodsize=5)

# Contains probability an observation belongs to class 1
rf_probs <- predict(rf_fit, 
                    data_train[,!(colnames(data_train) %in% c("y"))], 
                    type="prob")[,1]


# Combine results of the three model
ensemble_data <- data.frame(logistic_probs, tree_probs, rf_probs, data_train$y)

# Meta classifier – Logistic Regression
meta_fit <- glm(data_train.y ~.,
                family = "binomial",
                data = ensemble_data)

meta_probs = predict(meta_fit, 
                     ensemble_data[,-4],
                     type = "response")

ensemble_data$pred_class = "0"
ensemble_data$pred_class[meta_probs>0.5]="1"

table(ensemble_data$pred_class,ensemble_data[,4])

