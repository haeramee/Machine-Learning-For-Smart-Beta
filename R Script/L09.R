# --------------------------------------------------------------
# L9 Classification Tree
# --------------------------------------------------------------

# Load packages
library(tidyverse)
library(rpart)
library(rpart.plot)

# Load data
load("data.RData")

# Divide into train and test data
train_data <- data[1:7000,]
test_data <- data[-(1:7000), ]

# Create y_logit
# Change y to binary data 
y_logit <- rep(0, dim(train_data)[1])
y_logit[train_data$y >= 0] <- 1
y_logit <- as.factor(y_logit)

# Create x_logit
x_logit <- train_data %>%
  select(-y) %>% as.matrix()

# Classification Tree
tree_model <- rpart(y_logit ~ x_logit, 
parms = list(split = 'information'),
                    minbucket=200, 
                    minsplit = 3000,
                    cp = 0.0001, 
                    maxdepth = 10,
                    method = "class")

print(tree_model)
rpart.plot(tree_model)

# Check performance
tree_train_y_pred <- predict(tree_model, 
                             as.data.frame(x_logit), 
                             type='class')

# Prediction of y value for each obs
print(tree_train_y_pred)

# % of correctly classified obs 
sum(tree_train_y_pred == y_logit)/length(y_logit) # 0.5602857

# Create y_test_logit
# Change y to binary data 
y_test_logit <- rep(0, dim(test_data)[1])
y_test_logit[test_data$y >= 0] <- 1
y_test_logit <- as.factor(y_test_logit)

# Create x_logit
x_logit <- test_data %>%
  select(-y) %>% as.matrix()

# Create predictions using tree_model 
tree_test_y_pred <- predict(tree_model,
                            as.data.frame(x_logit), 
                            type="class")

# Confusion matrix
training_table <- table(y_logit, tree_train_y_pred)
training_table

test_table <- table(y_test_logit, tree_test_y_pred)
test_table

