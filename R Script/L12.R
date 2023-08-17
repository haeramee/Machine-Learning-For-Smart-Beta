# --------------------------------------------------------------
# L12 AdaBoost
# --------------------------------------------------------------

# Load packages
library(tidyverse)
library(JOUSBoost) # Contains adaboost function

# Load data
load(file = "data_train.RData") # 7000 observations
load(file = "data_test.RData") # 3000 observations

# Data preprocessing
# Save x as matrix
x <- data_train %>% 
  select(-y) %>% 
  as.matrix()

# Code 0 to -1, 1 to 1
y <- (data_train$y == "1") * 2 -1

# Adaboost classifier
adaboost_model <- adaboost(x, y, 
                           tree_depth = 2, # size of classification tree 
                           n_rounds = 200, # number of rounds of boosting to use
                           verbose = TRUE) # print number of iterations to console

head(adaboost_model$alphas, 20)
adaboost_model$confusion_matrix
  
