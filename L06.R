# --------------------------------------------------------------
# L6 Data Preprocessing
# --------------------------------------------------------------

# Load packages
library(tidyverse)
library(caret)
library(glmnet) 

# Load data
load("data_bs.RData")
glimpse(data_bs)
dim(data_bs) # 10,000 obs, 100 attributes 

# Select 17 features 
features <- c("Advt_12M_Usd","Asset_Turnover","Bb_Yld", "Bv", "Capex_Ps_Cf", "Cash_Per_Share" , "Cf_Sales", "Debtequity","Div_Yld","Eps" ,"Mkt_Cap_12M_Usd", "Mom_11M_Usd","Pb", "Pe", "Roe", "Share_Turn_12M" , "Vol1Y_Usd")

# Set R1M_Usd as target variable y
y <- data_bs$R1M_Usd

# Store attributes in matrix x 
x <- data_bs %>%
  select(all_of(features)) %>% as.matrix() 

# Normalize data using caret package 
# preProcess will find the mean and standard deviation of each feature
x_normalizing <- preProcess(x, method = c("center", "scale"))
# (column data – column mean) / (column std)
x_normalized <- predict(x_normalizing, x)

# Create single dataframe
data <- data.frame(y, x_normalized)

# Divide data into train and test sets
train_data <- data[1:7000,]
test_data <- data[-(1:7000), ]

# Regression model 
model_train <- lm(y ~ ., data = train_data)

# Calculate errors 
# 1. In-sample MSE(Mean Squared Error)
mean((predict(model_train,train_data[,-1])-train_data[,1])^2)

# 2. Out-of-sample MSE(Mean Squared Error)
mean((predict(model_train,test_data[,-1])-test_data[,1])^2)

save(data, file = "./RData/data.RData")
