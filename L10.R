# --------------------------------------------------------------
# L10 CART
# --------------------------------------------------------------

# Load packages 
library(tidyverse)
library(rpart)
library(rpart.plot)

# Load data
load(file = "data.RData")

x <- data %>%
  select(-y) 
features <- colnames(x)

# Defines the model
formula <- paste("y ~", paste(features, collapse = " + "))
formula <- as.formula(formula)

fit_tree <-  rpart(formula,  data =data,
                   minbucket = 350,  # Min nb of obs required in each terminal node (leaf)
                   minsplit = 800,   # Min nb of obs required to continue splitting
                   cp =     0.0001  ,  # Precision: smaller = more leaves
                   maxdepth =     3  # Maximum depth (i.e. tree levels)
                   )

rpart.plot(fit_tree)

predict(fit_tree,x[1:5,]) 

# Visualizing trend line 
data %>% ggplot() + 
  stat_smooth(aes(x = Cf_Sales, y, color = "Cf_Sales"), se = FALSE) + 
  stat_smooth(aes(x = Vol1Y_Usd, y, color = "Vol1Y_Usd"), se = FALSE) + 
  stat_smooth(aes(x = Mom_11M_Usd, y, color = "Mom_11M_Usd"), se = FALSE) + 
  xlab("Predictor") + coord_fixed(11) + labs(color = "Characteristic")
