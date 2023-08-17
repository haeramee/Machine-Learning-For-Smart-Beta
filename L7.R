# --------------------------------------------------------------
# L7 Cross Validation, Feature Selection
# --------------------------------------------------------------

# Load data
load(file = "data.RData")

# Define the model 
model <- lm(y ~ ., data = data)

# Model Specification
library(DAAG)
out <- CVlm(data=data, 
            model, m=5, 
            plotit = "Observed", 
            main = "Cross-Validation Observed vs. Predicted")

attr(out, 'ms')

# Feature Selection
library(leaps)
feature.selection <- regsubsets(y~., data=data, nbest = 5, method = "exhaustive")
summary(feature.selection)
plot(feature.selection, scale = "r2")
