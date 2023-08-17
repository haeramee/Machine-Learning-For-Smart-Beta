# --------------------------------------------------------------
# L14 Portfolio Blending, Signal Blending
# --------------------------------------------------------------

# Load package
library(tidyverse)

# Load data
load("data_ml.RData")

# Create returns and smart_beta
returns <- data_ml[,c("stock_id","date","R12M_Usd")]
smart_beta <- c("Eps","Mkt_Cap_12M_Usd","Mom_11M_Usd","Pe","Vol1Y_Usd","Div_Yld")

# Specify start_date
start_date <- as.Date("2007-01-01")

# Store unique values of stock_id to ticks
ticks <- data_ml$stock_id %>%
  as.factor() %>%
  levels()

N <- length(ticks) # 1207 stocks 

# Store unique values of date_id to dates_list
dates_list <- returns$date[returns$date > start_date] %>%
  unique() %>%
  as.Date(origin = "1970-01-01") 

Tt <- length(dates_list) # 147 months 

# Create matrix of 0s 
# First 6 columns - Forward 12 month returns of each smart beta
# Last 2 colmns - Returns from signal blending & portfolio blending
portf_returns <- matrix(0, nrow = Tt, ncol = 8)

model_data <- data_ml[as.Date(data_ml$date) == dates_list[1],  ] # date == "2007-01-31" 
returns_data <- returns[as.Date(returns$date) == dates_list[2],] # date == "2007-02-28"

# Apply feature_signal to each element in smart_beta
this_period_factor_result <- lapply(smart_beta, feature_signal)

# Select top 20 highest ranking stocks
get_top_rank <- function(data){data %>% filter(rank < 21) }
this_period_port <- lapply(this_period_factor_result, get_top_rank)

# Give each smart beta portfolio equal weight 
# Note: each dataframe in this_period_port may contain more than 20 stocks
get_equal_w_port <- function(data){sum(data$returns * 1/dim(data)[1])}
this_period_equal_w_port <- lapply(this_period_port,get_equal_w_port)

# Create dataframe
this_period_equal_w_port <- do.call(cbind.data.frame, this_period_equal_w_port) 
colnames(this_period_equal_w_port) <- smart_beta
this_period_equal_w_port$all <- rowSums(this_period_equal_w_port)/length(smart_beta)
this_period_equal_w_port$date <- dates_list[1]

# Signal blending - combine signal and then rank
rm(data)
for (k in 1:length(smart_beta)) {
  # Select dataframe of kth factor
  this.data <- data.frame(this_period_factor_result[k])
  
  # Add suffix “signal”
  colnames(this.data)[2:3] <- c(smart_beta[k], paste(smart_beta[k],"signal",sep="_"))
  
  # left_join by column ‘stock_id’
  if (k == 1) {data <- data.frame(this.data)
  } else {
    data <- left_join(data, this.data[, 1:3], by = 'stock_id')
  }

}

# Give equal weight to each signal
data$this_period_multi_signal <- rowSums(data[,  grepl('signal', colnames(data))])/length(smart_beta)

# Create rank column
data$rank <- rank(data$this_period_multi_signal)

# Average return of rows whose rank is in the top 20
this_period_equal_w_port$equal_w_signal_port <- mean(data$returns[data$rank < 21])
               
# Function definition ------------------------------------------

single_factor_one_period_signal <- function(model_data, returns_data, feature) {
  
  test_returns <- returns_data %>% 
    select("date", "stock_id", "R12M_Usd") 
  
  # Select stock_ids in test_returns
  model_features <- model_data %>% 
    select("stock_id",feature) %>% 
    filter(stock_id %in% test_returns$stock_id)
  
  # Standardize features
  score <- model_features %>% 
    select(-stock_id) %>% 
    scale() 
  # Create rank 
  # Smaller values of Mkt_Cap_12M_Usd, Pe, Vol1Y_Usd → Better portfolio performance
  # rank() gives the smallest value a rank of 1
  if (!(feature %in% c("Mkt_Cap_12M_Usd","Pe","Vol1Y_Usd"))) {score <- -1*score}
  model_features$signal <- score
  model_features$rank <- rank(score)
  
  model_features$returns <- test_returns$R12M_Usd[match(model_features$stock_id,test_returns$stock_id)]
  model_features$date <- test_returns$date[1]
  
  return(model_features)
}

feature_signal <- function(x){
  single_factor_one_period_signal(model_data, returns_data, x)
  }
