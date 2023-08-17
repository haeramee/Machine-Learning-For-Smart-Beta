# --------------------------------------------------------------
# L5 Recreating the MSCI Value Index
# --------------------------------------------------------------

library(tidyverse)
library(lubridate)

# Load data 
load("return.data.dist.RData") # 47 variables
load("fund.anu.dist.RData")

# Create variables mktval, ret in return.data.dist
return.data.dist$mktval <- return.data.dist$cshom * return.data.dist$prccm
return.data.dist$ret <- return.data.dist$trt1m/100

# Exclude NAs
monthly.ret <- return.data.dist[!is.na(return.data.dist$mktval)|!is.na(return.data.dist$ret),]

# Delete duplicate rows
monthly.ret <- unique(monthly.ret)

# Select rows
monthly.ret <- monthly.ret[monthly.ret$exchg %in% c("11","12","14","15"),]

# fund.anu.dist holds information on the fundamentals of each stock
# Updated on quarterly basis

# Create variables MVE, EV in fund.anu.dist 
fund.anu.dist$MVE <- abs(fund.anu.dist$prcc_f * fund.anu.dist$csho)
fund.anu.dist$EV <- rowSums(fund.anu.dist[,c("pstk","mii","dltt","dlc","ch","MVE")])

# Select rows that have positive values of EV & oancf
fund.anu.dist <- fund.anu.dist[(fund.anu.dist$EV > 0)&(fund.anu.dist$oancf > 0),]

# Exclude rows that have NA values in MVE
fund.anu.dist <- fund.anu.dist[!is.na(fund.anu.dist$MVE) & (fund.anu.dist$MVE != 0),]

# Create 3 indices
fund.anu.dist$cfo_ev <- fund.anu.dist$oancf/fund.anu.dist$EV
fund.anu.dist$PE.A.INV <- fund.anu.dist$ni/fund.anu.dist$MVE
fund.anu.dist$PB.A.INV <- fund.anu.dist$seq/fund.anu.dist$MVE

# Standardize indices
# (variable – mean) / standard deviation
fund.anu.dist$cfo_ev.zscore <- (fund.anu.dist$cfo_ev-mean(fund.anu.dist$cfo_ev, na.rm = TRUE))/sd(fund.anu.dist$cfo_ev, na.rm = TRUE)
fund.anu.dist$PE.A.INV.zscore <- (fund.anu.dist$PE.A.INV-mean(fund.anu.dist$PE.A.INV, na.rm = TRUE))/sd(fund.anu.dist$PE.A.INV, na.rm = TRUE)
fund.anu.dist$PB.A.INV.zscore <- (fund.anu.dist$PB.A.INV-mean(fund.anu.dist$PB.A.INV, na.rm = TRUE))/sd(fund.anu.dist$PB.A.INV, na.rm = TRUE)

# Create rollingCumReturn
rollingCumReturn <- function(targetDate, targetGroup) {
  data %>%
    # Pick out rows with a difference of 0 to 366 days between datadate and date
    mutate(thisDiff = difftime(as.Date(datadate), targetDate, unit = "days")) %>%
    filter(thisDiff < 0, thisDiff > -366, gvkey == targetGroup) %>%
    # Calculate cumulative return
    summarise(annValue = prod(1+ret)-1)
}

# Save monthly.ret as data
data <- monthly.ret

# Apply rollingCumReturn to each row
data$rollingCumReturn <- mapply(rollingCumReturn, data$datadate, data$gvkey)
