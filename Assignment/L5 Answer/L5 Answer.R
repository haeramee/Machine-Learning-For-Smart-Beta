library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)

# Use data sets 'fund.anu.dist' and 'monthly.ret' obtained in Lecture 5
load("fund.anu.dist.L5.RData")
load("monthly.ret.L5.RData")

# 1. Convert 'monthly.ret' from monthly to annual data

# One problem is that the rollingCumReturn function is very slow due to the data size of 'monthly.ret'.

# 1.1. Reduce data size
# So I will use the data of stocks that have the most data (top 50)

# Check how many lines of data for each stock
df.1 <- monthly.ret %>% 
  group_by(gvkey) %>% 
  count() %>% 
  arrange(desc(n))

# Keep 50 of stocks with the most data
df.2 <- df.1 %>% head(50) %>% select(gvkey) 
data <- monthly.ret %>% filter(gvkey %in% df.2$gvkey)

# 1.2 Apply rollingCumReturn to the data

# Create rollingCumReturn
rollingCumReturn <- function(targetDate, targetGroup) {
  data %>%
    # Pick out rows with a difference of 0 to 366 days between datadate and date
    mutate(thisDiff = difftime(as.Date(datadate), targetDate, unit = "days")) %>%
    filter(thisDiff < 0, thisDiff > -366, gvkey == targetGroup) %>%
    # Calculate cumulative return
    summarise(annValue = prod(1+ret)-1)
}

# Apply rollingCumReturn to each row
# This took my computer approximately 23 minutes to run - try to increase the number of stocks if your computer allows it!
data$rollingCumReturn <- mapply(rollingCumReturn, data$datadate, data$gvkey)

# Save just in case
save(data, file = "./RData/data.annual.RData")











# 2. Create portfolio of 20 stocks 

# 2.1 Pick out stocks from fund.anu.dist that are also in 'data'
fund.anu.dist.new <- fund.anu.dist %>% filter(gvkey %in% df.2$gvkey)

# 2.2 Calculate rank using 2021 data, giving cfo_ev.zscore, PE.A.INV.zscore, PB.A.INV.zscore equal weights. I decided to rank stocks using data recorded in 2021
fund.anu.dist.new <- fund.anu.dist.new %>% 
  filter(fyear == 2021) %>% 
  mutate(equal.weights.2021 = (cfo_ev.zscore + PE.A.INV.zscore + PB.A.INV.zscore)/3)

fund.anu.dist.new <- fund.anu.dist.new %>% 
  mutate(rank = rank(equal.weights.2021))

# Select top 20 highest ranking stocks
top.20 <- fund.anu.dist.new %>% 
  slice_min(rank, n = 20) 

# These are the gvkeys of the stocks that go into my portfolio 
top.20$gvkey










# 3. Give equal weights to each stock and compare performance
 
# 3.1 Filter gvkeys we are interested in 
df.3 <- data %>% filter(gvkey %in% top.20$gvkey)

# 3.2 Use quantmod to call in data for each ticker
library(quantmod)
tickers=unique(df.3$tic)
getSymbols(tickers, from="2015-01-01", to="2021-12-31", periodicity = "daily")

tickers = c("BALL", "D", "FLS", "ECL", "ESCA", "GTY", "AMOT", "HEI", "GFF", "IDCC", "LZB", "LOW", "MCS", "NAVI", "JSM","PEAK", "SITC", "EQR", "SVC", "IDT")

# Make adjusted price data frame
get.AdPrices <- function(x) {Ad(get(x))}
AdClosePrices <- do.call(merge, lapply(tickers, get.AdPrices))
head(AdClosePrices)

# Make return data frame
get.AdReturns <- function(x) {dailyReturn(Ad(get(x)))}
AdCloseReturns <- do.call(merge, lapply(tickers, get.AdReturns))
colnames(AdCloseReturns) <- tickers
head(AdCloseReturns)

# 3.3 Give equal weight to each stock 
AdCloseReturns.new <- cbind(AdCloseReturns,rowMeans(AdCloseReturns))

# 3.4 Evaluate portfolio performance
table.AnnualizedReturns(AdCloseReturns.new, scale=252, Rf=0)
table.Drawdowns(AdCloseReturns.new$rowMeans.AdCloseReturns.)
table.Stats(AdCloseReturns.new$rowMeans.AdCloseReturns.)
charts.PerformanceSummary(AdCloseReturns.new$rowMeans.AdCloseReturns.)







