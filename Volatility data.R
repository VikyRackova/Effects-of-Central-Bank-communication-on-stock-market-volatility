setwd()
source("Functions.R")
library(readxl)
library(quantmod)
library(dplyr)

########################################################## GATHER VOLATILITY DATA FROM YAHOO FINANCE (OR OTHER SOURCES) ########################################################## 
# Define a start date
start_date <- as.Date("1994-01-01")
### US indices
### Volatility index - VIX
getSymbols("^VIX", src = "yahoo", from = start_date)
VIX <- data.frame(Date = index(VIX), coredata(VIX)[, c("VIX.Open", "VIX.Close")])%>% # keep only Open and Close
  mutate(Date = sapply(Date, standardize_date))
colnames(VIX)[-1] <- c("VIX.Open", "VIX.Close")  # Rename columns to identify VIX data
### Market index - S&P 500
getSymbols("^GSPC", src = "yahoo", from = start_date)
SP500 <- data.frame(Date = index(GSPC), coredata(GSPC)[, c("GSPC.High", "GSPC.Low")])%>%
  mutate(Date = sapply(Date, standardize_date))
colnames(SP500)[-1] <- c("SP500.High", "SP500.Low")  # Rename columns to identify S&P 500 data

### EU indices
### Volatility index - VSTOXX (not available at Yahoo finance, obtained from Factset)
VSTOXX <-read.csv("C:/one drive/Počítač/UNI/Bachelors thesis/Data sets/STOXX 50 Volatility VSTOXX EUR Historical Data (1).csv")
### Market index - STOXX
getSymbols("^STOXX", src = "yahoo", from = start_date)
STOXX <- data.frame(Date = index(STOXX), coredata(STOXX)[, c("STOXX.High", "STOXX.Low")])%>%
  mutate(Date = sapply(Date, standardize_date))
colnames(STOXX)[-1] <- c("STOXX.High", "STOXX.Low")  # Rename columns to identify VIX data

########################################################## Calculate percentage change in volatility and store its lags ########################################################## 
STOXX <- STOXX %>%
  mutate(Percent_change.STOXX = 100*(log(STOXX.High)-log(STOXX.Low)))%>%
  dplyr::select(Date,Percent_change.STOXX)%>%
  rename(STOXX = Percent_change.STOXX) %>%
  tidyr::drop_na() %>%
  create_lagged_columns(
    column_name = "STOXX", # Replace with your actual column name
    max_lag = 10
  )

VSTOXX <- VSTOXX %>%
  rename(VSTOXX.Price = High, VSTOXX.Low = Low)%>%
  mutate(Percent_change.VSTOXX = 100*(log(VSTOXX.Price)-log(lag(VSTOXX.Price))))%>%
  dplyr::select(Date,Percent_change.VSTOXX)%>%
  rename(VSTOXX = Percent_change.VSTOXX) %>%
  tidyr::drop_na() %>%
  create_lagged_columns(
    column_name = "VSTOXX", # Replace with your actual column name
    max_lag = 10
  )

SP500 <- SP500 %>%
  mutate(Percent_change.SP500 = 100*(log(SP500.High)-log(SP500.Low)))%>%
  dplyr::select(Date, Percent_change.SP500)%>%
  rename(SP500 = Percent_change.SP500) %>%
  tidyr::drop_na() %>%
  create_lagged_columns(
    column_name = "SP500", # Replace with your actual column name
    max_lag = 10
  )

VIX <- VIX %>%
  mutate(Percent_change.VIX = 100*(log(VIX.Close)-log(VIX.Open)))%>%
  dplyr::select(Date, Percent_change.VIX)%>%
  rename(VIX = Percent_change.VIX) %>%
  tidyr::drop_na() %>%
  create_lagged_columns(
    column_name = "VIX", # Replace with your actual column name
    max_lag = 10
  )

# Save the results
write.csv(STOXX, file = "STOXX.csv",row.names = FALSE)
write.csv(VSTOXX, file = "VSTOXX.csv",row.names = FALSE)
write.csv(SP500, file = "SP500.csv",row.names = FALSE)
write.csv(VIX, file = "VIX.csv",row.names = FALSE)





