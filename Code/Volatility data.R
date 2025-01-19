setwd()
source("Functions.R")
library(readxl)
library(quantmod)
library(dplyr)

# Load data for the volatility index in EU
VSTOXX <-read.csv("C:/one drive/Počítač/UNI/Bachelors thesis/Data sets/VSTOXX EUR Historical Data.csv")

# Define the start date
start_date <- as.Date("1994-01-01")
getSymbols("^STOXX", src = "yahoo", from = start_date)
STOXX <- data.frame(Date = index(STOXX), coredata(STOXX)[, c("STOXX.High", "STOXX.Low", "STOXX.Open", "STOXX.Close")])%>%
  mutate(Date = sapply(Date, standardize_date))
colnames(STOXX)[-1] <- c("STOXX.High", "STOXX.Low", "STOXX.Open", "STOXX.Close")  # Rename columns to identify VIX data

# Download VIX data from Yahoo Finance and keep only Open and Close columns
getSymbols("^VIX", src = "yahoo", from = start_date)
VIX <- data.frame(Date = index(VIX), coredata(VIX)[, c("VIX.Open", "VIX.Close")])%>%
  mutate(Date = sapply(Date, standardize_date))
colnames(VIX)[-1] <- c("VIX.Open", "VIX.Close")  # Rename columns to identify VIX data

# Download S&P 500 data from Yahoo Finance and keep only Open and Close columns
getSymbols("^GSPC", src = "yahoo", from = start_date)
SP500 <- data.frame(Date = index(GSPC), coredata(GSPC)[, c("GSPC.High", "GSPC.Low","GSPC.Open", "GSPC.Close")])%>%
  mutate(Date = sapply(Date, standardize_date))
colnames(SP500)[-1] <- c("SP500.High", "SP500.Low","SP500.Open", "SP500.Close")  # Rename columns to identify S&P 500 data


############################################################## volatility with lags
STOXX_1 <- STOXX %>%
  mutate(Percent_change.STOXX = 100*(log(STOXX.High)-log(STOXX.Low)))%>%
  dplyr::select(Date,Percent_change.STOXX)%>%
  rename(STOXX = Percent_change.STOXX) %>%
  tidyr::drop_na() %>%
  create_lagged_columns(
    column_name = "STOXX", # Replace with your actual column name
    max_lag = 10
  )

VSTOXX_1 <- VSTOXX %>%
  mutate(Percent_change.VSTOXX = 100*(log(VSTOXX.Price)-log(lag(VSTOXX.Price))))%>%
  dplyr::select(Date,Percent_change.VSTOXX)%>%
  rename(VSTOXX = Percent_change.VSTOXX) %>%
  tidyr::drop_na() %>%
  create_lagged_columns(
    column_name = "VSTOXX", # Replace with your actual column name
    max_lag = 10
  )

SP500_1 <- SP500 %>%
  mutate(Percent_change.SP500 = 100*(log(SP500.High)-log(SP500.Low)))%>%
  dplyr::select(Date, Percent_change.SP500)%>%
  rename(SP500 = Percent_change.SP500) %>%
  tidyr::drop_na() %>%
  create_lagged_columns(
    column_name = "SP500", # Replace with your actual column name
    max_lag = 10
  )

VIX_1 <- VIX %>%
  mutate(Percent_change.VIX = 100*(log(VIX.Close)-log(VIX.Open)))%>%
  dplyr::select(Date, Percent_change.VIX)%>%
  rename(VIX = Percent_change.VIX) %>%
  tidyr::drop_na() %>%
  create_lagged_columns(
    column_name = "VIX", # Replace with your actual column name
    max_lag = 10
  )

#####################################SAVE THE WHOLE VOLATILITY DATA #####################################
write.csv(STOXX_1, file = "STOXX.csv",row.names = FALSE)
write.csv(VSTOXX_1, file = "VSTOXX.csv",row.names = FALSE)
write.csv(SP500_1, file = "SP500.csv",row.names = FALSE)
write.csv(VIX_1, file = "VIX.csv",row.names = FALSE)








##########################################################
#  Attempting GARCH
##########################################################
VSTOXX <-read.csv("C:/one drive/Počítač/UNI/Bachelors thesis/Data sets/VSTOXX EUR Historical Data.csv")

# Define the start date
start_date <- as.Date("1994-01-01")
getSymbols("^STOXX", src = "yahoo", from = start_date)
STOXX <- data.frame(Date = index(STOXX), coredata(STOXX)[, c("STOXX.Open", "STOXX.Close")])%>%
  mutate(Date = sapply(Date, standardize_date))
colnames(STOXX)[-1] <- c("STOXX.Open", "STOXX.Close")  # Rename columns to identify VIX data

# Download VIX data from Yahoo Finance and keep only Open and Close columns
getSymbols("^VIX", src = "yahoo", from = start_date)
VIX <- data.frame(Date = index(VIX), coredata(VIX)[, c("VIX.Open", "VIX.Close")])%>%
  mutate(Date = sapply(Date, standardize_date))
colnames(VIX)[-1] <- c("VIX.Open", "VIX.Close")  # Rename columns to identify VIX data

# Download S&P 500 data from Yahoo Finance and keep only Open and Close columns
getSymbols("^GSPC", src = "yahoo", from = start_date)
SP500 <- data.frame(Date = index(GSPC), coredata(GSPC)[, c("GSPC.Open", "GSPC.Close")])%>%
  mutate(Date = sapply(Date, standardize_date))
colnames(SP500)[-1] <- c("SP500.Open", "SP500.Close")  # Rename columns to identify S&P 500 data

##############################
# Get daily returns
STOXX_GARCH <- STOXX %>%
  mutate(Percent_change.STOXX = 100*(log(STOXX.Close)-log(STOXX.Open)))%>%
  dplyr::select(Date,Percent_change.STOXX)%>%
  rename(STOXX = Percent_change.STOXX) %>% 
  filter(is.finite(STOXX))%>%
  tidyr::drop_na()
  
VSTOXX_GARCH <- VSTOXX %>%
  mutate(Percent_change.VSTOXX = 100*(log(VSTOXX.Price)-log(lag(VSTOXX.Price))))%>%
  dplyr::select(Date,Percent_change.VSTOXX)%>%
  rename(VSTOXX = Percent_change.VSTOXX) %>%
  filter(is.finite(VSTOXX))%>%
  tidyr::drop_na()
  
SP500_GARCH <- SP500 %>%
  mutate(Percent_change.SP500 = 100*(log(SP500.Close)-log(SP500.Open)))%>%
  dplyr::select(Date, Percent_change.SP500)%>%
  rename(SP500 = Percent_change.SP500) %>%
  filter(is.finite(SP500))%>%
  tidyr::drop_na() 
  
VIX_GARCH <- VIX %>%
  mutate(Percent_change.VIX = 100*(log(VIX.Close)-log(VIX.Open)))%>%
  dplyr::select(Date, Percent_change.VIX)%>%
  rename(VIX = Percent_change.VIX) %>%
  filter(is.finite(VIX))%>%
  tidyr::drop_na() 
  
library(fGarch)
garch_model_STOXX <- garchFit(~ garch(1, 1), data = STOXX_GARCH$STOXX, trace = FALSE)
STOXX_GARCH$GARCH_Volatility <- garch_model_STOXX@sigma.t
STOXX_GARCH<- STOXX_GARCH%>%
  dplyr::select(Date,GARCH_Volatility)%>%
  rename(STOXX = GARCH_Volatility)%>%
  create_lagged_columns(
    column_name = "STOXX", # Replace with your actual column name
    max_lag = 10
  )
garch_model_VSTOXX <- garchFit(~ garch(1, 1), data = VSTOXX_GARCH$VSTOXX, trace = FALSE)
VSTOXX_GARCH$GARCH_Volatility <- garch_model_VSTOXX@sigma.t
VSTOXX_GARCH<- VSTOXX_GARCH%>%
  dplyr::select(Date,GARCH_Volatility)%>%
  rename(VSTOXX = GARCH_Volatility)%>%
  create_lagged_columns(
    column_name = "VSTOXX", # Replace with your actual column name
    max_lag = 10
  )
garch_model_SP500 <- garchFit(~ garch(1, 1), data = SP500_GARCH$SP500, trace = FALSE)
SP500_GARCH$GARCH_Volatility <- garch_model_SP500@sigma.t
SP500_GARCH<- SP500_GARCH%>%
  dplyr::select(Date,GARCH_Volatility)%>%
  rename(SP500 = GARCH_Volatility)%>%
  create_lagged_columns(
    column_name = "SP500", # Replace with your actual column name
    max_lag = 10
  )
garch_model_VIX <- garchFit(~ garch(1, 1), data = VIX_GARCH$VIX, trace = FALSE)
VIX_GARCH$GARCH_Volatility <- garch_model_VIX@sigma.t
VIX_GARCH<- VIX_GARCH%>%
  dplyr::select(Date,GARCH_Volatility)%>%
  rename(VIX = GARCH_Volatility)%>%
  create_lagged_columns(
    column_name = "VIX", # Replace with your actual column name
    max_lag = 10
  )
  
write.csv(STOXX_GARCH, file = "STOXX_GARCH.csv",row.names = FALSE)
write.csv(VSTOXX_GARCH, file = "VSTOXX_GARCH.csv",row.names = FALSE)
write.csv(SP500_GARCH, file = "SP500_GARCH.csv",row.names = FALSE)
write.csv(VIX_GARCH, file = "VIX_GARCH.csv",row.names = FALSE)

################################################ cOMPARISON OF VOLATILITY ESTIMATION


volatility_STOXX<-read.csv("STOXX.csv")
volatility_STOXX$Date<-as.Date(volatility_STOXX$Date,format = "%Y-%m-%d")
volatility_VSTOXX<-read.csv("VSTOXX.csv")
volatility_VSTOXX$Date<-as.Date(volatility_VSTOXX$Date,format = "%m/%d/%Y")
volatility_SP500<-read.csv("SP500.csv")
volatility_SP500$Date<-as.Date(volatility_SP500$Date,format = "%Y-%m-%d")
volatility_VIX<-read.csv("VIX.csv")
volatility_VIX$Date<-as.Date(volatility_VIX$Date,format = "%Y-%m-%d")

volatility_STOXX_GARCH<-read.csv("STOXX_GARCH.csv")
volatility_STOXX_GARCH$Date<-as.Date(volatility_STOXX_GARCH$Date,format = "%Y-%m-%d")
volatility_VSTOXX_GARCH<-read.csv("VSTOXX_GARCH.csv")
volatility_VSTOXX_GARCH$Date<-as.Date(volatility_VSTOXX_GARCH$Date,format = "%m/%d/%Y")
volatility_SP500_GARCH<-read.csv("SP500_GARCH.csv")
volatility_SP500_GARCH$Date<-as.Date(volatility_SP500_GARCH$Date,format = "%Y-%m-%d")
volatility_VIX_GARCH<-read.csv("VIX_GARCH.csv")
volatility_VIX_GARCH$Date<-as.Date(volatility_VIX_GARCH$Date,format = "%Y-%m-%d")

library(ggplot2)
library(dplyr)

# Merge datasets by Date
stoxx_data <- merge(volatility_STOXX, volatility_STOXX_GARCH, by = "Date", suffixes = c("_original", "_GARCH"))
vstoxx_data <- merge(volatility_VSTOXX, volatility_VSTOXX_GARCH, by = "Date", suffixes = c("_original", "_GARCH"))
sp500_data <- merge(volatility_SP500, volatility_SP500_GARCH, by = "Date", suffixes = c("_original", "_GARCH"))
vix_data <- merge(volatility_VIX, volatility_VIX_GARCH, by = "Date", suffixes = c("_original", "_GARCH"))

# STOXX Plot
ggplot(stoxx_data, aes(x = Date)) +
  geom_line(aes(y = STOXX_original, color = "High-Low")) +
  geom_line(aes(y = STOXX_GARCH, color = "GARCH")) +
  labs( y = "Volatility (%)", color = "Legend") +
  scale_color_manual(values=c("red","#858585"))+
  scale_x_date(
    date_breaks = "5 year",         # Increase frequency to every 6 months
    date_labels = "%Y"            # Format date labels as "Month Year"
  ) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=16, family = "Times", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,size=12, family = "Times", face = "italic"),
        axis.title = element_text(size = 28, family = "Times"),                                   # Change axis label font
        axis.text = element_text(size = 24, family = "Times"),                                    # Change axis text font
        legend.text = element_text(size = 28, family = "Times"),
        legend.position = "bottom",
        legend.title = element_blank(),        # Remove legend title
        panel.grid = element_blank(),          # Remove all gridlines
        axis.line = element_line(color = "black"),
        panel.background = element_blank(),    # Remove panel background
        plot.background = element_blank()      # Remove plot background
  )

 library(patchwork)

# Create individual plots
vstoxx_plot <- ggplot(vstoxx_data, aes(x = Date)) +
  geom_line(aes(y = VSTOXX_original, color = "Original")) +
  geom_line(aes(y = VSTOXX_GARCH, color = "GARCH")) +
  labs(title = "VSTOXX: Original vs. GARCH", y = "Volatility (%)", color = "Legend") +
  scale_color_manual(values=c("red","#858585"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=16, family = "Times", face = "bold"),
      plot.subtitle = element_text(hjust = 0.5,size=12, family = "Times", face = "italic"),
      axis.title = element_text(size = 28, family = "Times"),                                   # Change axis label font
      axis.text = element_text(size = 24, family = "Times"),                                    # Change axis text font
      legend.text = element_text(size = 28, family = "Times"),     
      legend.title = element_blank(),        # Remove legend title
      panel.grid = element_blank(),          # Remove all gridlines
      axis.line = element_line(color = "black"),
      panel.background = element_blank(),    # Remove panel background
      plot.background = element_blank()      # Remove plot background
)

ggplot(sp500_data, aes(x = Date)) +
  geom_line(aes(y = SP500_original, color = "High-Low")) +
  geom_line(aes(y = SP500_GARCH, color = "GARCH")) +
  labs(y = "Volatility (%)", color = "Legend") +
  scale_color_manual(values=c("red","#858585"))+
  scale_x_date(
    date_breaks = "5 year",         # Increase frequency to every 6 months
    date_labels = "%Y"            # Format date labels as "Month Year"
  ) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=16, family = "Times", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,size=12, family = "Times", face = "italic"),
        axis.title = element_text(size = 28, family = "Times"),                                   # Change axis label font
        axis.text = element_text(size = 24, family = "Times"),                                    # Change axis text font
        legend.text = element_text(size = 28, family = "Times"),  
        legend.position = "bottom",
        legend.title = element_blank(),        # Remove legend title
        panel.grid = element_blank(),          # Remove all gridlines
        axis.line = element_line(color = "black"),
        panel.background = element_blank(),    # Remove panel background
        plot.background = element_blank()      # Remove plot background
  )

vix_plot <- ggplot(vix_data, aes(x = Date)) +
  geom_line(aes(y = VIX_original, color = "High-Low")) +
  geom_line(aes(y = VIX_GARCH, color = "GARCH")) +
  labs(title = "VIX: Original vs. GARCH", y = "Volatility", color = "Legend") +
  scale_color_manual(values=c("red","#858585"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,size=16, family = "Times", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,size=12, family = "Times", face = "italic"),
        axis.title = element_text(size = 20, family = "Times"),                                   # Change axis label font
        axis.text = element_text(size = 16, family = "Times"),                                    # Change axis text font
        legend.text = element_text(size = 20, family = "Times"),     
        legend.title = element_blank(),        # Remove legend title
        panel.grid = element_blank(),          # Remove all gridlines
        axis.line = element_line(color = "black"),
        panel.background = element_blank(),    # Remove panel background
        plot.background = element_blank()      # Remove plot background
  )
# Combine all plots
stoxx_plot / sp500_plot 

# Correlation
cor.test(volatility_SP500$SP500, volatility_SP500_GARCH$SP500)
cor.test(volatility_STOXX$STOXX, volatility_STOXX_GARCH$STOXX)




#SUmmary statistics
library(dplyr)

# Create comparison data frames
STOXX_comparison <- data.frame(
  High_Low = volatility_STOXX$STOXX,
  GARCH = volatility_STOXX_GARCH$STOXX
)

SP500_comparison <- data.frame(
  High_Low = volatility_SP500$SP500,
  GARCH = volatility_SP500_GARCH$SP500
)

# Function to calculate summary statistics
calculate_summary <- function(volatility) {
     summarystatistics<- c( Mean = mean(volatility, na.rm = TRUE),
      Standard_deviation = sd(volatility, na.rm = TRUE),
      Minimum = min(volatility, na.rm = TRUE),
      Maximum = max(volatility, na.rm = TRUE)
     )
     return(summarystatistics)
}

stoxx_highlow<- as.data.frame(calculate_summary(STOXX_comparison$High_Low))
stoxx_garch<- as.data.frame(calculate_summary(STOXX_comparison$GARCH))
stoxx_summary <- stoxx_highlow%>%
  cbind(stoxx_garch)
colnames(stoxx_summary)<-c("STOXX High and Low method", "STOXX GARCH method")
sp500_highlow<- as.data.frame(calculate_summary(SP500_comparison$High_Low))
sp500_garch<- as.data.frame(calculate_summary(SP500_comparison$GARCH))
sp500_summary <- sp500_highlow%>%
  cbind(sp500_garch)
colnames(sp500_summary)<-c("S&P500 High and Low method", "S&P500 GARCH method")

# Combine summary statistics for SP500 and STOXX
summary_statistics <- cbind(sp500_summary, stoxx_summary)

# Use stargazer to format the table
stargazer(summary_statistics,
          type = "latex",
          summary = FALSE,  # Do not recalculate stats
          rownames = TRUE,  # Keep row names (Mean, Standard Deviation, etc.)
          title = "Summary Statistics of Volatility Estimation Methods",
          label = "tab:volatility_summary",
          column.sep.width = "1pt")  # Adjust spacing if needed











