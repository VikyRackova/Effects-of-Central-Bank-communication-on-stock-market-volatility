setwd()
########################################################################################################## Uncertainty data - from Survey of Professional Forecasters ########################################################################################################## 
#### Required packages
library(openxlsx)
library(dplyr)
library(tidyr)
library(lubridate)


########################################################################################################## Load downloaded data from Survey of Professional Forecasters and Hawk-score ##########################################################################################################
inflation_uncertainty_US <-read.xlsx("Dispersion_CPI_US.xlsx")
colnames(inflation_uncertainty_US) <- c("Quarter", "P25", "P75", "Uncertainty")

inflation_uncertainty_EU <- read.csv("Variance of inflation forecasts EU.csv")
colnames(inflation_uncertainty_EU) <- c("Date","Time","Variance")
inflation_uncertainty_EU$Date<-as.Date(inflation_uncertainty_EU$Date,format = "%m/%d/%Y")

GDP_uncertainty_US<-read.xlsx("Dispersion real GDP growth US.xlsx")
colnames(GDP_uncertainty_US) <- c("Quarter", "P25", "P75", "Uncertainty")

GDP_uncertainty_EU<- read.csv("Variance of real GDP growth forecasts EU.csv")
colnames(GDP_uncertainty_EU) <- c("Date","Time","Variance")
GDP_uncertainty_EU$Date<-as.Date(GDP_uncertainty_EU$Date,format = "%m/%d/%Y")

Final_score_ECBD<-read.csv("Final_score_ECBD.csv")
Final_score_ECBD$Date <- as.Date(Final_score_ECBD$Date)
Final_score_ECBM<-read.csv("Final_score_ECBM.csv")
Final_score_ECBM$Date <- as.Date(Final_score_ECBM$Date)
Final_score_FEDD<-read.csv("Final_score_FEDD.csv")
Final_score_FEDD$Date <- as.Date(Final_score_FEDD$Date)
Final_score_FEDM<-read.csv("Final_score_FEDM.csv")
Final_score_FEDM$Date <- as.Date(Final_score_FEDM$Date)

########################################################################################################## Estimate uncertainty as variance/dispersion of forecasts ##########################################################################################################
CPI_US <- inflation_uncertainty_US %>%
  filter(!is.na(P25) & !is.na(P75) & grepl("^[0-9.]+$", P25) & grepl("^[0-9.]+$", P75)) %>%
  mutate(
    year = as.numeric(substr(Quarter, 1, 4)),
    qtr = as.numeric(substr(Quarter, 6, 6)),
    quarter_start = ymd(paste0(year, "-", (qtr - 1) * 3 + 1, "-01"))
  )%>%
  dplyr::select(quarter_start,Uncertainty)

CPI_EU <- inflation_uncertainty_EU %>%
  mutate(quarter_start = floor_date(as.Date(Date), "quarter")) %>%  # Convert to quarter-start dates
  mutate(Uncertainty = 100* Variance)%>%
  dplyr::select(quarter_start,Uncertainty)

GDP_US <-GDP_uncertainty_US%>%
  mutate(
    P25 = as.numeric(P25),
    P75 = as.numeric(P75),
    Uncertainty = ((P75) - (P25))) %>%
  tidyr::drop_na()%>%
  mutate(
    year = as.numeric(substr(Quarter, 1, 4)),
    qtr = as.numeric(substr(Quarter, 6, 6)),
    quarter_start = ymd(paste0(year, "-", (qtr - 1) * 3 + 1, "-01"))
  ) %>%
  dplyr::select(quarter_start,Uncertainty)

GDP_EU <- GDP_uncertainty_EU%>%
  mutate(quarter_start = floor_date(as.Date(Date), "quarter")) %>%  # Convert to quarter-start dates
  mutate(Uncertainty = 100 * Variance)%>%
  dplyr::select(quarter_start,Uncertainty)

########################################################################################################## Extrapolate missing data ##########################################################################################################
# Generate all possible quarters
all_quarters <- tibble(
  quarter_start = seq(
    from = ymd("1994-01-01"),  # Start from Q1 of 1994
    to = ymd("2024-10-01"),    # End at Q4 of 2024
    by = "quarter"
  )
)

# Identify missing quarters and fill
CPI_US_filled <- all_quarters %>%
  left_join(CPI_US, by = "quarter_start") %>% # Merge with the original data
  arrange(quarter_start) %>%                # Ensure quarters are in order
  fill(Uncertainty, .direction = "down")    # Fill missing values with the last observation

# Identify missing quarters and fill
GDP_US_filled <- all_quarters %>%
  left_join(GDP_US, by = "quarter_start") %>% # Merge with the original data
  arrange(quarter_start) %>%                # Ensure quarters are in order
  fill(Uncertainty, .direction = "down")    # Fill missing values with the last observation

# Identify missing quarters and fill
CPI_EU_filled <- all_quarters %>%
  left_join(CPI_EU, by = "quarter_start") %>% # Merge with the original data
  arrange(quarter_start) %>%                # Ensure quarters are in order
  fill(Uncertainty, .direction = "down")    # Fill missing values with the last observation

# Identify missing quarters and fill
GDP_EU_filled <- all_quarters %>%
  left_join(GDP_EU, by = "quarter_start") %>% # Merge with the original data
  arrange(quarter_start) %>%                # Ensure quarters are in order
  fill(Uncertainty, .direction = "down")    # Fill missing values with the last observation

############################################### Filter the uncertainty based on the closest quarter and merge with dates of reported Hawk-score ###############################################
### FEDD
uncertainty_FEDD <- Final_score_FEDD %>%
  mutate(closest_quarter = floor_date(Date, "quarter"))

CPI_FEDD <- CPI_US_filled %>%
  right_join(uncertainty_FEDD, by = c("quarter_start" = "closest_quarter"))%>%
  dplyr::select(Date, Uncertainty)

GDP_FEDD <- GDP_US_filled %>%
  right_join(uncertainty_FEDD, by = c("quarter_start" = "closest_quarter"))%>%
  dplyr::select(Date, Uncertainty)

### FEDM
uncertainty_FEDM <- Final_score_FEDM %>%
  mutate(closest_quarter = floor_date(Date, "quarter"))

CPI_FEDM <- CPI_US_filled %>%
  right_join(uncertainty_FEDM, by = c("quarter_start" = "closest_quarter"))%>%
  dplyr::select(Date, Uncertainty)

GDP_FEDM <- GDP_US_filled %>%
  right_join(uncertainty_FEDM, by = c("quarter_start" = "closest_quarter"))%>%
  dplyr::select(Date, Uncertainty)

### ECBD
uncertainty_ECBD <- Final_score_ECBD %>%
  mutate(closest_quarter = floor_date(Date, "quarter"))

# Ensure Date column in CPI_EU is of type Date
CPI_ECBD <- CPI_EU_filled %>%
  right_join(uncertainty_ECBD, by = c("quarter_start" = "closest_quarter"))%>%
  dplyr::select(Date, Uncertainty)
  
GDP_ECBD <- GDP_EU_filled %>%
  right_join(uncertainty_ECBD, by = c("quarter_start" = "closest_quarter"))%>%
  dplyr::select(Date, Uncertainty)
  
### ECBM
uncertainty_ECBM <- Final_score_ECBM %>%
  mutate(closest_quarter = floor_date(Date, "quarter"))

CPI_ECBM <- CPI_EU_filled %>%
  right_join(uncertainty_ECBM, by = c("quarter_start" = "closest_quarter"))%>%
  dplyr::select(Date, Uncertainty)
  
GDP_ECBM <- GDP_EU_filled %>%
  right_join(uncertainty_ECBM, by = c("quarter_start" = "closest_quarter"))%>%
  dplyr::select(Date, Uncertainty)
  

### Save the results
write.csv(CPI_ECBD, file = "CPI_ECBD.csv",row.names = FALSE)
write.csv(CPI_ECBM, file = "CPI_ECBM.csv",row.names = FALSE)
write.csv(GDP_ECBD, file = "GDP_ECBD.csv",row.names = FALSE)
write.csv(GDP_ECBM, file = "GDP_ECBM.csv",row.names = FALSE)
write.csv(CPI_FEDD, file = "CPI_FEDD.csv",row.names = FALSE)
write.csv(CPI_FEDM, file = "CPI_FEDM.csv",row.names = FALSE)
write.csv(CPI_FEDM, file = "GDP_FEDD.csv",row.names = FALSE)
write.csv(GDP_FEDM, file = "GDP_FEDM.csv",row.names = FALSE)

