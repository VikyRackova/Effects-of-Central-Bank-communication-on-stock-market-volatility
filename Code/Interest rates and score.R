############################## Test for the predictive power of score to the changes in interest rates ##############################
############################################
rm(list=ls())
setwd()
library(dplyr)
library(stringr)
library(tidyverse)
library(stringr)
library(readr)
library(tidytext)
library(lubridate)
library(MASS)
library(tidyr)
library(officer)
library(flextable)
library(broom)
library(stargazer)
library(sandwich)
library(lmtest)
library(car)
library("fixest") 
library(xtable)
sentiment_score_ECBD<-read.csv("sentiment_score_ECBD.csv")
sentiment_score_ECBD$Date <- as.Date(sentiment_score_ECBD$Date)
sentiment_score_ECBM<-read.csv("sentiment_score_ECBM.csv")
sentiment_score_ECBM$Date <- as.Date(sentiment_score_ECBM$Date)
sentiment_score_FEDD<-read.csv("sentiment_score_FEDD.csv")
sentiment_score_FEDD$Date <- as.Date(sentiment_score_FEDD$Date)
sentiment_score_FEDM<-read.csv("sentiment_score_FEDM.csv")
sentiment_score_FEDM$Date <- as.Date(sentiment_score_FEDM$Date)
# Testing the robustness of the score using the interest rate changes
Final_score_ECBD<-read.csv("Unstandardized_ECBD.csv")%>%
  rename(Unstandardized_score= final_score)%>%
  dplyr::select(Date,Unstandardized_score)
Final_score_ECBD$Date <- as.Date(Final_score_ECBD$Date)#, format = "%m/%d/%Y" )
Final_score_FEDD<-read.csv("Unstandardized_FEDD.csv")%>%
  rename(Unstandardized_score= final_score)%>%
  dplyr::select(Date,Unstandardized_score)
Final_score_FEDD$Date <- as.Date(Final_score_FEDD$Date)

#### document with interest rates from ECB
ECB_rates<- read.csv("C:/one drive/Počítač/UNI/Bachelors thesis/Data sets/ECB effective data interest changes.csv")
colnames(ECB_rates) <- c("Date","Time","Rate")
ECB_rates$Date <- as.Date(ECB_rates$Date, format = "%Y-%m-%d")
## create interest rate changes
ECB_rates<- ECB_rates %>%
  mutate(Change = Rate - lag(Rate))%>%
  dplyr::select(Date,Change)%>%
  filter(Change>0)
ECB_rates <- ECB_rates[-1, ] 
#  filter(Date != as.Date("2022-11-02"))# omit first observation as we miss a data point

library(lubridate)
# Split the data set to be able to find the date of decisions matched with the interest change date 
ECB_rates_1period<- ECB_rates%>%
  filter(year(Date) < 2005)
ECB_rates_2005<-ECB_rates%>%
  filter(year(Date) == 2005)
ECB_rates_2nd<-ECB_rates%>%
  filter(year(Date) > 2005 & year(Date) < 2012)
ECB_rates_rest<-ECB_rates%>%
  filter(year(Date) > 2012 )

#### Find the coresponding decisions meeting
Final_score_ECBD_1st <- Final_score_ECBD%>%
  filter(Date %in% (ECB_rates_1period$Date - 1))

Final_score_ECBD_2005 <- Final_score_ECBD%>%
  filter(Date %in% (ECB_rates_2005$Date - 5))

Final_score_ECBD_2nd <- Final_score_ECBD%>%
  filter(Date %in% c(ECB_rates_2nd$Date - 6, ECB_rates_2nd$Date - 7))

Final_score_ECBD_rest <- Final_score_ECBD%>%
  filter(Date %in% c(ECB_rates_rest$Date,ECB_rates_rest$Date - 6 ))

## Create data set with the interest rate change and the meeting score
Rate_change_score <- rbind(Final_score_ECBD_1st,Final_score_ECBD_2005,Final_score_ECBD_2nd,Final_score_ECBD_rest)
Rate_change <- cbind(ECB_rates,Rate_change_score$Unstandardized_score)


## create document with all the dates when the interest rates remained unchanged
No_rate_change <- Final_score_ECBD%>%
  filter(!(Date %in% Rate_change$Date))
zeros_vector <- rep(0, length(No_rate_change$Date))
NO_RATE <- cbind(No_rate_change,zeros_vector)
colnames(NO_RATE)<- c("Date","Unstandardized score","Interest Change")
NO_RATE <- NO_RATE%>%
  dplyr::select(Date,"Interest Change","Unstandardized score")
colnames(Rate_change)<- c("Date","Interest Change","Unstandardized score")

### merge the 2 data sets and save the results
ECB_rate_changes<- rbind(Rate_change,NO_RATE)
write.csv(ECB_rate_changes, file = "ECB_rate_changes.csv",row.names = FALSE)         

### calculate the correlation and create a regression
(correlationECB<-(cor.test(ECB_rate_changes$`Unstandardized score`,ECB_rate_changes$`Interest Change`)))

model_check_ECB<-lm(ECB_rate_changes$`Interest Change`~ECB_rate_changes$`Unstandardized score`)
summary(model_check_ECB)
ECB_beta_1<-linearHypothesis(model_check_ECB, "ECB_rate_changes$`Unstandardized score` = 1")

FED_1 <- read.csv("C:/one drive/Počítač/UNI/Bachelors thesis/Data sets/DFEDTAR.csv")
colnames(FED_1)<-c("DATE","Rate")
FED_2 <- read.csv("C:/one drive/Počítač/UNI/Bachelors thesis/Data sets/DFEDTARU.csv")
colnames(FED_2)<-c("DATE","Rate")
FED_rates <- rbind(FED_1,FED_2)
FED_rates <- FED_rates %>%
  mutate(Change = Rate - lag(Rate))%>%
  rename(Date = DATE)%>%
  dplyr::select(Date,Change)
FED_rates$Date<- as.Date(FED_rates$Date)

merged<- Final_score_FEDD%>%
  left_join(FED_rates, by = "Date")

write.csv(merged, file = "FED_rate_changes.csv",row.names = FALSE)         

### calculate the correlation and create a regression
(correlationFED <- (cor.test(merged$Change,merged$Unstandardized_score)))

model_check_FED<-lm(merged$Change~merged$Unstandardized_score)
summary(model_check_FED)
FED_beta_1 <- linearHypothesis(model_check_FED, "merged$Unstandardized_score = 1")

stargazer(FED_beta_1,ECB_beta_1,
          type = "latex"
          )


ggplot(data = ECB_rate_changes, aes(x = Date)) +
  geom_line(aes(y = `Unstandardized score`, color = "Unstandardized Hawk-score"), size = 1) +
  scale_y_continuous(limits = c(-0.25, 0.75), name = "Interest rate change and Unstandardized Hawk-score") +  # Add secondary axis 
  scale_x_date(
    date_breaks = "2 year",         # Increase frequency to every 6 months
    date_labels = "%Y"            # Format date labels as "Month Year"
  ) +
  labs(title = "Unstandardized Hawk-score and Deposit facility rate change ", x = "Date") +
  labs(subtitle = "ECB Decisions")+
  labs(caption = "Source: FRED")+
  theme_minimal() +
  theme(legend.position = "bottom")+
  theme(plot.title = element_text(hjust = 0.5,size=16, family = "Times", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,size=12, family = "Times", face = "italic"),
        axis.title = element_text(size = 12, family = "Times"),                                   # Change axis label font
        axis.text = element_text(size = 10, family = "Times"),                                    # Change axis text font
        legend.text = element_text(size = 12, family = "Times"),     
        legend.title = element_blank(),        # Remove legend title
        panel.grid = element_blank(),          # Remove all gridlines
        axis.line = element_line(color = "black"),
        panel.background = element_blank(),    # Remove panel background
        plot.background = element_blank()      # Remove plot background
  )

sentiment_comparison_ECB <- sentiment_score_ECBD %>%
  left_join(ECB_rate_changes, by = "Date")

sentiment_comparison_FED <- sentiment_score_FEDD %>%
  left_join(FED_rates, by = "Date")

cor.test(sentiment_comparison_FED$Standardized_sentiment_score,sentiment_comparison_FED$Change)
cor.test(sentiment_comparison_ECB$Standardized_sentiment_score,sentiment_comparison_ECB$`Interest Change`)
model_FED <- lm(sentiment_comparison_FED$Change~sentiment_comparison_FED$Standardized_sentiment_score)
summary(model_FED)

(correlationFED <- (cor.test(FED_rates$Change,Final_score_FEDD$Standardized_score)))
model_check_FED<-lm(FED_rates$Change~Final_score_FEDD$Standardized_score)
summary(model_check_FED)

