####################################### REGRESSION ANALYSIS OF THE RELATIONSHIP BETWEEN THE VOLATILITY AND HAWK-SCORE ############################################################
###
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
######################################## Load all the prepared files ########################################
## score
Final_score_ECBD<-read.csv("Final_score_ECBD.csv")
Final_score_ECBD$Date <- as.Date(Final_score_ECBD$Date)
Final_score_ECBM<-read.csv("Final_score_ECBM.csv")
Final_score_ECBM$Date <- as.Date(Final_score_ECBM$Date)
Final_score_FEDD<-read.csv("Final_score_FEDD.csv")
Final_score_FEDD$Date <- as.Date(Final_score_FEDD$Date)
Final_score_FEDM<-read.csv("Final_score_FEDM.csv")
Final_score_FEDM$Date <- as.Date(Final_score_FEDM$Date)
## smoothed volatility
smoothed_STOXX<-read.csv("C:/one drive/Počítač/UNI/Bachelors thesis/Cleaned files/smoothed_STOXX.csv")
smoothed_STOXX$Date<-as.Date(smoothed_STOXX$Date,format = "%Y-%m-%d")
smoothed_VSTOXX<-read.csv("C:/one drive/Počítač/UNI/Bachelors thesis/Cleaned files/smoothed_VSTOXX.csv")
smoothed_VSTOXX$Date<-as.Date(smoothed_VSTOXX$Date,format = "%Y-%m-%d")
smoothed_SP500<-read.csv("C:/one drive/Počítač/UNI/Bachelors thesis/Cleaned files/smoothed_SP500.csv")
smoothed_SP500$Date<-as.Date(smoothed_SP500$Date,format = "%Y-%m-%d")
smoothed_VIX<-read.csv("C:/one drive/Počítač/UNI/Bachelors thesis/Cleaned files/smoothed_VIX.csv")
smoothed_VIX$Date<-as.Date(smoothed_VIX$Date,format = "%Y-%m-%d")
## Volatility
volatility_STOXX<-read.csv("STOXX.csv")
volatility_STOXX$Date<-as.Date(volatility_STOXX$Date,format = "%Y-%m-%d")
volatility_VSTOXX<-read.csv("VSTOXX.csv")
volatility_VSTOXX$Date<-as.Date(volatility_VSTOXX$Date,format = "%m/%d/%Y")
volatility_SP500<-read.csv("SP500.csv")
volatility_SP500$Date<-as.Date(volatility_SP500$Date,format = "%Y-%m-%d")
volatility_VIX<-read.csv("VIX.csv")
volatility_VIX$Date<-as.Date(volatility_VIX$Date,format = "%Y-%m-%d")
## readability
readability_ECBD<-read.csv("readability_ECBD.csv")
readability_ECBD$Date <- as.Date(readability_ECBD$Date)
readability_ECBM<-read.csv("readability_ECBM.csv")
readability_ECBM$Date <- as.Date(readability_ECBM$Date)
readability_FEDD<-read.csv("readability_FEDD.csv")
readability_FEDD$Date <- as.Date(readability_FEDD$Date)
readability_FEDM<-read.csv("readability_FEDM.csv")
readability_FEDM$Date <- as.Date(readability_FEDM$Date)
## uncertainty
CPI_ECBD<-read.csv("CPI_ECBD.csv")
CPI_ECBD$Date <- as.Date(CPI_ECBD$Date)
CPI_ECBM<-read.csv("CPI_ECBM.csv")
CPI_ECBM$Date <- as.Date(CPI_ECBM$Date)
CPI_FEDD<-read.csv("CPI_FEDD.csv")
CPI_FEDD$Date <- as.Date(CPI_FEDD$Date)
CPI_FEDM<-read.csv("CPI_FEDM.csv")
CPI_FEDM$Date <- as.Date(CPI_FEDM$Date)
GDP_ECBD<-read.csv("GDP_ECBD.csv")
GDP_ECBD$Date <- as.Date(GDP_ECBD$Date)
GDP_ECBM<-read.csv("GDP_ECBM.csv")
GDP_ECBM$Date <- as.Date(GDP_ECBM$Date)
GDP_FEDD<-read.csv("GDP_FEDD.csv")
GDP_FEDD$Date <- as.Date(GDP_FEDD$Date)
GDP_FEDM<-read.csv("GDP_FEDM.csv")
GDP_FEDM$Date <- as.Date(GDP_FEDM$Date)
## sentiment
sentiment_score_ECBD<-read.csv("sentiment_score_ECBD.csv")
sentiment_score_ECBD$Date <- as.Date(sentiment_score_ECBD$Date)
sentiment_score_ECBM<-read.csv("sentiment_score_ECBM.csv")
sentiment_score_ECBM$Date <- as.Date(sentiment_score_ECBM$Date)
sentiment_score_FEDD<-read.csv("sentiment_score_FEDD.csv")
sentiment_score_FEDD$Date <- as.Date(sentiment_score_FEDD$Date)
sentiment_score_FEDM<-read.csv("sentiment_score_FEDM.csv")
sentiment_score_FEDM$Date <- as.Date(sentiment_score_FEDM$Date)
## topic dummies
topics_ECBD_DUMMY<-read.csv("topics_ECBD_DUMMY.csv")
topics_ECBD_DUMMY$Date <- as.Date(topics_ECBD_DUMMY$Date)
topics_ECBM_DUMMY<-read.csv("topics_ECBM_DUMMY.csv")
topics_ECBM_DUMMY$Date <- as.Date(topics_ECBM_DUMMY$Date)
topics_FEDD_DUMMY<-read.csv("topics_FEDD_DUMMY.csv")
topics_FEDD_DUMMY$Date <- as.Date(topics_FEDD_DUMMY$Date)
topics_FEDM_DUMMY<-read.csv("topics_FEDM_DUMMY.csv")
topics_FEDM_DUMMY$Date <- as.Date(topics_FEDM_DUMMY$Date)
## Word count per document
WCPD_ECBD<-read.csv("WCPD_ECBD.csv")
WCPD_ECBD$Date <- as.Date(WCPD_ECBD$Date)
WCPD_ECBM<-read.csv("WCPD_ECBM.csv")
WCPD_ECBM$Date <- as.Date(WCPD_ECBM$Date)
WCPD_FEDD<-read.csv("WCPD_FEDD.csv")
WCPD_FEDD$Date <- as.Date(WCPD_FEDD$Date)
WCPD_FEDM<-read.csv("WCPD_FEDM.csv")
WCPD_FEDM$Date <- as.Date(WCPD_FEDM$Date)

################################# Create a data set with all metrics for each document type #################################
ECBD <- Final_score_ECBD%>%
  left_join(volatility_STOXX, by = "Date")%>%
  left_join(volatility_VSTOXX, by = "Date")%>%
  left_join(topics_ECBD_DUMMY, by = "Date")%>%
  left_join(sentiment_score_ECBD, by = "Date")%>%
  left_join(CPI_ECBD, by = "Date")%>%
  left_join(GDP_ECBD, by = "Date")%>%
  left_join(readability_ECBD, by = "Date")%>%
  left_join(WCPD_ECBD, by = "Date")%>%
  mutate(Post_2008 = if_else(as.Date(Date) >= as.Date("2013-01-01"), 1, 0))%>%
  dplyr:: select(!c(topic_keyword,count,Text))
colnames(ECBD) <- c("Date", "Standardized_score","Policy_score","Finance_score",
                    "Growth_score","Prices_score","Global_score",
                    "Other_score",
                    "STOXX","STOXXT1","STOXXT2","STOXXT3","STOXXT4","STOXXT5",
                    "STOXXT6","STOXXT7","STOXXT8","STOXXT9","STOXXT10", 
                    "VSTOXX", "VSTOXXT1","VSTOXXT2","VSTOXXT3","VSTOXXT4","VSTOXXT5",
                    "VSTOXXT6","VSTOXXT7","VSTOXXT8","VSTOXXT9","VSTOXXT10",
                    "Finance","Growth","Policy","Prices","Sentiment","Uncertainty",
                    "CPI","GDP","Flesch_Kincaid","Word_count","Post_2008")


ECBM <- Final_score_ECBM%>%
  left_join(volatility_STOXX, by = "Date")%>%
  left_join(volatility_VSTOXX, by = "Date")%>%
  left_join(topics_ECBM_DUMMY, by = "Date")%>%
  left_join(sentiment_score_ECBM, by = "Date")%>%
  left_join(CPI_ECBM, by = "Date")%>%
  left_join(GDP_ECBM, by = "Date")%>%
  left_join(readability_ECBM, by = "Date")%>%
  left_join(WCPD_ECBM, by = "Date")%>%
  dplyr:: select(!c(topic_keyword,count,Text))
colnames(ECBM) <- c("Date","Standardized_score","Finance_score",
                    "Policy_score","Prices_score","Global_score","Growth_score","Other_score", 
                    "STOXX","STOXXT1","STOXXT2","STOXXT3","STOXXT4","STOXXT5",
                    "STOXXT6","STOXXT7","STOXXT8","STOXXT9","STOXXT10", 
                    "VSTOXX", "VSTOXXT1","VSTOXXT2","VSTOXXT3","VSTOXXT4","VSTOXXT5",
                    "VSTOXXT6","VSTOXXT7","VSTOXXT8","VSTOXXT9","VSTOXXT10",
                    "Finance","Growth","Other","Policy","Prices","Sentiment","Uncertainty",
                    "CPI","GDP","Flesch_Kincaid","Word_count")

FEDD <- Final_score_FEDD%>%
  left_join(volatility_SP500, by = "Date")%>%
  left_join(volatility_VIX, by = "Date")%>%
  left_join(topics_FEDD_DUMMY, by = "Date")%>%
  left_join(sentiment_score_FEDD, by = "Date")%>%
  left_join(CPI_FEDD, by = "Date")%>%
  left_join(GDP_FEDD, by = "Date")%>%
  left_join(readability_FEDD, by = "Date")%>%
  left_join(WCPD_FEDD, by = "Date")%>%
  mutate(Post_2008 = if_else(as.Date(Date) >= as.Date("2009-01-01"), 1, 0))%>%
  dplyr:: select(!c(topic_keyword,count,Text))
colnames(FEDD) <- c("Date", "Standardized_score","Policy_score","Growth_score","Global_score",
                    "Prices_score","Other_score","Finance_score",
                    "SP500","SP500T1","SP500T2","SP500T3","SP500T4","SP500T5",
                    "SP500T6","SP500T7","SP500T8","SP500T9","SP500T10", 
                    "VIX", "VIXT1","VIXT2","VIXT3","VIXT4","VIXT5",
                    "VIXT6","VIXT7","VIXT8","VIXT9","VIXT10",
                    "Finance","Growth","Other","Policy","Prices","Sentiment","Uncertainty",
                    "CPI","GDP","Flesch_Kincaid","Word_count","Post_2008")

FEDM <- Final_score_FEDM%>%
  left_join(volatility_SP500, by = "Date")%>%
  left_join(volatility_VIX, by = "Date")%>%
  left_join(topics_FEDM_DUMMY, by = "Date")%>%
  left_join(sentiment_score_FEDM, by = "Date")%>%
  left_join(CPI_FEDM, by = "Date")%>%
  left_join(GDP_FEDM, by = "Date")%>%
  left_join(readability_FEDM, by = "Date")%>%
  left_join(WCPD_FEDM, by = "Date")%>%
  mutate(Post_2008 = if_else(as.Date(Date) >= as.Date("2009-01-01"), 1, 0))%>%
  dplyr:: select(!c(topic_keyword,count,Text))
colnames(FEDM) <- c("Date","Standardized_score","Global_score","Policy_score","Finance_score","Growth_score","Other_score","Prices_score", 
                    "SP500","SP500T1","SP500T2","SP500T3","SP500T4","SP500T5",
                    "SP500T6","SP500T7","SP500T8","SP500T9","SP500T10", 
                    "VIX", "VIXT1","VIXT2","VIXT3","VIXT4","VIXT5",
                    "VIXT6","VIXT7","VIXT8","VIXT9","VIXT10",
                    "Finance","Growth","Policy","Prices","Sentiment","Uncertainty",
                    "CPI","GDP","Flesch_Kincaid","Word_count","Post_2008")



############################################### Simple regression  - volatility on hawk-score#####################################
baselinemodel_ECBD_STOXX<- lm(STOXX~Standardized_score, data = ECBD)
baselinemodel_ECBD_VSTOXX<- lm(VSTOXX~Standardized_score, data = ECBD)
baselinemodel_ECBM_VSTOXX<- lm(VSTOXX~Standardized_score, data = ECBM)
baselinemodel_ECBM_STOXX<- lm(STOXX~Standardized_score, data = ECBM)
baselinemodel_FEDD_VIX<- lm(VIX~Standardized_score, data = FEDD)
baselinemodel_FEDD_SP500<- lm(SP500~Standardized_score , data= FEDD)
baselinemodel_FEDM_VIX<- lm(VIX~Standardized_score, data = FEDM)
baselinemodel_FEDM_SP500<- lm(SP500~Standardized_score , data = FEDM)

######################################## Model with a volatility lag ########################################
laggedmodel_ECBD_VSTOXX <- lm(VSTOXX ~ Standardized_score + VSTOXXT1, data = ECBD)
laggedmodel_ECBD_STOXX <- lm(STOXX ~ Standardized_score + STOXXT1, data = ECBD)
laggedmodel_ECBM_VSTOXX <- lm(VSTOXX ~ Standardized_score + VSTOXXT1, data =ECBM)
laggedmodel_ECBM_STOXX <- lm(STOXX ~ Standardized_score + STOXXT1, data = ECBM)
laggedmodel_FEDD_VIX <- lm(VIX ~ Standardized_score + VIXT1, data = FEDD)
laggedmodel_FEDD_SP500 <- lm(SP500 ~ Standardized_score + SP500T1, data = FEDD)
laggedmodel_FEDM_VIX <- lm(VIX ~ Standardized_score + VIXT1, data = FEDM)
laggedmodel_FEDM_SP500 <- lm(SP500 ~ Standardized_score + SP500T1, data = FEDM)

######################################## Model with sentiment, lag  ########################################
sentimentmodel_ECBD_VSTOXX <- lm(VSTOXX ~ Standardized_score + VSTOXXT1 + Sentiment + Uncertainty, data = ECBD)
sentimentmodel_ECBD_STOXX <- lm(STOXX ~ Standardized_score + STOXXT1 + Sentiment + Uncertainty, data = ECBD)
sentimentmodel_ECBM_VSTOXX <- lm(VSTOXX ~ Standardized_score + VSTOXXT1 + Sentiment + Uncertainty, data = ECBM )
sentimentmodel_ECBM_STOXX <- lm(STOXX ~ Standardized_score + STOXXT1 + Sentiment + Uncertainty, data = ECBM )
sentimentmodel_FEDD_VIX <- lm(VIX ~ Standardized_score + VIXT1 + Sentiment + Uncertainty, data = FEDD)
sentimentmodel_FEDD_SP500 <- lm(SP500 ~ Standardized_score + SP500T1 + Sentiment + Uncertainty, data = FEDD)
sentimentmodel_FEDM_VIX <- lm(VIX ~ Standardized_score + VIXT1 + Sentiment + Uncertainty, data = FEDM)
sentimentmodel_FEDM_SP500 <- lm(SP500 ~ Standardized_score + SP500T1 + Sentiment + Uncertainty, data = FEDM)

######################################## Including uncertainty and lag ########################################
uncertainty_model_ECBD_STOXX <- lm(STOXX ~ Standardized_score + STOXXT1 + CPI + GDP, data = ECBD)
uncertainty_model_ECBD_VSTOXX <- lm(VSTOXX ~ Standardized_score + VSTOXXT1 + CPI + GDP, data = ECBD)
uncertainty_model_ECBM_STOXX <- lm(STOXX ~ Standardized_score + STOXXT1 + CPI + GDP, data = ECBM)
uncertainty_model_ECBM_VSTOXX <- lm(VSTOXX ~ Standardized_score + VSTOXXT1+ CPI + GDP, data = ECBM)
uncertainty_model_FEDD_SP500 <- lm(SP500 ~ Standardized_score + SP500T1 + CPI + GDP, data = FEDD)
uncertainty_model_FEDD_VIX <- lm(VIX ~ Standardized_score + VIXT1 + CPI + GDP, data = FEDD)
uncertainty_model_FEDM_SP500 <- lm(SP500 ~ Standardized_score + SP500T1 + CPI + GDP, data = FEDM)
uncertainty_model_FEDM_VIX <- lm(VIX ~ Standardized_score + VIXT1 + CPI + GDP, data = FEDM)

##################################### Including readability with a lag #####################################
readability_model_ECBD_STOXX <- lm(STOXX ~ Standardized_score + STOXXT1 + CPI + GDP + Flesch_Kincaid, data = ECBD)
readability_model_ECBD_VSTOXX <- lm(VSTOXX ~ Standardized_score + VSTOXXT1 + CPI + GDP + Flesch_Kincaid, data = ECBD)
readability_model_ECBM_STOXX <- lm(STOXX ~ Standardized_score + STOXXT1 + CPI + GDP + Flesch_Kincaid, data = ECBM)
readability_model_ECBM_VSTOXX <- lm(VSTOXX ~ Standardized_score + VSTOXXT1+ CPI + GDP + Flesch_Kincaid, data = ECBM)
readability_model_FEDD_SP500 <- lm(SP500 ~ Standardized_score + SP500T1+  CPI + GDP + Flesch_Kincaid, data = FEDD)
readability_model_FEDD_VIX <- lm(VIX ~ Standardized_score + VIXT1 + CPI + GDP+ Flesch_Kincaid, data = FEDD)
readability_model_FEDM_SP500 <- lm(SP500 ~ Standardized_score + SP500T1 +  CPI + GDP+ Flesch_Kincaid, data = FEDM)
readability_model_FEDM_VIX <- lm(VIX ~ Standardized_score + VIXT1 +  CPI + GDP+ Flesch_Kincaid, data = FEDM)

########################################### Including word count  ################################
word_ECBD_VSTOXX <- lm(VSTOXX ~ Standardized_score + VSTOXXT1 + CPI + GDP + Flesch_Kincaid + Word_count, data = ECBD)
word_ECBD_STOXX <- lm(STOXX ~ Standardized_score + STOXXT1+  CPI + GDP + Flesch_Kincaid + Word_count, data = ECBD)
word_ECBM_VSTOXX <- lm(VSTOXX ~ Standardized_score + VSTOXXT1+  CPI + GDP + Flesch_Kincaid + Word_count, data =ECBM)
word_ECBM_STOXX <- lm(STOXX ~ Standardized_score + STOXXT1+  CPI + GDP + Flesch_Kincaid + Word_count, data = ECBM)
word_FEDD_VIX <- lm(VIX ~ Standardized_score + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count, data = FEDD)
word_FEDD_SP500 <- lm(SP500 ~ Standardized_score + SP500T1 +  CPI + GDP + Flesch_Kincaid + Word_count, data = FEDD)
word_FEDM_VIX <- lm(VIX ~ Standardized_score + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count, data = FEDM)
word_FEDM_SP500 <- lm(SP500 ~ Standardized_score + SP500T1 +  CPI + GDP + Flesch_Kincaid + Word_count, data = FEDM)

########################################### Including trend and lag  ################################
trend_ECBD_VSTOXX <- lm(VSTOXX ~ Standardized_score + VSTOXXT1 + CPI + GDP + Flesch_Kincaid + Word_count + I(1:length(VSTOXXT1)), data = ECBD)
trend_ECBD_STOXX <- lm(STOXX ~ Standardized_score + STOXXT1+  CPI + GDP + Flesch_Kincaid + Word_count+ I(1:length(STOXXT1)), data = ECBD)
trend_ECBM_VSTOXX <- lm(VSTOXX ~ Standardized_score + VSTOXXT1+  CPI + GDP + Flesch_Kincaid + Word_count+ I(1:length(VSTOXXT1)), data =ECBM)
trend_ECBM_STOXX <- lm(STOXX ~ Standardized_score + STOXXT1+  CPI + GDP + Flesch_Kincaid + Word_count+ I(1:length(STOXXT1)), data = ECBM)
trend_FEDD_VIX <- lm(VIX ~ Standardized_score + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count+ I(1:length(VIXT1)), data = FEDD)
trend_FEDD_SP500 <- lm(SP500 ~ Standardized_score + SP500T1 +  CPI + GDP + Flesch_Kincaid + Word_count+ I(1:length(SP500T1)), data = FEDD)
trend_FEDM_VIX <- lm(VIX ~ Standardized_score + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count+ I(1:length(VIXT1)), data = FEDM)
trend_FEDM_SP500 <- lm(SP500 ~ Standardized_score + SP500T1 +  CPI + GDP + Flesch_Kincaid + Word_count+ I(1:length(SP500T1)), data = FEDM)

########################################### Structural break  ################################
structural_break_model_ECBD_VSTOXX <- lm(
  VSTOXX ~ Standardized_score * Post_2008 + VSTOXXT1 + CPI + GDP + Flesch_Kincaid + Word_count,data = ECBD)
structural_break_model_ECBD_STOXX <- lm(
  STOXX ~ Standardized_score * Post_2008 + STOXXT1 + CPI + GDP + Flesch_Kincaid + Word_count,data = ECBD)
structural_break_model_FEDD_VIX <- lm(
  VIX ~ Standardized_score * Post_2008 + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count,data = FEDD)
structural_break_model_FEDD_SP500 <- lm(
  SP500 ~ Standardized_score * Post_2008 + SP500T1 + CPI + GDP + Flesch_Kincaid + Word_count,data = FEDD)
structural_break_model_FEDM_SP500 <- lm(
  SP500 ~ Standardized_score * Post_2008 + SP500T1 + CPI + GDP + Flesch_Kincaid + Word_count,data = FEDM)
structural_break_model_FEDM_VIX <- lm(
  VIX ~ Standardized_score * Post_2008 + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count,data = FEDM)

library(strucchange)

# Perform Chow Test for structural break at 2008
(chow_test_ECBD_STOXX <- sctest(STOXX ~ Standardized_score + STOXXT1 + CPI + GDP + Flesch_Kincaid + Word_count, 
                    type = "Chow", data = ECBD, 
                    breakpoint = which(as.Date(ECBD$Date) == as.Date("2013-01-01"))))
(chow_test_ECBD_VSTOXX <- sctest(VSTOXX ~ Standardized_score + VSTOXXT1 + CPI + GDP + Flesch_Kincaid + Word_count, 
                               type = "Chow", data = ECBD, 
                               breakpoint = which(as.Date(ECBD$Date) == as.Date("2013-01-01"))))
(chow_test_FEDD_SP500 <- sctest(SP500 ~ Standardized_score + SP500T1 + CPI + GDP + Flesch_Kincaid + Word_count, 
                               type = "Chow", data = FEDD, 
                               breakpoint = which(as.Date(ECBD$Date) == as.Date("2009-01-01"))))
(chow_test_FEDM_SP500 <- sctest(SP500 ~ Standardized_score + SP500T1 + CPI + GDP + Flesch_Kincaid + Word_count, 
                               type = "Chow", data = FEDM, 
                               breakpoint = which(as.Date(ECBD$Date) == as.Date("2009-01-01"))))
(chow_test_FEDD_VIX <- sctest(VIX ~ Standardized_score + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count, 
                               type = "Chow", data = FEDD, 
                               breakpoint = which(as.Date(ECBD$Date) == as.Date("2009-01-01"))))
(chow_test_FEDM_VIX <- sctest(VIX ~ Standardized_score + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count, 
                               type = "Chow", data = FEDM, 
                               breakpoint = which(as.Date(ECBD$Date) == as.Date("2009-01-01"))))

############################################# Test pre and post 2013 or 2008 #############################################
# Subset for pre-2008
ECBD_pre_2013 <- ECBD %>% filter(Date < as.Date("2013-01-01"))
ECBD_post_2013 <- ECBD %>% filter(Date >= as.Date("2013-01-01"))
FEDD_pre_2009 <- FEDD %>% filter(Date < as.Date("2009-01-01"))
FEDD_post_2009 <- FEDD %>% filter(Date >= as.Date("2009-01-01"))
FEDM_pre_2009 <- FEDM %>% filter(Date < as.Date("2009-01-01"))
FEDM_post_2009 <- FEDM %>% filter(Date >= as.Date("2009-01-01"))

pre_ECBD_VSTOXX <- lm(VSTOXX ~ Standardized_score + VSTOXXT1 + CPI + GDP + Flesch_Kincaid + Word_count, data = ECBD_pre_2013)
pre_ECBD_STOXX <- lm(STOXX ~ Standardized_score + STOXXT1+  CPI + GDP + Flesch_Kincaid + Word_count, data = ECBD_pre_2013)
pre_FEDD_VIX <- lm(VIX ~ Standardized_score + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count, data = FEDD_pre_2009)
pre_FEDD_SP500 <- lm(SP500 ~ Standardized_score + SP500T1 +  CPI + GDP + Flesch_Kincaid + Word_count, data = FEDD_pre_2009)
pre_FEDM_VIX <- lm(VIX ~ Standardized_score + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count, data = FEDM_pre_2009)
pre_FEDM_SP500 <- lm(SP500 ~ Standardized_score + SP500T1 +  CPI + GDP + Flesch_Kincaid + Word_count, data = FEDM_pre_2009)

post_ECBD_VSTOXX <- lm(VSTOXX ~ Standardized_score + VSTOXXT1 + CPI + GDP + Flesch_Kincaid + Word_count, data = ECBD_post_2013)
post_ECBD_STOXX <- lm(STOXX ~ Standardized_score + STOXXT1+  CPI + GDP + Flesch_Kincaid + Word_count, data = ECBD_post_2013)
post_FEDD_VIX <- lm(VIX ~ Standardized_score + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count, data = FEDD_post_2009)
post_FEDD_SP500 <- lm(SP500 ~ Standardized_score + SP500T1 +  CPI + GDP + Flesch_Kincaid + Word_count, data = FEDD_post_2009)
post_FEDM_VIX <- lm(VIX ~ Standardized_score + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count, data = FEDM_post_2009)
post_FEDM_SP500 <- lm(SP500 ~ Standardized_score + SP500T1 +  CPI + GDP + Flesch_Kincaid + Word_count, data = FEDM_post_2009)

#############################################
# Repeat with separated topics
########################################### Including topic scores and lag  ################################
topic_baselinemodel_ECBD_VSTOXX <- lm(VSTOXX ~  Finance_score  + Global_score + 
                                  Growth_score + Policy_score + Other_score +Prices_score , data = ECBD)
topic_baselinemodel_ECBD_STOXX <- lm(STOXX ~   Finance_score  + Global_score + 
                                       Growth_score + Policy_score + Other_score +Prices_score, data = ECBD)
topic_baselinemodel_ECBM_VSTOXX <- lm(VSTOXX ~    Finance_score  + Global_score + 
                                        Growth_score + Policy_score + Other_score +Prices_score , data =ECBM)
summary(topic_baselinemodel_ECBM_VSTOXX)
topic_baselinemodel_ECBM_STOXX <- lm(STOXX ~   Finance_score  + Global_score + 
                                       Growth_score + Policy_score + Other_score +Prices_score, data = ECBM) ## global and growth return NA thus are excluded
summary(topic_baselinemodel_ECBM_STOXX)
topic_baselinemodel_FEDD_VIX <- lm(VIX ~   Finance_score  + Global_score + 
                                     Growth_score + Policy_score + Other_score +Prices_score, data = FEDD)
topic_baselinemodel_FEDD_SP500 <- lm(SP500 ~   Finance_score  + Global_score + 
                                       Growth_score + Policy_score + Other_score +Prices_score, data = FEDD)
topic_baselinemodel_FEDM_VIX <- lm(VIX ~   Finance_score  + Global_score + 
                                     Growth_score + Policy_score + Other_score +Prices_score , data = FEDM)
topic_baselinemodel_FEDM_SP500 <- lm(SP500 ~ Finance_score  + Global_score + 
                                       Growth_score + Policy_score + Other_score +Prices_score, data = FEDM)
summary(topic_baselinemodel_FEDM_SP500)
summary(topic_baselinemodel_FEDM_VIX)

######################################## Model with a volatility lag ########################################
topic_laggedmodel_ECBD_VSTOXX <- lm(VSTOXX ~ Finance_score  + Global_score + 
                                      Growth_score + Policy_score + Other_score +Prices_score + VSTOXXT1, data = ECBD)
topic_laggedmodel_ECBD_STOXX <- lm(STOXX ~Finance_score  + Global_score + 
                                     Growth_score + Policy_score + Other_score +Prices_score + STOXXT1,data = ECBD)
topic_laggedmodel_ECBM_VSTOXX <- lm(VSTOXX ~ Finance_score  + Global_score + 
                                      Growth_score + Policy_score + Other_score +Prices_score+ VSTOXXT1, data =ECBM)
topic_laggedmodel_ECBM_STOXX <- lm(STOXX ~ Finance_score  + Global_score + 
                                     Growth_score + Policy_score + Other_score +Prices_score + STOXXT1,data = ECBM)
topic_laggedmodel_FEDD_VIX <- lm(VIX ~ Finance_score  + Global_score + 
                                   Growth_score + Policy_score + Other_score +Prices_score + VIXT1, data = FEDD)
topic_laggedmodel_FEDD_SP500 <- lm(SP500 ~ Finance_score  + Global_score + 
                                     Growth_score + Policy_score + Other_score +Prices_score + SP500T1,data = FEDD)
topic_laggedmodel_FEDM_VIX <- lm(VIX ~ Finance_score  + Global_score + 
                                   Growth_score + Policy_score + Other_score +Prices_score + VIXT1, data = FEDM)
topic_laggedmodel_FEDM_SP500 <- lm(SP500 ~ Finance_score  + Global_score + 
                                     Growth_score + Policy_score + Other_score +Prices_score + SP500T1, data = FEDM)

######################################## Model with sentiment, lag  ########################################
topic_sentimentmodel_ECBD_VSTOXX <- lm(VSTOXX ~ Finance_score  + Global_score + 
                                         Growth_score + Policy_score + Other_score +Prices_score + VSTOXXT1 + Sentiment + Uncertainty, data = ECBD)
topic_sentimentmodel_ECBD_STOXX <- lm(STOXX ~ Finance_score  + Global_score + 
                                        Growth_score + Policy_score + Other_score +Prices_score + STOXXT1 + Sentiment + Uncertainty, data = ECBD)
topic_sentimentmodel_ECBM_VSTOXX <- lm(VSTOXX ~ Finance_score  + Global_score + 
                                         Growth_score + Policy_score + Other_score +Prices_score + VSTOXXT1 + Sentiment + Uncertainty, data = ECBM )
topic_sentimentmodel_ECBM_STOXX <- lm(STOXX ~ Finance_score  + Global_score + 
                                        Growth_score + Policy_score + Other_score +Prices_score + STOXXT1 + Sentiment + Uncertainty,data = ECBM )
topic_sentimentmodel_FEDD_VIX <- lm(VIX ~ Finance_score  + Global_score + 
                                      Growth_score + Policy_score + Other_score +Prices_score + VIXT1 + Sentiment + Uncertainty, data = FEDD)
topic_sentimentmodel_FEDD_SP500 <- lm(SP500 ~ Finance_score  + Global_score + 
                                        Growth_score + Policy_score + Other_score +Prices_score + SP500T1 + Sentiment + Uncertainty, data = FEDD)
topic_sentimentmodel_FEDM_VIX <- lm(VIX ~ Finance_score  + Global_score + 
                                      Growth_score + Policy_score + Other_score +Prices_score + VIXT1 + Sentiment + Uncertainty, data = FEDM)
topic_sentimentmodel_FEDM_SP500 <- lm(SP500 ~ Finance_score  + Global_score + 
                                        Growth_score + Policy_score + Other_score +Prices_score + SP500T1 + Sentiment + Uncertainty, data = FEDM)

######################################## Including uncertainty and lag ########################################
topic_uncertainty_model_ECBD_STOXX <- lm(STOXX ~ Finance_score  + Global_score + 
                                           Growth_score + Policy_score + Other_score +Prices_score + STOXXT1 + CPI + GDP, data = ECBD)
topic_uncertainty_model_ECBD_VSTOXX <- lm(VSTOXX ~Finance_score  + Global_score + 
                                            Growth_score + Policy_score + Other_score +Prices_score + VSTOXXT1 + CPI + GDP, data = ECBD)
topic_uncertainty_model_ECBM_STOXX <- lm(STOXX ~ Finance_score  + Global_score + 
                                           Growth_score + Policy_score + Other_score +Prices_score + STOXXT1 + CPI + GDP, data = ECBM)
topic_uncertainty_model_ECBM_VSTOXX <- lm(VSTOXX ~ Finance_score  + Global_score + 
                                            Growth_score + Policy_score + Other_score +Prices_score + VSTOXXT1+ CPI + GDP, data = ECBM)
topic_uncertainty_model_FEDD_SP500 <- lm(SP500 ~ Finance_score  + Global_score + 
                                           Growth_score + Policy_score + Other_score +Prices_score + SP500T1 + CPI + GDP, data = FEDD)
topic_uncertainty_model_FEDD_VIX <- lm(VIX ~ Finance_score  + Global_score + 
                                         Growth_score + Policy_score + Other_score +Prices_score + VIXT1 + CPI + GDP, data = FEDD)
topic_uncertainty_model_FEDM_SP500 <- lm(SP500 ~ Finance_score  + Global_score + 
                                           Growth_score + Policy_score + Other_score +Prices_score + SP500T1 + CPI + GDP, data = FEDM)
topic_uncertainty_model_FEDM_VIX <- lm(VIX ~ Finance_score  + Global_score + 
                                         Growth_score + Policy_score + Other_score +Prices_score + VIXT1 + CPI + GDP, data = FEDM)

##################################### Including readability with a lag #####################################
topic_readability_model_ECBD_STOXX <- lm(STOXX ~ Finance_score  + Global_score + 
                                           Growth_score + Policy_score + Other_score +Prices_score+ STOXXT1 + CPI + GDP + Flesch_Kincaid, data = ECBD)
topic_readability_model_ECBD_VSTOXX <- lm(VSTOXX ~ Finance_score  + Global_score + 
                                            Growth_score + Policy_score + Other_score +Prices_score + VSTOXXT1 + CPI + GDP + Flesch_Kincaid, data = ECBD)
topic_readability_model_ECBM_STOXX <- lm(STOXX ~ Finance_score  + Global_score + 
                                           Growth_score + Policy_score + Other_score +Prices_score + STOXXT1 + CPI + GDP + Flesch_Kincaid,data = ECBM)
topic_readability_model_ECBM_VSTOXX <- lm(VSTOXX ~ Finance_score  + Global_score + 
                                            Growth_score + Policy_score + Other_score +Prices_score+ VSTOXXT1+ CPI + GDP + Flesch_Kincaid, data = ECBM)
topic_readability_model_FEDD_SP500 <- lm(SP500 ~ Finance_score  + Global_score + 
                                           Growth_score + Policy_score + Other_score +Prices_score + SP500T1+  CPI + GDP + Flesch_Kincaid, data = FEDD)
topic_readability_model_FEDD_VIX <- lm(VIX ~ Finance_score  + Global_score + 
                                         Growth_score + Policy_score + Other_score +Prices_score + VIXT1 + CPI + GDP+ Flesch_Kincaid, data = FEDD)
topic_readability_model_FEDM_SP500 <- lm(SP500 ~ Finance_score  + Global_score + 
                                           Growth_score + Policy_score + Other_score +Prices_score + SP500T1 +  CPI + GDP+ Flesch_Kincaid, data = FEDM)
topic_readability_model_FEDM_VIX <- lm(VIX ~ Finance_score  + Global_score + 
                                         Growth_score + Policy_score + Other_score +Prices_score + VIXT1 +  CPI + GDP+ Flesch_Kincaid, data = FEDM)

########################################### Including word count and lag  ################################
topic_word_ECBD_VSTOXX <- lm(VSTOXX ~Finance_score  + Global_score + 
                               Growth_score + Policy_score + Other_score +Prices_score + VSTOXXT1 + CPI + GDP + Flesch_Kincaid + Word_count, data = ECBD)
topic_word_ECBD_STOXX <- lm(STOXX ~ Finance_score  + Global_score + 
                              Growth_score + Policy_score + Other_score +Prices_score + STOXXT1+  CPI + GDP + Flesch_Kincaid + Word_count, data = ECBD)
topic_word_ECBM_VSTOXX <- lm(VSTOXX ~ Finance_score  + Global_score + 
                               Growth_score + Policy_score + Other_score +Prices_score + VSTOXXT1+  CPI + GDP + Flesch_Kincaid + Word_count, data =ECBM)
topic_word_ECBM_STOXX <- lm(STOXX ~ Finance_score  + Global_score + 
                              Growth_score + Policy_score + Other_score +Prices_score + STOXXT1+  CPI + GDP + Flesch_Kincaid + Word_count, data = ECBM)
topic_word_FEDD_VIX <- lm(VIX ~ Finance_score  + Global_score + 
                            Growth_score + Policy_score + Other_score +Prices_score + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count, data = FEDD)
topic_word_FEDD_SP500 <- lm(SP500 ~ Finance_score  + Global_score + 
                              Growth_score + Policy_score + Other_score +Prices_score + SP500T1 +  CPI + GDP + Flesch_Kincaid + Word_count, data = FEDD)
topic_word_FEDM_VIX <- lm(VIX ~ Finance_score  + Global_score + 
                            Growth_score + Policy_score + Other_score +Prices_score + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count, data = FEDM)
topic_word_FEDM_SP500 <- lm(SP500 ~ Finance_score  + Global_score + 
                              Growth_score + Policy_score + Other_score +Prices_score + SP500T1 +  CPI + GDP + Flesch_Kincaid + Word_count, data = FEDM)

########################################### Including trend and lag  ################################
topic_trend_ECBD_VSTOXX <- lm(VSTOXX ~ Finance_score  + Global_score + 
                                Growth_score + Policy_score + Other_score +Prices_score+ VSTOXXT1 + CPI + GDP + Flesch_Kincaid + Word_count + I(1:length(VSTOXXT1)), data = ECBD)
topic_trend_ECBD_STOXX <- lm(STOXX ~ Finance_score  + Global_score + 
                               Growth_score + Policy_score + Other_score +Prices_score + STOXXT1+  CPI + GDP + Flesch_Kincaid + Word_count+ I(1:length(STOXXT1)), data = ECBD)
topic_trend_ECBM_VSTOXX <- lm(VSTOXX ~ Finance_score  + Global_score + 
                                Growth_score + Policy_score + Other_score +Prices_score + VSTOXXT1+  CPI + GDP + Flesch_Kincaid + Word_count+ I(1:length(VSTOXXT1)), data =ECBM)
topic_trend_ECBM_STOXX <- lm(STOXX ~ Finance_score  + Global_score + 
                               Growth_score + Policy_score + Other_score +Prices_score + STOXXT1+  CPI + GDP + Flesch_Kincaid + Word_count+ I(1:length(STOXXT1)), data = ECBM)
topic_trend_FEDD_VIX <- lm(VIX ~ Finance_score  + Global_score + 
                             Growth_score + Policy_score + Other_score +Prices_score + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count+ I(1:length(VIXT1)), data = FEDD)
topic_trend_FEDD_SP500 <- lm(SP500 ~ Finance_score  + Global_score + 
                               Growth_score + Policy_score + Other_score +Prices_score + SP500T1 +  CPI + GDP + Flesch_Kincaid + Word_count+ I(1:length(SP500T1)), data = FEDD)
topic_trend_FEDM_VIX <- lm(VIX ~ Finance_score  + Global_score + 
                             Growth_score + Policy_score + Other_score +Prices_score + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count+ I(1:length(VIXT1)), data = FEDM)
topic_trend_FEDM_SP500 <- lm(SP500 ~ Finance_score  + Global_score + 
                               Growth_score + Policy_score + Other_score +Prices_score + SP500T1 +  CPI + GDP + Flesch_Kincaid + Word_count+ I(1:length(SP500T1)), data = FEDM)

########################################### Structural break  ################################
topic_structural_break_model_ECBD_VSTOXX <- lm(
  VSTOXX ~ (Finance_score  + Global_score + 
              Growth_score + Policy_score + Other_score +Prices_score) * Post_2008 + VSTOXXT1 + CPI + GDP + Flesch_Kincaid + Word_count,data = ECBD)
topic_structural_break_model_ECBD_STOXX <- lm(
  STOXX ~ (Finance_score  + Global_score + 
             Growth_score + Policy_score + Other_score +Prices_score) * Post_2008 + STOXXT1 + CPI + GDP + Flesch_Kincaid + Word_count,data = ECBD)
topic_structural_break_model_FEDD_VIX <- lm(
  VIX ~ (Finance_score  + Global_score + 
           Growth_score + Policy_score + Other_score +Prices_score) * Post_2008 + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count,data = FEDD)
topic_structural_break_model_FEDD_SP500 <- lm(
  SP500 ~ (Finance_score  + Global_score + 
             Growth_score + Policy_score + Other_score +Prices_score) * Post_2008 + SP500T1 + CPI + GDP + Flesch_Kincaid + Word_count,data = FEDD)

topic_structural_break_model_FEDM_SP500 <- lm(
  SP500 ~ (Finance_score  + Global_score + 
             Growth_score + Policy_score + Other_score +Prices_score) * Post_2008 + SP500T1 + CPI + GDP + Flesch_Kincaid + Word_count,data = FEDM)
summary(topic_structural_break_model_FEDM_SP500)
topic_structural_break_model_FEDM_VIX <- lm(
  VIX ~ (Finance_score  + Global_score + 
           Growth_score + Policy_score + Other_score +Prices_score) * Post_2008 + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count,data = FEDM)

(chow_test_ECBD_STOXX <- sctest(STOXX ~ Finance_score  + Global_score + 
                                  Growth_score + Policy_score + Other_score +Prices_score + STOXXT1 + CPI + GDP + Flesch_Kincaid + Word_count, 
                                type = "Chow", data = ECBD, 
                                breakpoint = which(as.Date(ECBD$Date) == as.Date("2008-01-01"))))
(chow_test_ECBD_VSTOXX <- sctest(VSTOXX ~ Finance_score  + Global_score + 
                                   Growth_score + Policy_score + Other_score +Prices_score + VSTOXXT1 + CPI + GDP + Flesch_Kincaid + Word_count, 
                                 type = "Chow", data = ECBD, 
                                 breakpoint = which(as.Date(ECBD$Date) == as.Date("2008-01-01"))))
(chow_test_FEDD_SP500 <- sctest(SP500 ~ Finance_score  + Global_score + 
                                  Growth_score + Policy_score + Other_score +Prices_score + SP500T1 + CPI + GDP + Flesch_Kincaid + Word_count, 
                                type = "Chow", data = FEDD, 
                                breakpoint = which(as.Date(ECBD$Date) == as.Date("2008-01-01"))))
(chow_test_FEDM_SP500 <- sctest(SP500 ~ Finance_score  + Global_score + 
                                  Growth_score + Policy_score + Other_score +Prices_score + SP500T1 + CPI + GDP + Flesch_Kincaid + Word_count, 
                                type = "Chow", data = FEDM, 
                                breakpoint = which(as.Date(ECBD$Date) == as.Date("2008-01-01"))))
(chow_test_FEDD_VIX <- sctest(VIX ~ Finance_score  + Global_score + 
                                Growth_score + Policy_score + Other_score +Prices_score + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count, 
                              type = "Chow", data = FEDD, 
                              breakpoint = which(as.Date(ECBD$Date) == as.Date("2008-01-01"))))
(chow_test_FEDM_VIX <- sctest(VIX ~ Finance_score  + Global_score + 
                                  Growth_score + Policy_score + Other_score +Prices_score + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count, 
                              type = "Chow", data = FEDM, 
                              breakpoint = which(as.Date(ECBD$Date) == as.Date("2008-01-01"))))

topic_pre_ECBD_VSTOXX <- lm(VSTOXX ~ Finance_score  + Global_score + 
                              Growth_score + Policy_score + Other_score +Prices_score + VSTOXXT1 + CPI + GDP + Flesch_Kincaid + Word_count, data = ECBD_pre_2013)
topic_pre_ECBD_STOXX <- lm(STOXX ~ Finance_score  + Global_score + 
                             Growth_score + Policy_score + Other_score +Prices_score + STOXXT1+  CPI + GDP + Flesch_Kincaid + Word_count, data = ECBD_pre_2013)
topic_pre_FEDD_VIX <- lm(VIX ~ Finance_score  + Global_score + 
                           Growth_score + Policy_score + Other_score +Prices_score + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count, data = FEDD_pre_2009)
topic_pre_FEDD_SP500 <- lm(SP500 ~ Finance_score  + Global_score + 
                             Growth_score + Policy_score + Other_score +Prices_score + SP500T1 +  CPI + GDP + Flesch_Kincaid + Word_count, data = FEDD_pre_2009)
topic_pre_FEDM_VIX <- lm(VIX ~ Finance_score  + Global_score + 
                           Growth_score + Policy_score + Other_score +Prices_score + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count, data = FEDM_pre_2009)
topic_pre_FEDM_SP500 <- lm(SP500 ~ Finance_score  + Global_score + 
                             Growth_score + Policy_score + Other_score +Prices_score + SP500T1 +  CPI + GDP + Flesch_Kincaid + Word_count, data = FEDM_pre_2009)

topic_post_ECBD_VSTOXX <- lm(VSTOXX ~ Finance_score  + Global_score + 
                               Growth_score + Policy_score + Other_score +Prices_score + VSTOXXT1 + CPI + GDP + Flesch_Kincaid + Word_count, data = ECBD_post_2013)
topic_post_ECBD_STOXX <- lm(STOXX ~ Finance_score  + Global_score + 
                              Growth_score + Policy_score + Other_score +Prices_score + STOXXT1+  CPI + GDP + Flesch_Kincaid + Word_count, data = ECBD_post_2013)
topic_post_FEDD_VIX <- lm(VIX ~ Finance_score  + Global_score + 
                            Growth_score + Policy_score + Other_score +Prices_score + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count, data = FEDD_post_2009)
topic_post_FEDD_SP500 <- lm(SP500 ~ Finance_score  + Global_score + 
                              Growth_score + Policy_score + Other_score +Prices_score + SP500T1 +  CPI + GDP + Flesch_Kincaid + Word_count, data = FEDD_post_2009)
topic_post_FEDM_VIX <- lm(VIX ~ Finance_score  + Global_score + 
                            Growth_score + Policy_score + Other_score +Prices_score + VIXT1 + CPI + GDP + Flesch_Kincaid + Word_count, data = FEDM_post_2009)
topic_post_FEDM_SP500 <- lm(SP500 ~ Finance_score  + Global_score + 
                              Growth_score + Policy_score + Other_score +Prices_score + SP500T1 +  CPI + GDP + Flesch_Kincaid + Word_count, data = FEDM_post_2009)

############################################### Visual inspection ###############################################
plot(ECBD$Standardized_score,ECBD$VSTOXX)
abline(a = baselinemodel_ECBD_VSTOXX$coefficients [1] , b =baselinemodel_ECBD_VSTOXX$coefficients[2] , col = "red", lwd = 2)
plot(ECBD$Standardized_score,ECBD$STOXX)
abline(a = baselinemodel_ECBD_STOXX$coefficients [1] , b =baselinemodel_ECBD_STOXX$coefficients[2] , col = "red", lwd = 2)
plot(ECBM$Standardized_score,ECBM$VSTOXX)
abline(a = baselinemodel_ECBM_VSTOXX$coefficients [1] , b =baselinemodel_ECBM_VSTOXX$coefficients[2] , col = "red", lwd = 2)
plot(ECBM$Standardized_score,ECBM$STOXX)
abline(a = baselinemodel_ECBM_STOXX$coefficients [1] , b =baselinemodel_ECBM_STOXX$coefficients[2] , col = "red", lwd = 2)

plot(FEDD$Standardized_score,FEDD$VIX)
abline(a = baselinemodel_FEDD_VIX$coefficients [1] , b =baselinemodel_FEDD_VIX$coefficients[2] , col = "red", lwd = 2)
plot(FEDD$Standardized_score,FEDD$SP500)
abline(a = baselinemodel_FEDD_SP500$coefficients [1] , b =baselinemodel_FEDD_SP500$coefficients[2] , col = "red", lwd = 2)
plot(FEDM$Standardized_score,FEDM$VIX)
abline(a = baselinemodel_FEDM_VIX$coefficients [1] , b =baselinemodel_FEDM_VIX$coefficients[2] , col = "red", lwd = 2)
plot(FEDM$Standardized_score,FEDM$SP500)
abline(a = baselinemodel_FEDM_SP500$coefficients [1] , b =baselinemodel_FEDM_SP500$coefficients[2] , col = "red", lwd = 2)



# Load required packages
library(lmtest)
library(broom)
library(tibble)
library(purrr)

# List of models (as defined earlier)
models <- list(
  # Baseline models
  "Model 1 ECB Decisions and realized volatility" = baselinemodel_ECBD_STOXX,
  "Model 1 ECB Decisions and implied volatility" = baselinemodel_ECBD_VSTOXX,
  "Model 1 ECB Minutes and implied volatility" = baselinemodel_ECBM_VSTOXX,
  "Model 1 ECB Minutes and realized volatility" = baselinemodel_ECBM_STOXX,
  "Model 1 FED Decisions and implied volatility" = baselinemodel_FEDD_VIX,
  "Model 1 FED Decisions and realized volatility" = baselinemodel_FEDD_SP500,
  "Model 1 FED Minutes and implied volatility" = baselinemodel_FEDM_VIX,
  "Model 1 FED Minutes and realized volatility" = baselinemodel_FEDM_SP500,
  
  # Lagged models
  "Model 2 ECB Decisions and realized volatility" = laggedmodel_ECBD_STOXX,
  "Model 2 ECB Decisions and implied volatility" = laggedmodel_ECBD_VSTOXX,
  "Model 2 ECB Minutes and realized volatility" = laggedmodel_ECBM_STOXX,
  "Model 2 ECB Minutes and implied volatility" = laggedmodel_ECBM_VSTOXX,
  "Model 2 FED Decisions and implied volatility" = laggedmodel_FEDD_VIX,
  "Model 2 FED Decisions and realized volatility" = laggedmodel_FEDD_SP500,
  "Model 2 FED Minutes and implied volatility" = laggedmodel_FEDM_VIX,
  "Model 2 FED Minutes and realized volatility" = laggedmodel_FEDM_SP500,
  
  # Sentiment models
  "Model 3 ECB Decisions and realized volatility" = sentimentmodel_ECBD_STOXX,
  "Model 3 ECB Decisions and implied volatility" = sentimentmodel_ECBD_VSTOXX,
  "Model 3 ECB Minutes and realized volatility" = sentimentmodel_ECBM_STOXX,
  "Model 3 ECB Minutes and implied volatility" = sentimentmodel_ECBM_VSTOXX,
  "Model 3 FED Decisions and implied volatility" = sentimentmodel_FEDD_VIX,
  "Model 3 FED Decisions and realized volatility" = sentimentmodel_FEDD_SP500,
  "Model 3 FED Minutes and implied volatility" = sentimentmodel_FEDM_VIX,
  "Model 3 FED Minutes and realized volatility" = sentimentmodel_FEDM_SP500,
  
  # Uncertainty models
  "Model 4 ECB Decisions and realized volatility" = uncertainty_model_ECBD_STOXX,
  "Model 4 ECB Decisions and implied volatility" = uncertainty_model_ECBD_VSTOXX,
  "Model 4 ECB Minutes and realized volatility" = uncertainty_model_ECBM_STOXX,
  "Model 4 ECB Minutes and implied volatility" = uncertainty_model_ECBM_VSTOXX,
  "Model 4 FED Decisions and realized volatility" = uncertainty_model_FEDD_SP500,
  "Model 4 FED Decisions and implied volatility" = uncertainty_model_FEDD_VIX,
  "Model 4 FED Minutes and realized volatility" = uncertainty_model_FEDM_SP500,
  "Model 4 FED Minutes and implied volatility" = uncertainty_model_FEDM_VIX,
  
  # Readability models
  "Model 5 ECB Decisions and realized volatility" = readability_model_ECBD_STOXX,
  "Model 5 ECB Decisions and implied volatility" = readability_model_ECBD_VSTOXX,
  "Model 5 ECB Minutes and realized volatility" = readability_model_ECBM_STOXX,
  "Model 5 ECB Minutes and implied volatility" = readability_model_ECBM_VSTOXX,
  "Model 5 FED Decisions and realized volatility" = readability_model_FEDD_SP500,
  "Model 5 FED Decisions and implied volatility" = readability_model_FEDD_VIX,
  "Model 5 FED Minutes and realized volatility" = readability_model_FEDM_SP500,
  "Model 5 FED Minutes and implied volatility" = readability_model_FEDM_VIX,
  
  # Word count models
  "Model 6 ECB Decisions and realized volatility" = word_ECBD_STOXX,
  "Model 6 ECB Decisions and implied volatility" = word_ECBD_VSTOXX,
  "Model 6 ECB Minutes and realized volatility" = word_ECBM_STOXX,
  "Model 6 ECB Minutes and implied volatility" = word_ECBM_VSTOXX,
  "Model 6 FED Decisions and realized volatility" = word_FEDD_SP500,
  "Model 6 FED Decisions and implied volatility" = word_FEDD_VIX,
  "Model 6 FED Minutes and realized volatility" = word_FEDM_SP500,
  "Model 6 FED Minutes and implied volatility" = word_FEDM_VIX,
  
  
  # Break models
  "Model 7 ECB Decisions and realized volatility" = structural_break_model_ECBD_STOXX,
  "Model 7 ECB Decisions and implied volatility" = structural_break_model_ECBD_VSTOXX,
  "Model 7 FED Decisions and realized volatility" = structural_break_model_FEDD_SP500,
  "Model 7 FED Decisions and implied volatility" = structural_break_model_FEDD_VIX,
  "Model 7 FED Minutes and realized volatility" = structural_break_model_FEDM_SP500,
  "Model 7 FED Minutes and implied volatility" = structural_break_model_FEDM_VIX,
  
  # Pre-break
  "Model 8 ECB Decisions and realized volatility" = pre_ECBD_STOXX,
  "Model 8 ECB Decisions and implied volatility" = pre_ECBD_VSTOXX,
  "Model 8 FED Decisions and realized volatility" = pre_FEDD_SP500 ,
  "Model 8 FED Decisions and implied volatility" = pre_FEDD_VIX,
  "Model 8 FED Minutes and realized volatility" = pre_FEDM_SP500, 
  "Model 8 FED Minutes and implied volatility" = pre_FEDM_VIX ,
  
  # Post-break
  "Model 9 ECB Decisions and realized volatility" =  post_ECBD_STOXX ,
  "Model 9 ECB Decisions and implied volatility" = post_ECBD_VSTOXX,
  "Model 9 FED Decisions and realized volatility" = post_FEDD_SP500,
  "Model 9 FED Decisions and implied volatility" = post_FEDD_VIX ,
  "Model 9 FED Minutes and realized volatility" = post_FEDM_SP500 ,
  "Model 9 FED Minutes and implied volatility" = post_FEDM_VIX 
  
)


models_topics <- list(
  # Baseline models
  "Model 10 ECB Decisions and realized volatility" = topic_baselinemodel_ECBD_STOXX,
  "Model 10 ECB Decisions and implied volatility" = topic_baselinemodel_ECBD_VSTOXX,
  "Model 10 ECB Minutes and implied volatility" = topic_baselinemodel_ECBM_VSTOXX,
  "Model 10 ECB Minutes and realized volatility" = topic_baselinemodel_ECBM_STOXX,
  "Model 10 FED Decisions and implied volatility" = topic_baselinemodel_FEDD_VIX,
  "Model 10 FED Decisions and realized volatility" = topic_baselinemodel_FEDD_SP500,
  "Model 10 FED Minutes and implied volatility" = topic_baselinemodel_FEDM_VIX,
  "Model 10 FED Minutes and realized volatility" = topic_baselinemodel_FEDM_SP500,
  
  # Lagged models
  "Model 11 ECB Decisions and realized volatility" = topic_laggedmodel_ECBD_STOXX,
  "Model 11 ECB Decisions and implied volatility" = topic_laggedmodel_ECBD_VSTOXX,
  "Model 11 ECB Minutes and realized volatility" = topic_laggedmodel_ECBM_STOXX,
  "Model 11 ECB Minutes and implied volatility" = topic_laggedmodel_ECBM_VSTOXX,
  "Model 11 FED Decisions and implied volatility" = topic_laggedmodel_FEDD_VIX,
  "Model 11 FED Decisions and realized volatility" = topic_laggedmodel_FEDD_SP500,
  "Model 11 FED Minutes and implied volatility" = topic_laggedmodel_FEDM_VIX,
  "Model 11 FED Minutes and realized volatility" = topic_laggedmodel_FEDM_SP500,
  
  # Sentiment models
  "Model 12 ECB Decisions and realized volatility" = topic_sentimentmodel_ECBD_STOXX,
  "Model 12 ECB Decisions and implied volatility" = topic_sentimentmodel_ECBD_VSTOXX,
  "Model 12 ECB Minutes and realized volatility" = topic_sentimentmodel_ECBM_STOXX,
  "Model 12 ECB Minutes and implied volatility" = topic_sentimentmodel_ECBM_VSTOXX,
  "Model 12 FED Decisions and implied volatility" = topic_sentimentmodel_FEDD_VIX,
  "Model 12 FED Decisions and realized volatility" = topic_sentimentmodel_FEDD_SP500,
  "Model 12 FED Minutes and implied volatility" = topic_sentimentmodel_FEDM_VIX,
  "Model 12 FED Minutes and realized volatility" = topic_sentimentmodel_FEDM_SP500,
  
  # Uncertainty models
  "Model 13 ECB Decisions and realized volatility" = topic_uncertainty_model_ECBD_STOXX,
  "Model 13 ECB Decisions and implied volatility" = topic_uncertainty_model_ECBD_VSTOXX,
  "Model 13 ECB Minutes and realized volatility" = topic_uncertainty_model_ECBM_STOXX,
  "Model 13 ECB Minutes and implied volatility" = topic_uncertainty_model_ECBM_VSTOXX,
  "Model 13 FED Decisions and realized volatility" = topic_uncertainty_model_FEDD_SP500,
  "Model 13 FED Decisions and implied volatility" = topic_uncertainty_model_FEDD_VIX,
  "Model 13 FED Minutes and realized volatility" = topic_uncertainty_model_FEDM_SP500,
  "Model 13 FED Minutes and implied volatility" = topic_uncertainty_model_FEDM_VIX,
  
  # Readability models
  "Model 14 ECB Decisions and realized volatility" = topic_readability_model_ECBD_STOXX,
  "Model 14 ECB Decisions and implied volatility" = topic_readability_model_ECBD_VSTOXX,
  "Model 14 ECB Minutes and realized volatility" = topic_readability_model_ECBM_STOXX,
  "Model 14 ECB Minutes and implied volatility" = topic_readability_model_ECBM_VSTOXX,
  "Model 14 FED Decisions and realized volatility" = topic_readability_model_FEDD_SP500,
  "Model 14 FED Decisions and implied volatility" = topic_readability_model_FEDD_VIX,
  "Model 14 FED Minutes and realized volatility" = topic_readability_model_FEDM_SP500,
  "Model 14 FED Minutes and implied volatility" = topic_readability_model_FEDM_VIX,
  
  # Word count models
  "Model 15 ECB Decisions and realized volatility" = topic_word_ECBD_STOXX,
  "Model 15 ECB Decisions and implied volatility" = topic_word_ECBD_VSTOXX,
  "Model 15 ECB Minutes and realized volatility" = topic_word_ECBM_STOXX,
  "Model 15 ECB Minutes and implied volatility" = topic_word_ECBM_VSTOXX,
  "Model 15 FED Decisions and realized volatility" = topic_word_FEDD_SP500,
  "Model 15 FED Decisions and implied volatility" = topic_word_FEDD_VIX,
  "Model 15 FED Minutes and realized volatility" = topic_word_FEDM_SP500,
  "Model 15 FED Minutes and implied volatility" = topic_word_FEDM_VIX,
  
  # Break models
  "Model 16 ECB Decisions and realized volatility" = topic_structural_break_model_ECBD_STOXX,
  "Model 16 ECB Decisions and implied volatility" = topic_structural_break_model_ECBD_VSTOXX,
  "Model 16 FED Decisions and realized volatility" = topic_structural_break_model_FEDD_SP500,
  "Model 16 FED Decisions and implied volatility" = topic_structural_break_model_FEDD_VIX,
  "Model 16 FED Minutes and realized volatility" = topic_structural_break_model_FEDM_SP500,
  "Model 16 FED Minutes and implied volatility" = topic_structural_break_model_FEDM_VIX,
  # Pre-break
  "Model 17 ECB Decisions and realized volatility" = topic_pre_ECBD_STOXX,
  "Model 17 ECB Decisions and implied volatility" = topic_pre_ECBD_VSTOXX,
  "Model 17 FED Decisions and realized volatility" = topic_pre_FEDD_SP500 ,
  "Model 17 FED Decisions and implied volatility" = topic_pre_FEDD_VIX,
  "Model 17 FED Minutes and realized volatility" = topic_pre_FEDM_SP500, 
  "Model 17 FED Minutes and implied volatility" = topic_pre_FEDM_VIX ,
  
  # Post-break
  "Model 18 ECB Decisions and realized volatility" =  topic_post_ECBD_STOXX ,
  "Model 18 ECB Decisions and implied volatility" = topic_post_ECBD_VSTOXX,
  "Model 18 FED Decisions and realized volatility" = topic_post_FEDD_SP500,
  "Model 18 FED Decisions and implied volatility" = topic_post_FEDD_VIX ,
  "Model 18 FED Minutes and realized volatility" = topic_post_FEDM_SP500 ,
  "Model 18 FED Minutes and implied volatility" = topic_post_FEDM_VIX 
)
perform_tests <- function(model, model_name) {
  library(car)
  library(lmtest)
  
  # Breusch-Pagan test for heteroskedasticity
  hetero_test <- tryCatch(
    bptest(model),
    error = function(e) NA
  )
  
  # Durbin-Watson test for autocorrelation
  auto_test <- tryCatch(
    dwtest(model),
    error = function(e) NA
  )
  
  tibble(
    Model = model_name,
    BP_statistic = ifelse(is.na(hetero_test), NA, hetero_test$statistic),
    BP_p_value = ifelse(is.na(hetero_test), NA, hetero_test$p.value),
    BP_result = ifelse(is.na(hetero_test), "Test Failed", ifelse(hetero_test$p.value > 0.05, "Homoskedastic", "Heteroskedastic")),
    DW_statistic = ifelse(is.na(auto_test), NA, auto_test$statistic),
    DW_p_value = ifelse(is.na(auto_test), NA, auto_test$p.value),
    DW_result = ifelse(is.na(auto_test), "Test Failed", ifelse(auto_test$p.value > 0.05, "Not autocorrelated", "Autocorrelated"))
  )
}


# Apply the tests to all models and collect results
results <- imap_dfr(models, ~ perform_tests(.x, .y))  # Use `imap_dfr` to include names from the list
results_topics <- imap_dfr(models_topics, ~ perform_tests(.x, .y))  # Use `imap_dfr` to include names from the list

library(kableExtra)

# Generate the LaTeX code for the table
latex_table <- results %>%
  kable("latex", booktabs = TRUE, digits = 3, align = "c", caption = "Heteroskedasticity and Autocorrelation Test Results") %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

# Print the LaTeX code
cat(latex_table)


# Generate the LaTeX code for the table
latex_table_topics <- results_topics %>%
  kable("latex", booktabs = TRUE, digits = 3, align = "c", caption = "Heteroskedasticity and Autocorrelation Test Results") %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

# Print the LaTeX code
cat(latex_table_topics)

library(lmtest)
library(sandwich)
library(dplyr)

# Function to adjust model standard errors
adjust_model <- function(model, heteroskedastic, autocorrelated) {
  if (autocorrelated) {
    # Apply HAC adjustment
    coeftest(model, vcov = NeweyWest(model, lag = NULL, prewhite = FALSE))
  } else if (heteroskedastic) {
    # Apply HC1 adjustment
    coeftest(model, vcov = vcovHC(model, type = "HC1"))
  } else {
    # No adjustment needed
    coeftest(model)
  }
}

adjusted_models <- imap(models, function(model, model_name) {
  # Get test results for this model
  test_results <- results %>% filter(Model == model_name) %>% slice(1)  # Ensure only one row
  
  # Extract the heteroskedastic and autocorrelated flags
  heteroskedastic <- test_results$BP_result == "Heteroskedastic"
  autocorrelated <- test_results$DW_result == "Autocorrelated"
  
  # Adjust the model
  list(
    model = model,
    adjusted = adjust_model(model, heteroskedastic, autocorrelated)
  )
})




adjusted_models_topics <- imap(models_topics, function(model, model_name) {
  # Get test results for this model
  test_results <- results_topics %>%
    filter(Model == model_name) %>%
    slice(1)  # Ensure only one row is used per model
  
  # Check for missing or invalid results
  heteroskedastic <- ifelse(is.na(test_results$BP_result), FALSE, test_results$BP_result == "Heteroskedastic")
  autocorrelated <- ifelse(is.na(test_results$DW_result), FALSE, test_results$DW_result == "Autocorrelated")
  
  # Ensure the results are logical scalars
  if (length(heteroskedastic) > 1 || length(autocorrelated) > 1) {
    stop("Test results contain more than one value. Ensure unique filtering for each model.")
  }
  
  # Adjust the model
  list(
    model = model,
    adjusted = adjust_model(model, heteroskedastic, autocorrelated)
  )
})



# Define topics
topics <- c(
  "ECB Decisions and realized volatility",
  "ECB Decisions and implied volatility",
  "ECB Minutes and realized volatility",
  "ECB Minutes and implied volatility",
  "FED Decisions and realized volatility",
  "FED Decisions and implied volatility",
  "FED Minutes and realized volatility",
  "FED Minutes and implied volatility"
)

# Group models by topic
models_by_topic <- lapply(topics, function(topic) {
  adjusted_models[names(adjusted_models) %>% grepl(topic, .)]
})

# Group models by topic
models_by_topic_topics <- lapply(topics, function(topic) {
  adjusted_models_topics[names(adjusted_models_topics) %>% grepl(topic, .)]
})

library(stargazer)

# Generate tables for each topic
for (i in seq_along(topics)) {
  topic_name <- topics[i]
  models <- models_by_topic[[i]]
  
  # Extract the adjusted models
  adjusted_models_list <- lapply(models, function(x) x$adjusted)
  
  # Extract F-test statistics and p-values
  f_stats <- sapply(models, function(m) summary(m$model)$fstatistic)
  f_stat_values <- sapply(f_stats, function(f) f[1])  # F-statistic
  f_df1 <- sapply(f_stats, function(f) f[2])         # Degrees of freedom 1
  f_df2 <- sapply(f_stats, function(f) f[3])         # Degrees of freedom 2
  f_p_values <- sapply(models, function(m) {
    f_stat <- summary(m$model)$fstatistic
    pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
  })  # F-test p-value
  
  # Create the LaTeX table
  stargazer(
    adjusted_models_list,
    type = "latex",
    title = paste("Results for", topic_name),
    omit = "Constant",
    omit.labels = NULL, # Prevent "Omitted" label from appearing
    add.lines = list(
      c("Observations", sapply(models, function(m) length(residuals(m$model)))),
      c("R-squared", sprintf("%.3f", sapply(models, function(m) summary(m$model)$r.squared))),
      c("Adjusted R-squared", sprintf("%.3f", sapply(models, function(m) summary(m$model)$adj.r.squared))),
      c("F-statistic", sprintf("%.3f", f_stat_values)),
      c("F-test p-value", sprintf("%.3f", f_p_values))
    ),
    digits = 3,
    notes = "Models employ robust standard errors as necessary.(Detailed in Appendix)",
    notes.align = "r"  # Align notes to the left
  )
}


# Generate tables for each topic
for (i in seq_along(topics)) {
  topic_name <- topics[i]
  models <- models_by_topic_topics[[i]]
  
  # Extract the adjusted models
  adjusted_models_list_topics <- lapply(models, function(x) x$adjusted)
  
  # Extract F-test statistics and p-values
  f_stats <- sapply(models, function(m) summary(m$model)$fstatistic)
  f_stat_values <- sapply(f_stats, function(f) f[1])  # F-statistic
  f_df1 <- sapply(f_stats, function(f) f[2])         # Degrees of freedom 1
  f_df2 <- sapply(f_stats, function(f) f[3])         # Degrees of freedom 2
  f_p_values <- sapply(models, function(m) {
    f_stat <- summary(m$model)$fstatistic
    pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
  })  # F-test p-value
  
  # Create the LaTeX table
  stargazer(
    adjusted_models_list_topics,
    type = "latex",
    title = paste("Results for", topic_name),
    omit = "Constant",
    omit.labels = NULL, # Prevent "Omitted" label from appearing
    add.lines = list(
      c("Observations", sapply(models, function(m) length(residuals(m$model)))),
      c("R-squared", sprintf("%.3f", sapply(models, function(m) summary(m$model)$r.squared))),
      c("Adjusted R-squared", sprintf("%.3f", sapply(models, function(m) summary(m$model)$adj.r.squared))),
      c("F-statistic", sprintf("%.3f", f_stat_values)),
      c("F-test p-value", sprintf("%.3f", f_p_values))
    ),
    digits = 3,
    notes = "Models employ robust standard errors as necessary.",
    notes.align = "r"  # Align notes to the right
  )
}










scale_model <- function(model, scale_factor) {
  # Extract current coefficients and scale them
  scaled_coefficients <- coef(model) * scale_factor
  
  # Extract standard errors and scale them
  model_summary <- summary(model)
  coefs <- coef(model_summary)
  scaled_standard_errors <- coefs[, 2] * scale_factor  # Scale standard errors
  
  # Create a new table with scaled coefficients and standard errors
  scaled_table <- cbind(
    Estimate = scaled_coefficients,
    `Std. Error` = scaled_standard_errors,
    `t value` = scaled_coefficients / scaled_standard_errors,
    `Pr(>|t|)` = coefs[, 4]
  )
  
  # Store the scaled table in the model's attributes for use in stargazer
  attr(model, "scaled_coefficients") <- scaled_table
  return(model)
}
get_scaled_models_by_topic <- function(topic, models, scale_factor) {
  # Filter models matching the topic
  filtered_models <- models[grepl(topic, names(models))]
  
  # Scale each model
  scaled_models <- lapply(filtered_models, function(model) {
    if (inherits(model, "lm")) {
      scale_model(model, scale_factor)
    } else {
      stop(paste("Invalid model structure for topic:", topic))
    }
  })
  
  return(scaled_models)
}

generate_latex_table <- function(scaled_models, topic_name) {
  # Extract scaled coefficients for each model
  scaled_coeffs <- lapply(scaled_models, function(model) {
    attr(model, "scaled_coefficients")
  })
  
  # Use stargazer to generate the table
  stargazer(
    scaled_models,
    coef = lapply(scaled_coeffs, function(x) x[, "Estimate"]),
    se = lapply(scaled_coeffs, function(x) x[, "Std. Error"]),
    type = "latex",
    title = paste("Results for", topic_name, "(Scaled Coefficients and Standard Errors)"),
    omit = "Constant",
    digits = 3,
    notes = paste("Coefficients and standard errors have been scaled by a factor of 10^14 for", topic_name, "models."),
    notes.align = "r"
  )
}



# Topics of interest
topics_of_interest <- c(
  "ECB Minutes and realized volatility",
  "ECB Minutes and implied volatility",
  "FED Minutes and realized volatility",
  "FED Minutes and implied volatility"
)

# Scale factor
scale_factor <- 1e14

# Generate tables for each topic
for (topic_name in topics_of_interest) {
  scaled_models <- get_scaled_models_by_topic(topic_name, models_topics, scale_factor)
  
  if (length(scaled_models) > 0) {
    generate_latex_table(scaled_models, topic_name)
  } else {
    cat(paste("No models found for topic:", topic_name, "\n"))
  }
}


























# Load necessary library
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Extract coefficients and confidence intervals for models
extract_model_info <- function(model, label, source) {
  summary_model <- summary(model)
  coef <- summary_model$coefficients["Standardized_score", "Estimate"]
  se <- summary_model$coefficients["Standardized_score", "Std. Error"]
  lower_ci <- coef - 1.645 * se
  upper_ci <- coef + 1.645 * se
  data.frame(
    Model = label,
    Coefficient = coef,
    Lower_CI = lower_ci,
    Upper_CI = upper_ci,
    Source = source
  )
}

# Create a list of models and their labels/sources
# Realized Volatility Models
models_realized_volatility <- list(
  # Baseline models
  baselinemodel_ECBD_STOXX = list(model = baselinemodel_ECBD_STOXX, label = "1", source = "ECB Decisions"),
  baselinemodel_ECBM_STOXX = list(model = baselinemodel_ECBM_STOXX, label = "1", source = "ECB Minutes"),
  baselinemodel_FEDD_SP500 = list(model = baselinemodel_FEDD_SP500, label = "1", source = "FOMC Decisions"),
  baselinemodel_FEDM_SP500 = list(model = baselinemodel_FEDM_SP500, label = "1", source = "FOMC Minutes"),
  
  # Lagged models
  laggedmodel_ECBD_STOXX = list(model = laggedmodel_ECBD_STOXX, label = "2", source = "ECB Decisions"),
  laggedmodel_ECBM_STOXX = list(model = laggedmodel_ECBM_STOXX, label = "2", source = "ECB Minutes"),
  laggedmodel_FEDD_SP500 = list(model = laggedmodel_FEDD_SP500, label = "2", source = "FOMC Decisions"),
  laggedmodel_FEDM_SP500 = list(model = laggedmodel_FEDM_SP500, label = "2", source = "FOMC Minutes"),
  
  # Sentiment models
  sentimentmodel_ECBD_STOXX = list(model = sentimentmodel_ECBD_STOXX, label = "3", source = "ECB Decisions"),
  sentimentmodel_ECBM_STOXX = list(model = sentimentmodel_ECBM_STOXX, label = "3", source = "ECB Minutes"),
  sentimentmodel_FEDD_SP500 = list(model = sentimentmodel_FEDD_SP500, label = "3", source = "FOMC Decisions"),
  sentimentmodel_FEDM_SP500 = list(model = sentimentmodel_FEDM_SP500, label = "3", source = "FOMC Minutes"),
  
  # Uncertainty models
  uncertainty_model_ECBD_STOXX = list(model = uncertainty_model_ECBD_STOXX, label = "4", source = "ECB Decisions"),
  uncertainty_model_ECBM_STOXX = list(model = uncertainty_model_ECBM_STOXX, label = "4", source = "ECB Minutes"),
  uncertainty_model_FEDD_SP500 = list(model = uncertainty_model_FEDD_SP500, label = "4", source = "FOMC Decisions"),
  uncertainty_model_FEDM_SP500 = list(model = uncertainty_model_FEDM_SP500, label = "4", source = "FOMC Minutes"),
  
  # Readability models
  readability_model_ECBD_STOXX = list(model = readability_model_ECBD_STOXX, label = "5", source = "ECB Decisions"),
  readability_model_ECBM_STOXX = list(model = readability_model_ECBM_STOXX, label = "5", source = "ECB Minutes"),
  readability_model_FEDD_SP500 = list(model = readability_model_FEDD_SP500, label = "5", source = "FOMC Decisions"),
  readability_model_FEDM_SP500 = list(model = readability_model_FEDM_SP500, label = "5", source = "FOMC Minutes"),
  
  # Word count models
  word_ECBD_STOXX = list(model = word_ECBD_STOXX, label = "6", source = "ECB Decisions"),
  word_ECBM_STOXX = list(model = word_ECBM_STOXX, label = "6", source = "ECB Minutes"),
  word_FEDD_SP500 = list(model = word_FEDD_SP500, label = "6", source = "FOMC Decisions"),
  word_FEDM_SP500 = list(model = word_FEDM_SP500, label = "6", source = "FOMC Minutes"),
  
  
  # Structural break models
  structural_break_model_ECBD_STOXX = list(model = structural_break_model_ECBD_STOXX, label = "7", source = "ECB Decisions"),
  structural_break_model_FEDD_SP500 = list(model = structural_break_model_FEDD_SP500, label = "7", source = "FOMC Decisions"),
  structural_break_model_FEDM_SP500 = list(model = structural_break_model_FEDM_SP500, label = "7", source = "FOMC Minutes"),
  
  # Pre/Post models
  pre_ECBD_STOXX = list(model = pre_ECBD_STOXX, label = "8", source = "ECB Decisions"),
  pre_FEDD_SP500 = list(model = pre_FEDD_SP500, label = "8", source = "FOMC Decisions"),
  pre_FEDM_SP500 = list(model = pre_FEDM_SP500, label = "8", source = "FOMC Minutes"),
  post_ECBD_STOXX = list(model = post_ECBD_STOXX, label = "9", source = "ECB Decisions"),
  post_FEDD_SP500 = list(model = post_FEDD_SP500, label = "9", source = "FOMC Decisions"),
  post_FEDM_SP500 = list(model = post_FEDM_SP500, label = "9", source = "FOMC Minutes")
)

# Implied Volatility Models
models_implied_volatility <- list(
  # Baseline models
  baselinemodel_ECBD_VSTOXX = list(model = baselinemodel_ECBD_VSTOXX, label = "1", source = "ECB Decisions"),
  baselinemodel_ECBM_VSTOXX = list(model = baselinemodel_ECBM_VSTOXX, label = "1", source = "ECB Minutes"),
  baselinemodel_FEDD_VIX = list(model = baselinemodel_FEDD_VIX, label = "1", source = "FOMC Decisions"),
  baselinemodel_FEDM_VIX = list(model = baselinemodel_FEDM_VIX, label = "1", source = "FOMC Minutes"),
  
  # Lagged models
  laggedmodel_ECBD_VSTOXX = list(model = laggedmodel_ECBD_VSTOXX, label = "2", source = "ECB Decisions"),
  laggedmodel_ECBM_VSTOXX = list(model = laggedmodel_ECBM_VSTOXX, label = "2", source = "ECB Minutes"),
  laggedmodel_FEDD_VIX = list(model = laggedmodel_FEDD_VIX, label = "2", source = "FOMC Decisions"),
  laggedmodel_FEDM_VIX = list(model = laggedmodel_FEDM_VIX, label = "2", source = "FOMC Minutes"),
  
  # Sentiment models
  sentimentmodel_ECBD_VSTOXX = list(model = sentimentmodel_ECBD_VSTOXX, label = "3", source = "ECB Decisions"),
  sentimentmodel_ECBM_VSTOXX = list(model = sentimentmodel_ECBM_VSTOXX, label = "3", source = "ECB Minutes"),
  sentimentmodel_FEDD_VIX = list(model = sentimentmodel_FEDD_VIX, label = "3", source = "FOMC Decisions"),
  sentimentmodel_FEDM_VIX = list(model = sentimentmodel_FEDM_VIX, label = "3", source = "FOMC Minutes"),
  
  # Uncertainty models
  uncertainty_model_ECBD_VSTOXX = list(model = uncertainty_model_ECBD_VSTOXX, label = "4", source = "ECB Decisions"),
  uncertainty_model_ECBM_VSTOXX = list(model = uncertainty_model_ECBM_VSTOXX, label = "4", source = "ECB Minutes"),
  uncertainty_model_FEDD_VIX = list(model = uncertainty_model_FEDD_VIX, label = "4", source = "FOMC Decisions"),
  uncertainty_model_FEDM_VIX = list(model = uncertainty_model_FEDM_VIX, label = "4", source = "FOMC Minutes"),
  
  # Readability models
  readability_model_ECBD_VSTOXX = list(model = readability_model_ECBD_VSTOXX, label = "5", source = "ECB Decisions"),
  readability_model_ECBM_VSTOXX = list(model = readability_model_ECBM_VSTOXX, label = "5", source = "ECB Minutes"),
  readability_model_FEDD_VIX = list(model = readability_model_FEDD_VIX, label = "5", source = "FOMC Decisions"),
  readability_model_FEDM_VIX = list(model = readability_model_FEDM_VIX, label = "5", source = "FOMC Minutes"),
  # Word count models
  word_ECBD_VSTOXX = list(model = word_ECBD_VSTOXX, label = "6", source = "ECB Decisions"),
  word_ECBM_VSTOXX = list(model = word_ECBM_VSTOXX, label = "6", source = "ECB Minutes"),
  word_FEDD_VIX = list(model = word_FEDD_VIX, label = "6", source = "FOMC Decisions"),
  word_FEDM_VIX = list(model = word_FEDM_VIX, label = "6", source = "FOMC Minutes"),
  

  # Structural break models
  structural_break_model_ECBD_VSTOXX = list(model = structural_break_model_ECBD_VSTOXX, label = "7", source = "ECB Decisions"),
  structural_break_model_FEDD_VIX = list(model = structural_break_model_FEDD_VIX, label = "7", source = "FOMC Decisions"),
  structural_break_model_FEDM_VIX = list(model = structural_break_model_FEDM_VIX, label = "7", source = "FOMC Minutes"),
  
  # Pre/Post models
  pre_ECBD_VSTOXX = list(model = pre_ECBD_VSTOXX, label = "8", source = "ECB Decisions"),
  pre_FEDD_VIX = list(model = pre_FEDD_VIX, label = "8", source = "FOMC Decisions"),
  pre_FEDM_VIX = list(model = pre_FEDM_VIX, label = "8", source = "FOMC Minutes"),
  post_ECBD_VSTOXX = list(model = post_ECBD_VSTOXX, label = "9", source = "ECB Decisions"),
  post_FEDD_VIX = list(model = post_FEDD_VIX, label = "9", source = "FOMC Decisions"),
  post_FEDM_VIX = list(model = post_FEDM_VIX, label = "9", source = "FOMC Minutes")
)


# Combine data from all models
model_data_realized_volatility <- do.call(rbind, lapply(models_realized_volatility, function(x) {
  extract_model_info(x$model, x$label, x$source)
}))

model_data_implied_volatility <- do.call(rbind, lapply(models_implied_volatility, function(x) {
  extract_model_info(x$model, x$label, x$source)
}))

# Plotting the coefficients and confidence intervals
ggplot(model_data_realized_volatility, aes(x = Model, y = Coefficient, color = Source)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "Regression Model",
    y = "Coefficient Value",
    color = "Source"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c(
      "ECB Decisions" = "#1560bd",
      "ECB Minutes" = "#db0004",
      "FOMC Decisions" = "black",
      "FOMC Minutes" = "gray"
    )
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, family = "Times", face = "bold"),
        axis.title = element_text(size = 28, family = "Times"),
        axis.text = element_text(size = 24, family = "Times", hjust = 0.5),
        legend.text = element_text(size = 28, family = "Times"),
        legend.title = element_blank(),
        legend.position = "bottom",                        # Place legend below
        panel.grid.major.x = element_blank(),               # Remove gridlines for cleaner look
        panel.background = element_blank(), # Remove panel background
        plot.background = element_blank()   # Remove plot background
  )+
  guides(fill = guide_legend(nrow = 1))  # Set legend to one row



# Plotting the coefficients and confidence intervals
ggplot(model_data_implied_volatility, aes(x = Model, y = Coefficient, color = Source)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "Regression Model",
    y = "Coefficient Value",
    color = "Source"
  )+
  theme_minimal() +
  scale_color_manual(
    values = c(
      "ECB Decisions" = "#1560bd",
      "ECB Minutes" = "#db0004",
      "FOMC Decisions" = "black",
      "FOMC Minutes" = "gray"
    )
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, family = "Times", face = "bold"),
        axis.title = element_text(size = 28, family = "Times"),
        axis.text = element_text(size = 24, family = "Times", hjust = 0.5),
        legend.text = element_text(size = 28, family = "Times"),
        legend.title = element_blank(),
        legend.position = "bottom",                        # Place legend below
        panel.grid.major.x = element_blank(),               # Remove gridlines for cleaner look
        panel.background = element_blank(), # Remove panel background
        plot.background = element_blank()   # Remove plot background
  )+
  guides(fill = guide_legend(nrow = 1))  # Set legend to one row
