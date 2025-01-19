######################################## 
#Load all the prepared files 
########################################
## score
Final_score_ECBD<-read.csv("Final_score_ECBD.csv")
Final_score_ECBD$Date <- as.Date(Final_score_ECBD$Date)
Final_score_ECBM<-read.csv("Final_score_ECBM.csv")
Final_score_ECBM$Date <- as.Date(Final_score_ECBM$Date)
Final_score_FEDD<-read.csv("Final_score_FEDD.csv")
Final_score_FEDD$Date <- as.Date(Final_score_FEDD$Date)
Final_score_FEDM<-read.csv("Final_score_FEDM.csv")
Final_score_FEDM$Date <- as.Date(Final_score_FEDM$Date)

Unstandardized_ECBD<-read.csv("Unstandardized_ECBD.csv")
Unstandardized_ECBD$Date <- as.Date(Unstandardized_ECBD$Date)
Unstandardized_ECBM<-read.csv("Unstandardized_ECBM.csv")
Unstandardized_ECBM$Date <- as.Date(Unstandardized_ECBM$Date)
Unstandardized_FEDD<-read.csv("Unstandardized_FEDD.csv")
Unstandardized_FEDD$Date <- as.Date(Unstandardized_FEDD$Date)
Unstandardized_FEDM<-read.csv("Unstandardized_FEDM.csv")
Unstandardized_FEDM$Date <- as.Date(Unstandardized_FEDM$Date)

Scores_ECBD<-read.csv("Scores_ECBD.csv")
Scores_ECBD$Date <- as.Date(Scores_ECBD$Date)
Scores_ECBM<-read.csv("Scores_ECBM.csv")
Scores_ECBM$Date <- as.Date(Scores_ECBM$Date)
Scores_FEDD<-read.csv("Scores_FEDD.csv")
Scores_FEDD$Date <- as.Date(Scores_FEDD$Date)
Scores_FEDM<-read.csv("Scores_FEDM.csv")
Scores_FEDM$Date <- as.Date(Scores_FEDM$Date)

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
### Interest rates
FED_rate_changes <- read.csv("FED_rate_changes.csv")
FED_rate_changes$Date<- as.Date(FED_rate_changes$Date)
ECB_rate_changes <- read.csv("ECB_rate_changes.csv")
ECB_rate_changes$Date<- as.Date(ECB_rate_changes$Date)


################################################################## VISUALIZATION ################################################################## 
####################################################
# Plot the interest rate changes with the score
####################################################
# Create the plot
library(extrafont)
windowsFonts(
  Arial = windowsFont("Arial"),
  Times = windowsFont("Times New Roman"),
  Calibri = windowsFont("Calibri")
)

# ECB
ggplot(data = ECB_rate_changes, aes(x = Date)) +
  geom_line(aes(y = Unstandardized.score, color = "Hawk-score"), size = 1) +
  geom_line(aes(y = Interest.Change, color = "Change in Deposit Facility Rate"), size = 1) +
  scale_y_continuous(limits = c(-0.3, 0.6), name = "Interest Rate Change and Hawk-score") +  # Add secondary axis 
  scale_color_manual(values = c("#1560bd", "#db0004")) + # Custom colors
  scale_x_date(
    date_breaks = "5 year",         # Increase frequency to every 6 months
    date_labels = "%Y"            # Format date labels as "Month Year"
  ) +
  labs(x = "Date") +
  theme_minimal() +
  theme(legend.position = "bottom")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, family = "Times", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "Times", face = "italic"),
    axis.title = element_text(size = 24, family = "Times"),
    axis.text = element_text(size = 24, family = "Times"),
    legend.text = element_text(size = 28, family = "Times"),
    legend.title = element_blank(),
    panel.grid = element_blank(),       # Remove gridlines
    axis.line = element_line(color = "black"),
    panel.background = element_blank(), # Remove panel background
    plot.background = element_blank()   # Remove plot background
  )

# FED
ggplot(data = FED_rate_changes, aes(x = Date)) +
  geom_line(aes(y = Unstandardized_score, color = "Hawk-score"), size = 1) +
  geom_line(aes(y = Change, color = "Change in Federal Funds Target Rate"), size = 1) +
  scale_y_continuous(
    name = "Interest Rate Change and Hawk-score"
  ) + 
  scale_color_manual(values = c("#1560bd", "#db0004")) + # Custom colors
  scale_x_date(
    date_breaks = "5 years",         # Increase frequency of date breaks
    date_labels = "%Y"              # Format date labels as "Year"
  ) +
  labs(
    x = "Date"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, family = "Times", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "Times", face = "italic"),
    axis.title = element_text(size = 24, family = "Times"),
    axis.text = element_text(size = 24, family = "Times"),
    legend.text = element_text(size = 28, family = "Times"),
    legend.title = element_blank(),
    panel.grid = element_blank(),       # Remove gridlines
    axis.line = element_line(color = "black"),
    panel.background = element_blank(), # Remove panel background
    plot.background = element_blank()   # Remove plot background
  )

####################################################
# Plot the total score and its topic drivers
####################################################
# calculate the fractions of the score, normalize to positive values when needed
Fraction_ECBM<-topics(Scores_ECBM)%>%
  mutate(Mean= abs(Mean))%>%
  mutate(Normalized = 100*(Mean/sum(Mean)))%>%
  rename("ECB Minutes" = Normalized)%>%
  dplyr::select(topic_keyword,`ECB Minutes`)
Fraction_ECBD<-topics(Scores_ECBD)%>%
  mutate(Mean = abs(Mean))%>%
  mutate(Normalized = 100*(Mean/sum(Mean)))%>%
  rename("ECB Decisions" = Normalized)%>%
  dplyr::select(topic_keyword,`ECB Decisions`)
Fraction_FEDD<-topics(Scores_FEDD)%>%
  mutate(Mean = abs(Mean))%>%
  mutate(Normalized = 100*(Mean/sum(Mean)))%>%
  rename("FOMC Decisions" = Normalized)%>%
  dplyr::select(topic_keyword,`FOMC Decisions`) 
Fraction_FEDM<-topics(Scores_FEDM)%>%
  mutate(Mean= abs(Mean))%>%
  mutate(Normalized = 100*(Mean/sum(Mean)))%>%
  rename("FOMC Minutes" = Normalized)%>%
  dplyr::select(topic_keyword,`FOMC Minutes`)

library(forcats)
# Prepare a merged data set for barchart use
Stacked_bar_chart_topics <-Fraction_ECBM%>%
  left_join(Fraction_FEDM, by = "topic_keyword")%>%
  left_join(Fraction_ECBD, by = "topic_keyword")%>%
  left_join(Fraction_FEDD, by = "topic_keyword")%>%
  pivot_longer(
    cols = -c(topic_keyword),        # Keep the `Topic` column (or equivalent column for topics)
    names_to = "Document",   # Convert document type columns into a single column
    values_to = "Value"      # Store values in the `Value` column
  )%>%
  mutate(topic_keyword = fct_reorder(topic_keyword, Value, .fun = sum))

Stacked_bar_chart_topics <- Fraction_ECBM %>%
  left_join(Fraction_FEDM, by = "topic_keyword") %>%
  left_join(Fraction_ECBD, by = "topic_keyword") %>%
  left_join(Fraction_FEDD, by = "topic_keyword") %>%
  pivot_longer(
    cols = -c(topic_keyword),        # Keep the `topic_keyword` column
    names_to = "Document",           # Convert document type columns into a single column
    values_to = "Value"              # Store values in the `Value` column
  ) %>%
  group_by(topic_keyword) %>%        # Group by topic to calculate the total
  mutate(total_value = sum(Value, na.rm = TRUE)) %>%  # Compute the total value per topic
  ungroup() %>%
  mutate(topic_keyword = fct_reorder(topic_keyword, total_value)) %>% # Reorder by the aggregated value
  dplyr::select(-total_value)               # Remove the temporary total column

##########################
# Plot stacked bar chart
#########################
ggplot(Stacked_bar_chart_topics, aes(x = Document, y = Value, fill = topic_keyword)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +  # Stacked bars
  geom_text(
   aes(
      label = ifelse(Value > 0.5, paste0(round(Value, 1), "%"), ""),  # Show labels only for values > 0.5%
    ),
    position = position_stack(vjust = 0.5),  # Automatically position labels in the middle of each segment
    color = "white", size = 3, family = "Times"  # Adjust text size and color
 ) +
  labs(
    x = "Document Type",
    y = "Proportion (%)",
    fill = "Topic"
  ) +
  scale_fill_manual(
    values = c(
      "Policy" = "#000058", 
      "Prices" = "#7bb6f2",   
      "Growth" = "#0053a0",   
      "Finance" = "#178fff",   
      "Global" = "#52acff",    
      "Other" = "#b4dbff"
    ))+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, family = "Times", face = "bold"),
    axis.title = element_text(size = 20, family = "Times"),
    axis.text = element_text(size = 16, family = "Times", hjust = 0.5),
    legend.text = element_text(size = 20, family = "Times"),
    legend.title = element_blank(),
    legend.position = "bottom",                        # Place legend below
    panel.grid.major.x = element_blank(),               # Remove gridlines for cleaner look
    panel.background = element_blank(), # Remove panel background
    plot.background = element_blank()   # Remove plot background
  )+
  guides(fill = guide_legend(nrow = 1))  # Set legend to one row

library(RColorBrewer)
library(colorspace)
blue_shades <- darken(brewer.pal(n = 6, name = "RdBu"), amount = 0.1) 


ggplot(Stacked_bar_chart_topics, aes(x = Document, y = Value, fill = topic_keyword)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +  # Stacked bars
  labs(
    title = "Topic Distribution",
    x = "Document Type",
    y = "Proportion",
    fill = "Topic"
  ) +
  #scale_fill_brewer(palette = "YlGnBu") + 
  scale_fill_manual(values = blue_shades) +# Use Blues palette from RColorBrewer
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, family = "Times", face = "bold"),
    axis.title = element_text(size = 12, family = "Times"),
    axis.text = element_text(size = 10, family = "Times", hjust = 0.5),
    legend.text = element_text(size = 12, family = "Times"),
    legend.title = element_blank(),
    legend.position = "bottom",                        # Place legend below
    panel.grid.major.x = element_blank(),               # Remove gridlines for cleaner look
    panel.background = element_blank(), # Remove panel background
    plot.background = element_blank()   # Remove plot background
  ) +
  guides(fill = guide_legend(nrow = 1))

############################################
# Hawkish and dovish explanation
############################################

FED_1 <- read.csv("C:/one drive/Počítač/UNI/Bachelors thesis/Data sets/DFEDTAR.csv")
colnames(FED_1)<-c("Date","Rate")
FED_2 <- read.csv("C:/one drive/Počítač/UNI/Bachelors thesis/Data sets/DFEDTARU.csv")
colnames(FED_2)<-c("Date","Rate")
FED_rates <- rbind(FED_1,FED_2)
FED_rates$Date <- as.Date(FED_rates$Date)

# Ensure continuous dates and fill missing rates
FED_rate_changes_Policy <- FED_rate_changes %>%
  left_join(FED_rates, by = "Date") %>% # Merge with FED_rates by Date
  mutate(
    Policy = case_when(
      Change > 0 ~ "Hawks",                                   # Assign "Hawks" if Change > 0
      Change < 0 ~ "Doves",                                   # Assign "Doves" if Change < 0
      TRUE ~ "Neutral"                                        # Assign "Neutral" if no change
    )
  ) %>%
  dplyr::select(Date, Rate, Policy)%>%                          # Select relevant columns
  filter(Date >= as.Date("2000-01-01") & Date < as.Date("2010-01-01"))%>%
  arrange(Date) %>%  # Ensure data is sorted by Date
  mutate(
    Rate_Change = Rate - lag(Rate, default = first(Rate)),  # Calculate rate changes
    Segment = cumsum(if_else(Policy == "Neutral" & lag(Rate_Change, default = 0) != Rate_Change, 1, 0)),  # Identify segments
    Policy = case_when(
      Policy == "Neutral" & Rate_Change > 0 ~ "Hawks",       # Increasing segment -> Hawks
      Policy == "Neutral" & Rate_Change < 0 ~ "Doves",       # Decreasing segment -> Doves
      TRUE ~ Policy                                          # Retain existing classification otherwise
    )
  )

# Create the plot
ggplot(FED_rate_changes_Policy, aes(x = Date, y = Rate, color = Policy, group = 1)) +
  # Step line with conditional thickness for Neutral policy
  geom_step(aes(size = ifelse(Policy == "Neutral", 0.5, 1.3))) +  
  # Custom colors for the policy types
  scale_color_manual(values = c("Hawks" = "#db0004", "Doves" = "#1560bd", "Neutral" = "gray")) +  
  # Customize the x-axis date breaks and format
  scale_x_date(
    date_breaks = "1 year",        # Show a tick for each year
    date_labels = "%Y"            # Display the year format (e.g., 2000, 2001)
  ) +
  # Use the size mapping directly without scaling
  scale_size_identity() +  
  # Add titles and labels
  labs(
    x = "Date",
    y = "Federal Funds Target Rate (%)",
    color = "Policy Type"
  ) +
  # Clean, minimalistic theme
  theme_minimal(base_size = 14) +  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, family = "Times", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "Times", face = "italic"),
    axis.title = element_text(size = 28, family = "Times"),
    axis.text = element_text(size = 24, family = "Times"),
    legend.text = element_text(size = 28, family = "Times"),
    legend.title = element_blank(),
    panel.grid = element_blank(),       # Remove gridlines
    axis.line = element_line(color = "black"),
    panel.background = element_blank(), # Remove panel background
    plot.background = element_blank()   # Remove plot background
  )



























# Plot the data
ggplot(data = Final_score_ECBD, aes(x = Date)) +
  geom_line(aes(y = Standardized_score, color = "Standardized Hawk-score"), size = 1) +
  geom_line(aes(y = Prices, color = "Prices"), size = 1) +
  geom_line(aes(y = Finance, color = "Finance"), size = 1) +
  geom_line(aes(y = Policy, color = "Policy"), size = 1) +
  geom_line(aes(y = Growth, color = "Growth"), size = 1) +
  geom_line(aes(y = Global, color = "Global"), size = 1) +
  geom_line(aes(y = Other, color = "Other"), size = 1) +
  scale_y_continuous(
    name = "Standardized Hawk-score"
  ) + 
  scale_color_manual(values = c("black","#191970","#1560bd","#6495ed","#007ba7","#1ca9c9", "#87ceeb")) + # Custom colors
  scale_x_date(
    date_breaks = "2 years",         # Increase frequency of date breaks
    date_labels = "%Y"              # Format date labels as "Year"
  ) +
  labs(
    title = "Standardized Hawk-score",
    subtitle = "ECB Decisions",
    caption = "Source: FRED",
    x = "Date"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 16, family = "Times", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "Times", face = "italic"),
    axis.title = element_text(size = 12, family = "Times"),
    axis.text = element_text(size = 10, family = "Times"),
    legend.text = element_text(size = 12, family = "Times"),
    legend.title = element_blank(),
    panel.grid = element_blank(),       # Remove gridlines
    axis.line = element_line(color = "black"),
    panel.background = element_blank(), # Remove panel background
    plot.background = element_blank()   # Remove plot background
  )



# Add a column to distinguish datasets
Final_score_FEDM$Source <- "FOMC Minutes"
Final_score_FEDD$Source <- "FOMC Decisions"
Final_score_ECBM$Source <- "ECB Minutes"
Final_score_ECBD$Source <- "ECB Decisions"


combined_scores<-  rbind(Final_score_FEDD,Final_score_FEDM,Final_score_ECBD,Final_score_ECBM)

# Filter FED datasets
fed_scores <- combined_scores %>%
  filter(Source %in% c("FOMC Minutes", "FOMC Decisions"))

# Filter ECB datasets
ecb_scores <- combined_scores %>%
  filter(Source %in% c("ECB Minutes", "ECB Decisions"))

ggplot(fed_scores, aes(x = Date, y = Standardized_score, color = Source)) +
  geom_line(size = 1) +  # Increase line thickness
  scale_x_date(
    date_breaks = "5 years",         # Increase frequency of date breaks
    date_labels = "%Y"              # Format date labels as "Year"
  ) +
  scale_color_manual(
    values = c(
      "FOMC Minutes" = "#db0004",  # Custom color for FOMC Minutes
      "FOMC Decisions" = "#1560bd" # Custom color for FOMC Decisions
    )
  ) +
  labs(
    x = "Date",
    y = "Standardized Score",
    color = "Source"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 16, family = "Times", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "Times", face = "italic"),
    axis.title = element_text(size = 28, family = "Times"),
    axis.text = element_text(size = 24, family = "Times"),
    legend.text = element_text(size = 28, family = "Times"),
    legend.title = element_blank(),
    panel.grid = element_blank(),       # Remove gridlines
    axis.line = element_line(color = "black"),
    panel.background = element_blank(), # Remove panel background
    plot.background = element_blank()   # Remove plot background
  )


ggplot(ecb_scores, aes(x = Date, y = Standardized_score, color = Source)) +
  geom_line(size = 1) +  # Increase line thickness
  scale_x_date(
    date_breaks = "5 years",         # Increase frequency of date breaks
    date_labels = "%Y"              # Format date labels as "Year"
  ) +
  scale_color_manual(
    values = c(
      "ECB Minutes" = "#db0004",  # Custom color for ECB Minutes
      "ECB Decisions" = "#1560bd" # Custom color for ECB Decisions
    )
  ) +
  labs(
    x = "Date",
    y = "Standardized Score",
    color = "Source"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 16, family = "Times", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "Times", face = "italic"),
    axis.title = element_text(size = 28, family = "Times"),
    axis.text = element_text(size = 24, family = "Times"),
    legend.text = element_text(size = 28, family = "Times"),
    legend.title = element_blank(),
    panel.grid = element_blank(),       # Remove gridlines
    axis.line = element_line(color = "black"),
    panel.background = element_blank(), # Remove panel background
    plot.background = element_blank()   # Remove plot background
  )

#################Volatility
# SP500
ggplot(volatility_SP500, aes(x = Date, y = SP500, color = "SP500")) +
  geom_line(size = 0.5) +  # Increase line thickness
  scale_x_date(
    date_breaks = "5 years",         # Increase frequency of date breaks
    date_labels = "%Y"              # Format date labels as "Year"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  scale_color_manual(values = c("#1560bd"))+
  labs(
    x = "Date",
    y = "Realized Volatility (%)",
    color = "Source"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, family = "Times", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "Times", face = "italic"),
    axis.title = element_text(size = 28, family = "Times"),
    axis.text = element_text(size = 24, family = "Times"),
    legend.text = element_text(size = 28, family = "Times"),
    legend.title = element_blank(),
    panel.grid = element_blank(),       # Remove gridlines
    axis.line = element_line(color = "black"),
    panel.background = element_blank(), # Remove panel background
    plot.background = element_blank()   # Remove plot background
  )


# VIX
ggplot(volatility_VIX, aes(x = Date, y = VIX, color = "VIX")) +
  geom_line(size = 0.5) +  # Increase line thickness
  scale_x_date(
    date_breaks = "2 years",         # Increase frequency of date breaks
    date_labels = "%Y"              # Format date labels as "Year"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  scale_color_manual(values = c("#1560bd"))+
  labs(,
    x = "Date",
    y = "Implied Volatility (%)",
    color = "Source"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, family = "Times", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "Times", face = "italic"),
    axis.title = element_text(size = 28, family = "Times"),
    axis.text = element_text(size = 24, family = "Times"),
    legend.text = element_text(size = 28, family = "Times"),
    legend.title = element_blank(),
    panel.grid = element_blank(),       # Remove gridlines
    axis.line = element_line(color = "black"),
    panel.background = element_blank(), # Remove panel background
    plot.background = element_blank()   # Remove plot background
  )


# STOXX
ggplot(volatility_STOXX, aes(x = Date, y = STOXX, color = "STOXX")) +
  geom_line(size = 0.5) +  # Increase line thickness
  scale_x_date(
    date_breaks = "2 years",         # Increase frequency of date breaks
    date_labels = "%Y"              # Format date labels as "Year"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  scale_color_manual(values = c("#1560bd"))+
  labs(
    x = "Date",
    y = "Realized Volatility (%)",
    color = "Source"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, family = "Times", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "Times", face = "italic"),
    axis.title = element_text(size = 28, family = "Times"),
    axis.text = element_text(size = 24, family = "Times"),
    legend.text = element_text(size = 28, family = "Times"),
    legend.title = element_blank(),
    panel.grid = element_blank(),       # Remove gridlines
    axis.line = element_line(color = "black"),
    panel.background = element_blank(), # Remove panel background
    plot.background = element_blank()   # Remove plot background
  )

# VSTOXX
ggplot(volatility_VSTOXX, aes(x = Date, y = VSTOXX, color = "VSTOXX")) +
  geom_line(size = 0.5) +  # Increase line thickness
  scale_x_date(
    date_breaks = "2 years",         # Increase frequency of date breaks
    date_labels = "%Y"              # Format date labels as "Year"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  scale_color_manual(values = c("#1560bd"))+
  labs(
    x = "Date",
    y = "Implied Volatility (%)",
    color = "Source"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, family = "Times", face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, family = "Times", face = "italic"),
    axis.title = element_text(size = 28, family = "Times"),
    axis.text = element_text(size = 24, family = "Times"),
    legend.text = element_text(size = 28, family = "Times"),
    legend.title = element_blank(),
    panel.grid = element_blank(),       # Remove gridlines
    axis.line = element_line(color = "black"),
    panel.background = element_blank(), # Remove panel background
    plot.background = element_blank()   # Remove plot background
  )
