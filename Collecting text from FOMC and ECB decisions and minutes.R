rm(list=ls())
# Set your working directory
setwd("C:/one drive/Počítač/UNI/Bachelors thesis/Cleaned files")
# Import all the functions
source("Functions.R")
# Required packages to download
library(rvest)
library(dplyr)
library(openxlsx)
library(readxl)
library(httr)
library(stringr)
library(xml2)
library(pdftools)

######################################################## Gather links from FOMC decisions and minutes########################################################
# Note: Links from ECB minutes and decisions were gathered manually and are provided in the Excel files ("ECB Decisions links" and "ECB Minutes links")
######################################################## Save links to FOMC decisions ########################################################
############################## Scrape FOMC Statements from 1994 to 2018  ##############################
decisions <- c()
years <- c()
#  Gather pages from 1994 until 2018
for (year in 1994:2018) {
  # URL for each year
  page_link <- paste0("https://www.federalreserve.gov/monetarypolicy/fomchistorical", year, ".htm")
  # Read the HTML content 
  page <- rvest::read_html(page_link)
  # Select relevant HTML Nodes
  headings <- page %>%
    html_nodes(xpath = "//h5 | //a")  # Capture headings and hyperlinks
  current_heading <- NA  # Initialize heading variable (The links (<a>) must be associated with the heading to determine relevance.)
  # Loop through Headings 
  for (node in headings) {  # Process Each Node
    if (html_name(node) == "h5") {  
      current_heading <- html_text(node, trim = TRUE)  # Extract the heading text and store it in current_heading
    } else if (html_name(node) == "a") {  # If it's a link proceed further
      if (grepl("Meeting", current_heading, ignore.case = TRUE)) { # Check whether the current_heading contains "Meeting"
        link_text <- html_text(node, trim = TRUE) # Extract the visible text of the link
        link <- html_attr(node, "href") # Extract the hyperlink reference (URL).
        # Filter for links where the link text contains exactly "Statement"
        if (grepl("^Statement$", link_text, ignore.case = TRUE)) {
          decisions <- c(decisions, link) # If it matches, append the link to the meetings list and the corresponding year to the years list.
          years <- c(years, year) 
        }
      }
    }
  }
}
# Ensure all gathered links in the meetings vector are complete URLs
base_url <- "https://www.federalreserve.gov" # Define the base URL for the Federal Reserve website
decisions <- ifelse(grepl("^http", decisions), decisions, paste0(base_url, decisions))
links <- data.frame(year = years, link = decisions, stringsAsFactors = FALSE) # save the links to a dataset

############################## Scrape FOMC Statements from 2024 to 2018 ##############################
# Use the main webpage with all the links (open the link for more understanding of the structure)
page_link1 <- paste0("https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm")
# Read the HTML content of the specified page
page <- read_html(page_link1)
#  Extract Links Matching a Specific Pattern (looking for ones with "statements")
statement_links <- page %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  grep("/monetarypolicy/files/monetary", ., value = TRUE)  # Filter for the specific link pattern
# Ensure all gathered links in the vector are complete URLs
base_url <- "https://www.federalreserve.gov"
statement_links <- ifelse(grepl("^http", statement_links), statement_links, paste0(base_url, statement_links))
# Save the links in a data frame and add the extracted year as a separate column
years <- sub(".*/monetary([0-9]{4}).*", "\\1", statement_links)
links_df1 <- data.frame(year = years, link = statement_links, stringsAsFactors = FALSE)
links_df1 <- links_df1[nrow(links_df1):1, ]
## Merge the two periods and create an excel file 
Statements <- rbind(links,links_df1)
write.xlsx(Statements, file = "FOMC Statements links.xlsx")

######################################################## Save links to FOMC minutes ########################################################
# Note: accessing the links for the minutes needs to be divided into several periods because of the difference in formatting
############################## Scrape FOMC Minutes from 1994 to 2006 ##############################
minutes <- c()
years1 <- c()
#  Gather pages from 1994 until 2006
for (year in 1994:2007) {
  # URL for each year
  page_link1 <- paste0("https://www.federalreserve.gov/monetarypolicy/fomchistorical", year, ".htm")
  # Read the HTML content 
  page1 <- read_html(page_link1)
  # Select only links from "Meetings"
  headings <- page1 %>%
    html_nodes(xpath = "//h5 | //a")  # Capture both headings and links
  current_heading <- NA  # Initialize heading variable
  # Loop through Headings 
  for (node in headings) {
    if (html_name(node) == "h5") {
      current_heading <- html_text(node, trim = TRUE)  # Extract the heading text
    } else if (html_name(node) == "a") {  # If it's a link and we are in the "Meeting" section
      if (grepl("Meeting", current_heading, ignore.case = TRUE)) {
        link_text <- html_text(node, trim = TRUE)
        url <- html_attr(node, "href")
        # Filter for links where the link text contains exactly "Statement"
        if (grepl("^Minutes$", link_text, ignore.case = TRUE)) {
          minutes <- c(minutes, url)
          years1 <- c(years1, year)  # Append the current year directly
        }
      }
    }
  }
}
# Clean up the links (ensure full URLs)
base_url <- "https://www.federalreserve.gov"
minutes <- ifelse(grepl("^http", minutes), minutes, paste0(base_url, minutes))
urls <- data.frame(year = years1, url = minutes, stringsAsFactors = FALSE)

############################## Scrape FOMC Minutes from 2007 to 2010 ##############################
minutes1 <- c()
years2 <- c()
#  Gather pages from 2007 until 2010
for (year in 2007:2010) {
  # URL for each year
  page_link1 <- paste0("https://www.federalreserve.gov/monetarypolicy/fomchistorical", year, ".htm")
  # Read the HTML content 
  page1 <- read_html(page_link1)
  # Select only links from "Meetings"
  headings <- page1 %>%
    html_nodes(xpath = "//h5 | //a")  # Capture both headings and links
  current_heading <- NA  # Initialize heading variable
  # Loop through Headings 
  for (node in headings) {
    if (html_name(node) == "h5") {
      current_heading <- html_text(node, trim = TRUE)  # Extract the heading text
    } else if (html_name(node) == "a") {  # If it's a link and we are in the "Meeting" section
      if (grepl("Meeting", current_heading, ignore.case = TRUE)) {
        link_text <- html_text(node, trim = TRUE)
        url <- html_attr(node, "href")
        # Ensure url is not NULL or empty
        if (!is.null(url) && url != "" && grepl("/monetarypolicy/fomc", url)) {
          minutes1 <- c(minutes1, url)
          years2 <- c(years2, year)  # Append the current year directly
        }
      }
    }
  }
} 
# Clean up the links (ensure full URLs)
base_url <- "https://www.federalreserve.gov"
minutes1 <- ifelse(grepl("^http", minutes1), minutes1, paste0(base_url, minutes1))
urls1 <- data.frame(year = years2, url = minutes1, stringsAsFactors = FALSE)

############################## Scrape FOMC Minutes from 2011 to 2018 ##############################
minutes2 <- c()
years3 <- c()
#  Gather pages from 2011 until 2018
for (year in 2011:2018) {
  # URL for each year
  page_link1 <- paste0("https://www.federalreserve.gov/monetarypolicy/fomchistorical", year, ".htm")
  # Read the HTML content 
  page1 <- read_html(page_link1)
  # Select only links from "Meetings"
  headings <- page1 %>%
    html_nodes(xpath = "//h5 | //a")  # Capture both headings and links
  current_heading <- NA  # Initialize heading variable
  # Loop through Headings 
  for (node in headings) {
    if (html_name(node) == "h5") {
      current_heading <- html_text(node, trim = TRUE)  # Extract the heading text
    } else if (html_name(node) == "a") {  # If it's a link and we are in the "Meeting" section
      if (grepl("Meeting", current_heading, ignore.case = TRUE)) {
        link_text <- html_text(node, trim = TRUE)
        url <- html_attr(node, "href")
        # Ensure url is not NULL or empty
        if (!is.null(url) && url != "" && grepl("/monetarypolicy/fomcminutes", url)) {
          minutes2 <- c(minutes2, url)
          years3 <- c(years3, year)  # Append the current year directly
        }
      }
    }
  }
} 
# Clean up the links (ensure full URLs)
base_url <- "https://www.federalreserve.gov"
minutes2 <- ifelse(grepl("^http", minutes2), minutes2, paste0(base_url, minutes2))
urls2 <- data.frame(year = years3, url = minutes2, stringsAsFactors = FALSE)

############################## Scrape FOMC Minutes from 2019 to 2024 ##############################
page_link1 <- paste0("https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm")
# Read the HTML content of the specified page
page <- read_html(page_link1)
# Find all "statement" links on that page
minutes_links <- page %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  grep("/monetarypolicy/fomcminutes", ., value = TRUE)  # Filter for the specific link pattern
# Clean up the links (ensure full URLs)
base_url <- "https://www.federalreserve.gov"
minutes_links <- ifelse(grepl("^http", minutes_links), minutes_links, paste0(base_url, minutes_links))
# Save the links in a data frame and add the extracted year as a separate column
Years <- sub(".*/fomcminutes([0-9]{4}).*", "\\1", minutes_links)
urls3<- data.frame(year = Years, url = minutes_links, stringsAsFactors = FALSE)
urls3 <- urls3[nrow(urls3):1, ]

## Merge all the periods and create an excel file 
Minutes <- rbind(urls,urls1,urls2,urls3)
colnames(Minutes)<-c("year","link")
write.xlsx(Minutes, file = "FOMC Minutes links.xlsx")


######################################################## Extracting Text and date from the links ########################################################
############################## ECB Accounts --> Minutes ##############################
# Load the file with the links 
Accounts_file <- "C:/one drive/Počítač/UNI/Bachelors thesis/Cleaned files/ECB Minute links.xlsx"
ECB_Minutes <- read_excel(Accounts_file)
# Access each link in the data frame and extract text
ECB_Minutes <- ECB_Minutes %>%
  mutate(Text = sapply(link, extract_text_ECB)) 
# Extract precise date
date <- str_extract(ECB_Minutes$Text, "\\b(\\d{1,2}(?:-\\d{1,2})? [A-Za-z]+ \\d{4})\\b")
date<- as.data.frame(date)
# Create a data frame with the date and text
Minutes_ECB <- cbind(date, ECB_Minutes$Text)
colnames(Minutes_ECB)<- c("Date","Text")
#### Create a file with the processed data
write.csv(Minutes_ECB,file = "ECB Accounts.csv",row.names=FALSE)

############################## ECB Decisions  ##############################
# Load the file with the links 
Decisions_file <- "C:/one drive/Počítač/UNI/Bachelors thesis/Cleaned files/ECB Decision links.xlsx"
ECB_decisions <- read_excel(Decisions_file)
# Access each link in the data frame and extract text
ECB_decisions <- ECB_decisions %>%
  mutate(Text = sapply(link, extract_text_ECB)) 
# Extract precise date
Text <- sapply(ECB_decisions$link, extract_text_for_date)
date_pattern <- "\\b(\\d{1,2}(?:-\\d{1,2})? [A-Za-z]+ \\d{4})|([A-Za-z]+ \\d{1,2}, \\d{4})\\b"
dates <- as.data.frame(sapply(Text, function(text) str_extract(text, date_pattern)))
#### Create a dataframe with the date and text
Decisions_ECB <- cbind(dates, ECB_decisions$Text)
colnames(Decisions_ECB)<- c("Date","Text")
#### Create a file with the processed data
write.csv(Decisions_ECB,file = "ECB Decisions.csv",row.names=FALSE)

############################## FED Minutes  ##############################
# Load the file with the links 
Minutes_file <- "C:/one drive/Počítač/UNI/Bachelors thesis/Cleaned files/FOMC Minutes links.xlsx"
FOMC_Minutes <- read_excel(Minutes_file)
# Access each link in the data frame and extract text
FOMC_Minutes <- FOMC_Minutes %>%
  mutate(Text = sapply(link, extract_text_FED_minutes)) 
# Extract precise date
date <- str_extract(FOMC_Minutes$Text, "([A-Za-z]+ \\d{1,2}(-[A-Za-z]+ \\d{1,2})?, \\d{4})|([A-Za-z]+ \\d{1,2}, \\d{4})") 
date<- as.data.frame(date)
# Clean the file                               
cleaned_files <- (cleaning_FED(FOMC_Minutes))
Clean_FOMC_Minutes <- cbind(date, cleaned_files)
colnames(Clean_FOMC_Minutes)<- c("Date","Text")
write.csv(Clean_FOMC_Minutes,file = "FOMC Minutes.csv",row.names=FALSE

          
############################## FED Statements --> Decisions  ##############################          
# Load the file with the links 
Statements_file <- "C:/one drive/Počítač/UNI/Bachelors thesis/Cleaned files/FOMC Statements links.xlsx"
FOMC_Statements <- read_excel(Statements_file)
# Access each link in the data frame and extract text
FOMC_Statements <- FOMC_Statements %>%
  mutate(Text = sapply(link, extract_text_FED_decisions)) 
# Extract precise date
dates <- str_extract(FOMC_Statements$Text, "([A-Za-z]+ \\d{1,2}(-[A-Za-z]+ \\d{1,2})?, \\d{4})|([A-Za-z]+ \\d{1,2}, \\d{4})")
Date<- as.data.frame(dates)
cleaned_filess <- (cleaning_FED(FOMC_Statements))
#Create a data frame with only the data and the cleaned text
Clean_FOMC_Statements <- cbind(Date, cleaned_filess)
colnames(Clean_FOMC_Statements)<- c("Date","Text")
#Save the results in a new file
write.csv(Clean_FOMC_Statements,file = "FOMC Statements.csv",row.names = FALSE)
        
                          


