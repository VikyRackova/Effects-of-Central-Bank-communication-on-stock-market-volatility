rm(list=ls())
# Set your working directory
setwd()
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
library(tidyverse)
library(readr)
library(tidytext)
library(lubridate)
library(MASS)
library(tidyr)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

######################################################################################### GATHER LINKS FROM FOMC DECISIONS AND MINUTES #########################################################################################
# Note: Links from ECB minutes and decisions were gathered manually and are provided in the Excel files ("ECB Decisions links" and "ECB Minutes links")
####################################################### Save links to FOMC decisions #######################################################
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

####################################################### Save links to FOMC minutes #######################################################
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


######################################################################################### EXTRACT TEXT AND DATE FROM LINKS #########################################################################################
####################################################### ECB Accounts --> Minutes #######################################################
# Load the file with the links
ECB_Minutes <- read_excel("ECB Minute links.xlsx")
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

####################################################### ECB Decisions  #######################################################
# Load the file with the links 
ECB_decisions <- read_excel("ECB Decision links.xlsx")
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

####################################################### FED Minutes  #######################################################
# Load the file with the links 
FOMC_Minutes <- read_excel("FOMC Minutes links.xlsx")
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

          
####################################################### FED Statements --> Decisions  #######################################################          
# Load the file with the links 
FOMC_Statements <- read_excel("FOMC Statements links.xlsx")
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


#########################################################################################  CREATE A DICTIONARY OF TERMS FOR HAWK-SCORE #########################################################################################
####################################################### Modifiers #######################################################          
high_modifiers <- c( "above", "accelerate", "accelerated", "accelerates", "accelerating", "added",
                     "augment", "augmented", "augmenting", "augments", "big","bigger", "bigger than estimated", 
                     "bigger than expected", "bigger than usual","biggest", "boost", "boosted",
                     "boosting", "boosts", "brighter", "buoy", "buoyant", "buoyed", "buoying", "buoys",
                     "climb", "climbed", "climbing", "climbs", "elevate", "elevated", "elevates",
                     "elevating", "escalate", "escalated", "escalates", "escalating", "exceed", "exceeded",
                     "exceeding", "exceeds", "excessive","expand", "expanded", "expanding", "expands",
                      "expansionary", "fast", "faster", "faster than estimated", "faster than expected",
                      "faster than usual","fastest","further", "further than estimated", 
                     "further than expected", "further than usual","furthering", "gain", "gain", "go up",
                     "gaining", "gained", "grew", "grow", "growing", "grown", "grows", "hawk", "hawkish", 
                     "high","higher", "higher than estimated", "higher than expected", "higher than usual",
                     "highest", "hike", "hikes", "hiking", "impulse", "impulsed", "impulses", "impulsing",
                     "increase", "increased", "increases", "increasing", "inflationary", "large",
                     "larger", "larger than estimated", "larger than expected", "larger than usual",
                     "largest", "lift", "lifted", "lifting","lifts", "maximum","more", "more than estimated", 
                     "more than expected", "more than usual","mount", "mounted", "mounting", "mounts", "peak",
                     "peaked", "peaking", "peaks", "pick up", "picked up", "picking up", "picks up", "raise",
                     "raised", "raises", "raising", "ramp", "ramped", "ramping", "ramps", "rapid", "rise",
                     "risen", "rises", "rising", "rose","show growth", "showed growth", "showing growth", 
                     "shows growth","skyrocket", "skyrocketed", "skyrocketing", "skyrockets", "spike", "spiked",
                     "spikes", "spiking", "spur", "spurred", "spurring", "spurs", "strengthen",
                     "strengthened", "strengthening", "strengthens", "strong", "stronger", "stronger than estimated", 
                     "stronger than expected", "stronger than usual","strongest", "surge", "surged", "surges", 
                     "surging","swifter", "stronger than estimated", "stronger than expected", "stronger than usual",
                     "tighten", "tightened", "tightening", "tightens", "tighter","upper", "upside", "upside risk", 
                     "upside risks","upswing", "upswinging", "upswings", "upswung", "uptrend", "upturn", "upturned",
                     "upturning", "upturns", "upward", "upwards", "upward risk", "upward risks",
                     "upward trend", "upward trends", "upwards risk", "upwards risks","upwards trend", 
                     "upwards trends","vigor", "vigorous", "widen", "widened", "widening", "widens", "wider")

all_terms<- wordlist2dataframe(high_modifiers,"High modifier")

positive_modifiers <- c( "accommodate", "accommodated", "accommodates", "accommodating","benign", "best",
                         "better", "better than estimated", "better than expected", "better than usual",
                         "calm", "calmed","calmer", "calmer than estimated", "calmer than expected", "calmer than usual",
                         "calming", "calms","depreciation", "depreciate", "depreciated", "depreciating", "depreciates",
                         "dynamic", "ease", "eases", "eased", "easing", "encouraging","excellent", "expansion", "expansionary", 
                         "expansive","favorable", "favourable", "firmer", "good", "great","greater", "greater than estimated", 
                         "greater than expected", "greater than usual","greatest","healthier", "healthier than estimated", 
                         "healthier than expected", "healthier than usual","improve", "improved", "improves", "improving",
                         "loose", "loosen", "loosened", "loosening", "loosens","looser", "looser than estimated", 
                         "looser than expected", "looser than usual","mitigate", "mitigated", "mitigates", "mitigating", 
                         "optimistic", "outperform","outperformed", "outperforming", "outperforms", "positive","recover", 
                         "recovered", "recovering", "recovers","reinforce", "reinforced", "reinforces", "reinforcing",
                         "remain unchanged","restore", "restored", "restores", "restoring","satisfactory", "stabilise", "stabilised",
                         "stabilises", "stabilising", "stabilize", "stabilized","stabilizes", "stabilizing", "stable",
                         "stimulate", "stimulated", "stimulates", "stimulating","stimulative", "stimulatory", "steady", 
                         "successful")
all_terms<- rbind(all_terms,wordlist2dataframe(positive_modifiers,"Positive modifier"))

low_modifiers<- c( "accommodate", "accommodating", "accommodative", "below", "bottom", "bottomed","bottoming", 
                   "bottoms", "collapse", "collapsing", "collapsed","compress",  "compressed", "compression", 
                   "contract", "contracted","contracting", "contraction", "contractions", "contractionary", "contracts",
                   "cut", "cutting", "cuts","dampen", "dampened", "dampening", "dampens", "decelerate", "decelerated", 
                   "decelerates","decelerating", "decline", "declined", "declines", "declining", "decrease", "decreased",
                   "decreases", "decreasing", "deflationary","depress", "depressed", "depresses", "depressing",
                   "descend", "descended", "descending", "descends", "diminish", "diminished","diminishes", "diminishing", 
                   "disinflationary", "dove", "dovish", "down","downside", "downside risk", "downside risks", "downsides",
                   "downsize", "downsized", "downsizes","downsizing", "downward", "downwards","downward trend ", 
                   "downwards trend", "downward trends", "downwards trends","downward risk", "downwards risk", 
                   "downward risks", "downwards risks","drop", "dropped", "dropping", "drops","erode", "eroded", 
                   "erodes", "eroding", "fade","faded", "fades", "fading",  "fall", "fallen","falling", "falls", "fell",
                   "fewer", "fewer than estimated", "fewer than expected", "fewer than usual","flatten", "flattened", 
                   "flattening", "flattens", "hopeful","lagged", "lagging", "lagged behind", "lagging behind", "least",
                   "less", "less than estimated", "less than expected", "less than usual","lost", "losing", "slowdown", 
                   "low","lower", "lower than estimated", "lower than expected", "lower than usual","lowered", "lowering",
                   "lowers", "lowest", "mild","minimal", "minimum", "minor", "moderate", "moderated", "moderates", "moderating",
                   "modest", "negative", "recede", "receding", "recedes","recessionary", "reduce", "reduced",
                   "reduces", "reducing", "reduction", "reductions","reversal of increases", "reversed increases", "sank", 
                   "shorten", "shortened","shortening", "shortens", "shrink", "shrinking", "shrinks", "shrunk", "shrunken", 
                   "sink","sinking", "slow", "slowed", "slower", "slowest", "slowing", "slows", "sluggish","slump", "slumping", 
                   "small","smaller", "smaller than estimated", "smaller than expected", "smaller than usual","smallest", "soften", 
                   "softened", "softening", "softens", "subside","subsides", "subsiding", "subdued","sunk", "suppress", "suppressed", 
                   "suppresses", "suppressing","temper", "tempered", "tempering", "wane", "waned", "wanes", "waning")
all_terms<- rbind(all_terms,wordlist2dataframe(low_modifiers,"Low modifier"))

negative_modifiers <- c("adverse", "aggravate", "aggravated", "aggravates", "aggravating","appreciate", "appreciation", "appreciated", 
                        "apppreciating", "appreciates","bad", "badly","challenging", "concern", "concerned", "concerning", "concerns", 
                        "conservative","constrain", "constrained", "constraining", "constrains","deepen", "deepened", "deepening", 
                        "deepens","deeper",  "deeper than estimated", "deeper than expected", "deeper than usual","destabilizing", 
                        "deteriorate", "deteriorated", "deteriorates", "deteriorating", "difficult", "difficulty", "disappoint", 
                        "disappointed", "disappointing","disappoints", "fail", "failed", "failing", "fails","fluctuate", "fluctuated",
                        "fluctuates", "fluctuating", "fragile", "harm", "harmed", "harmful", "harming", "harms", "inconsistent", 
                        "jeopardise", "jeopardised", "jeopardises","jeopardising", "jeopardize", "jeopardized", "jeopardizes", 
                        "jeopardizing", "lackluster","pessimistic", "poor", "restrictive", "require support", "requiring support",
                        "requires support",  "riskier", "risky", "stagnating", "stagnation","stress", "stressed", "stresses","stressful", 
                        "stressing","stringent", "subprime", "tepid", "terrible","threaten", "threatened", "threatening","threatens", 
                        "torrid", "tougher", "troubling", "troubled","turbulent", "uncertain","unclear", "undermine", "unfavorable", 
                        "unfavourable", "unstable", "volatile","vulnerable", "weak", "weaken", "weakened","weakening", "weakens", 
                        "weakness", "weaknesses", "weakest","weaker", "weaker than estimated", "weaker than expected","weaker than usual",
                        "worrying","worse", "worse than estimated", "worse than expected", "worse than usual",
                        "worsen", "worsened", "worsening", "worsens", "worst")
all_terms<- rbind(all_terms,wordlist2dataframe(negative_modifiers,"Negative modifier"))

negators <- c("anti", "aren t", "by no means",
              "can t", "cannot", "cannot be", "cannot but be",
              "halt", "halt a", "halt an", "halt the",
              "halted", "halted the",
              "halting", "halting the",
              "halts", "halts the",
              "nt", "not", "not a", "not an", "not the","not expected to",
              "not allow", "not allow a", "not allow an", "not allow the", "not be",
              "not permit", "not permit a", "not permit an", "not permit the",
              "prevent", "prevent a", "prevent an", "prevent the",
              "preventing", "preventing a", "preventing an", "preventing the",
              "prevents",  "prevents a", "prevents an", "prevents the",
              "not permit a", "not permit an", "not permit the","no reason to","no reason to expect",
              "not rule out", "not rule out a", "not rule out an", "not rule out the",
              "reverse", "reverse the", "reverse a", "reverse an",
              "reversed", "reversed the", "reversed a", "reversed an",
              "reverses", "reverses the", "reverses a", "reverses an",
              "reversing", "reversing the", "reversing a", "reversing an",
              "reversal of", "reversal of the","unlikely to")

all_terms<- rbind(all_terms,wordlist2dataframe(negators,"Negator"))

####################################################### HAWKISH keywords #######################################################

policy_hawk <- c("bank rate", "board member", "board members", "central bank", "central banks",
                 "committee member",  "committee members", "core rates", "deposit rates",
                 "deposit facility","euribor", "interbank interest rate",
                 "interbank rate", "interbank rates", "interest rate", "interest rates",
                 "libor", "lombard rate", "main refinancing operations",
                 "marginal standing facility","marginal lending facility", "market rates", "monetary conditions",
                 "monetary policy", "monetary policy action", "monetary policy actions",
                 "monetary policy instrument", "monetary policy instruments",
                 "monetary policy stance", "monetary policy stances",
                 "monetary policies", "monetary stance", "monetary stances",
                 "money demand", "money supply",
                 "policy action", "policy actions", "policy decision", "policy decisions",
                 "policy instrument", "policy instruments", "policy stance", "policy stances",
                 "policy repo rate", "policy rate", "policy rates",
                 "pribor", "rates",
                 "repo rate", "repo rates", "reserve bank", "reverse repo rate", "reverse repo rates",
                 "selic rate","swap rates")
all_terms<- rbind(all_terms,wordlist2dataframe(policy_hawk, "Hawkish Keyword", topic = "Policy"))

growth_hawk <- c("activity", "aggregate demand", "capacity utilisation", "capacity utilization",
                 "capital expenditure", "capital formation", "capital investment", "capital spending",
                 "consumption", "consumption expenditure", "consumer spending",
                 "demand", "demand side", "disposable income",
                 "domestic demand", "domestic economy", "domestic growth",
                 "economic", "economic activity", "economic conditions",
                 "economic development", "economic growth", "economic output",
                 "economic recovery", "employment",
                 "employment growth", "expenditure", "expenditure growth",
                 "growth", "growth outlook", "growth expectations",
                 "growth forecast", "growth forecasts", "growth prospects",
                 "gdp", "gdp growth",
                 "household consumption", "household income", "household spending",
                 "industrial", "industrial production", "investment",
                 "inventory", "inventories",
                 "labor market", "labour market", "labour productivity", "manufacturing",
                 "output", "output gap", "output growth",
                 "pmi", "private consumption", "production", "productivity",
                 "recovery", "retail sales",
                 "services sector", "spending", "supply side")
all_terms<- rbind(all_terms,wordlist2dataframe(growth_hawk, "Hawkish Keyword", topic = "Growth"))

prices_hawk <- c("commodity price", "commodity prices", "consumer price", "consumer prices",
                 "core inflation", "cost",  "costs",
                 "consumer price index", "cpi", "cpi inflation", "domestic inflation",
                 "energy price", "energy prices", "expected inflation", "food inflation",
                 "food price", "food prices",
                 "headline inflation", "house price", "house prices", "inflation",
                 "inflation expectation", "inflation expectations", 
                 "inflation outlook", "inflationary pressures", "inflationary pressure",
                 "inflation projection", "inflation projections",
                 "inflation report", "inflation risk", "inflation risks", "inflation target",
                 "inflationary expectation", "inflationary expectations",
                 "inflation data", "inflation prediction", "inflation forecast", "inflation forecats",
                 "inflationary pressure", "inflationary pressure", "inflationary pressures",
                 "inflationary risk", "inflationary risks",
                 "labour cost", "labour costs", "minimum wage", "minimum wages",
                 "oil", "oil price", "oil prices",
                 "price", "price growth", "price inflation", "prices", "prices growth",
                 "producer price", "producer prices", "producer price index",
                 "underlying inflation", "unit labour cost", "unit labour costs",
                 "wage", "wage growth", "wages", "wages growth")
all_terms<- rbind(all_terms,wordlist2dataframe(prices_hawk, "Hawkish Keyword", topic = "Prices"))


finance_hawk <- c("asset price", "asset prices",
                  "banks", "banking system",  "banking sector", "banking sector",
                  "capital flows", "credit", "credit growth", "commercial paper",
                  "dollar", "euro", "euro area",
                  "exchange rate", "exchange rates", "equity", "equities", "equity market", "equity markets",
                  "financial conditions", "financial market", "financial markets",
                  "financial sector", "financial services", "financial system", "financial systems",
                  "foreign currency", "foreign currencies", "foreign exchange",
                  "global financial", "investors", "lending", "liquidity", "liquidity conditions",
                  "loan", "securities", "us dollar")
all_terms<- rbind(all_terms,wordlist2dataframe(finance_hawk, "Hawkish Keyword", topic = "Finance"))

global_hawk <- c("advanced country", "advanced countries", "advanced economy", "advanced economies",
                 "capital flows", "current account", "china",
                 "developed markets", "developed economies", "exports",
                 "emerging economy", "emerging economies", "emerging market", "emerging markets",
                 "emerging country", "emerging countries",
                 "europe", "european", "european central bank",
                 "export", "exports", "net exports", "export growth", "exports growth",
                 "federal reserve",
                 "global", "global economic", "global economic growth", "global economic activity",
                 "global economy", "global growth", "global outlook",
                 "global recovery",
                 "international", "international growth", "japan", "japanese",
                 "trade", "trading partners",
                 "united states", "usa", "us rates",
                 "world economic activity", "world economic growth",
                 "world economy", "world growth")
all_terms<- rbind(all_terms,wordlist2dataframe(global_hawk, "Hawkish Keyword", topic = "Global"))

other_hawk <- c("confidence", "housing", "housing market", "sentiment", "surplus",
                "vaccine", "vaccines", "vaccination", "vaccinations")
all_terms<- rbind(all_terms,wordlist2dataframe(other_hawk, "Hawkish Keyword", topic = "Other"))

####################################################### DOVISH Keywords #######################################################

policy_dove <- c("monetary easing", "monetary easing cycle", "monetary stimulus", "stimulus")
all_terms<- rbind(all_terms,wordlist2dataframe(policy_dove, "Dovish Keyword", topic = "Policy"))

growth_dove <- c("economic activity contraction", "economic contraction", "economic slack",
                 "economic uncertainty",
                 "gdp decline", "household saving", "household savings", "idleness",
                 "precautionary saving", "precautionary savings",
                 "recession", "saving", "savings",
                 "slack", "spare capacity", "uncertainty about economic growth",
                 "unemployed", "unemployment", "unemployment rate", "unemployment rates")
all_terms<- rbind(all_terms,wordlist2dataframe(growth_dove, "Dovish Keyword", topic = "Growth"))

prices_dove <- c("expected deflation", "expected disinflation", "deflation", "disinflation",
                 "deflationary risk", "deflationary risks",
                 "disinflationary risk", "disinflationary risks")
all_terms<- rbind(all_terms,wordlist2dataframe(prices_dove, "Dovish Keyword", topic = "Prices"))

finance_dove <- c("financial crisis", "financial instability", "financial uncertainty",
                  "financial volatility", "market volatility")
all_terms<- rbind(all_terms,wordlist2dataframe(finance_dove, "Dovish Keywords", topic = "Finance"))

global_dove <- c("geopolitical risks", "import", "imports", "import growth", "imports growth",
                 "net imports")
all_terms<- rbind(all_terms,wordlist2dataframe(global_dove, "Dovish Keyword", topic = "Global"))

other_dove <- c("coronavirus", "covid", "covid19", "covid 19", "deficit",
                "epidemic", "infections", "instability", "lockdown", "lockdowns",
                "pandemic", "risk", "risks", "tax rates",
                "uncertainty", "vat rates", "variance", "volatility")
all_terms<- rbind(all_terms,wordlist2dataframe(other_dove, "Dovish Keyword", topic = "Other"))

neutral_terms <- c("all of the above", "at least", "best practice", "best practices",
                   "committee stresses that", "forward contracts", "greater transparency",
                   "greater focus",
                   "high frequency", "higher frequency", "high quality", "high yield",
                   "increasing weight", "more detail", "more timely", "more or less",
                   "rate of change", "rates of change")
all_terms<- rbind(all_terms,wordlist2dataframe(neutral_terms, "Neutral Phrase"))
# Initialize empty data frames for each type
fourgrams <- data.frame(term = character(), category = character(), topic = character())
trigrams <- data.frame(term = character(), category = character(), topic = character())
bigrams <- data.frame(term = character(), category = character(), topic = character())
monograms <- data.frame(term = character(), category = character(), topic = character())

# Loop through each row in all_terms
for (i in seq_len(nrow(all_terms))) {
  # Extract term, category, and topic from each row
  term <- all_terms$term[i]
  category <- all_terms$category[i]
  topic <- all_terms$topic[i
  # Count the number of words in the term
  word_count <- str_count(term, "\\S+")
  # Append the row to the appropriate data frame based on word count
  if (word_count == 4) {
    fourgrams <- rbind(fourgrams, data.frame(term = term, category = category, topic = topic))
  } else if (word_count == 3) {
    trigrams <- rbind(trigrams, data.frame(term = term, category = category, topic = topic))
  } else if (word_count == 2) {
    bigrams <- rbind(bigrams, data.frame(term = term, category = category, topic = topic))
  } else if (word_count == 1) {
    monograms <- rbind(monograms, data.frame(term = term, category = category, topic = topic))
  }
}

# Replace the spaces with an underscore for all_terms
all_terms<- all_terms%>%
  mutate(term = str_replace_all(term, " ", "_"))

# Save the results          
write.csv(all_terms,file = "All terms.csv", row.names=FALSE)
write.csv(fourgrams,file = "Fourgrams.csv", row.names=FALSE)
write.csv(trigrams,file = "Trigrams.csv", row.names=FALSE)
write.csv(bigrams,file = "Bigrams.csv", row.names=FALSE)

######################################################################################### HAWK-SCORE CALCULATION #########################################################################################
# Load all the text files and dictionary classifications
Minutes_FED<-read.csv("FOMC Minutes.csv")
Minutes_ECB<-read.csv("ECB Accounts.csv")
Decisions_FED<-read.csv("FOMC Statements.csv")
Decisions_ECB<- read.csv("ECB Decisions.csv")
All_terms <- read.csv("All terms.csv")
Fourgrams<- read.csv("Fourgrams.csv")
Trigrams <-read.csv("Trigrams.csv")
Bigrams <-read.csv("Bigrams.csv")
Four_grams <- Fourgrams %>%
  mutate(term = str_replace_all(term, " ", "_"))
Tri_grams<- Trigrams %>%
  mutate(term = str_replace_all(term, " ", "_"))
Bi_grams<- Bigrams %>%
  mutate(term = str_replace_all(term, " ", "_"))
   
####################################################### Unify the date format #######################################################
Minutes_FED<- Minutes_FED%>%
  mutate(Date = sapply(Date, standardize_date))
Minutes_ECB <- Minutes_ECB%>%
  mutate(Date = sapply(Date, standardize_date))
Decisions_FED <- Decisions_FED%>%
  mutate(Date = sapply(Date, standardize_date))
Decisions_ECB <- Decisions_ECB%>%
  mutate(Date = sapply(Date, standardize_date))

####################################################### Clean the data and split into sentences  #######################################################
Sentences_minutes_FED <- sentenceSplit(Minutes_FED)%>% # Split the text to sentences 
  mutate(Cleaned_Text = sapply(sentence, clean_text))%>% # clean the text with a function 
  group_by(Date) %>%
  mutate(sentence_id = row_number()) %>%  # assign every sentence a number
  ungroup()
Sentences_minutes_ECB <- sentenceSplit(Minutes_ECB)%>% 
  mutate(Cleaned_Text = sapply(sentence, clean_text))%>%
  group_by(Date) %>%
  mutate(sentence_id = row_number()) %>%  
  ungroup()
Sentences_decisions_FED <- sentenceSplit(Decisions_FED)%>% 
  mutate(Cleaned_Text = sapply(sentence, clean_text))%>%
  group_by(Date) %>%
  mutate(sentence_id = row_number()) %>%  
  ungroup()
Sentences_decisions_ECB <- sentenceSplit(Decisions_ECB)%>% 
  mutate(Cleaned_Text = sapply(sentence, clean_text))%>%
  group_by(Date) %>%
  mutate(sentence_id = row_number()) %>%  
  ungroup()
          
######################################################################################### CHECKING FOR WORDS IN THE TERM LIST (DICTIONARY) #########################################################################################3
####################################################### CHECKING FOR FOURGRAMS #######################################################
##### Decisions
#ECB
ECBD <- process_fourgrams(Sentences_decisions_ECB, Fourgrams, Four_grams)
# FED
FEDD <- process_fourgrams(Sentences_decisions_FED, Fourgrams, Four_grams)
##### Minutes
# ECB
ECBM <- process_fourgrams(Sentences_minutes_ECB, Fourgrams, Four_grams)
# FED
FEDM <- process_fourgrams(Sentences_minutes_FED, Fourgrams, Four_grams)

####################################################### CHECKING FOR TRIGRAMS #######################################################
##### Decisions
ECBD <- process_trigrams(ECBD, Trigrams, Tri_grams)
# FED
FEDD <- process_trigrams(FEDD, Trigrams, Tri_grams)
##### Minutes
# ECB
ECBM <- process_trigrams(ECBM, Trigrams, Tri_grams)
# FED
FEDM <- process_trigrams(FEDM, Trigrams, Tri_grams)

####################################################### CHECKING FOR BIGRAMS #######################################################
##### Decisions
#ECB
ECBD <- process_bigrams(ECBD, Bigrams, Bi_grams)
#FED
FEDD <-process_bigrams(FEDD, Bigrams, Bi_grams)
##### Minutes
#ECB
ECBM <- process_bigrams(ECBM, Bigrams, Bi_grams)
#FED
FEDM <- process_bigrams(FEDM, Bigrams, Bi_grams)

          
####################################################### RETAIN DATE, SENTENCE ID, SENTENCE, AND WORD COUNT PER SENTENCE #######################################################          
Clean_ECBD <- ECBD%>%
  mutate(word_count = str_count(merged_sentence, "\\S+"))# create word count per sentence
Clean_FEDD <- FEDD%>%
  mutate(word_count = str_count(merged_sentence, "\\S+"))# create word count per sentence
Clean_ECBM <- ECBM%>%
  mutate(word_count = str_count(merged_sentence, "\\S+"))# create word count per sentence
Clean_FEDM <- FEDM%>%
  mutate(word_count = str_count(merged_sentence, "\\S+"))# create word count per sentence


####################################################### ASSIGN THE SCORE TO ALL MODIFIER AND KEYWORD COMBINATIONS #######################################################          
Scores_FEDD <-calculate_scores(Clean_FEDD, All_terms)
Scores_FEDM <- calculate_scores(Clean_FEDM, All_terms)
Scores_ECBD <- calculate_scores(Clean_ECBD, All_terms)
Scores_ECBM <- calculate_scores(Clean_ECBM, All_terms)

#Save the results
write.csv(Scores_ECBD, file = "Scores_ECBD.csv",row.names = FALSE)
write.csv(Scores_ECBM, file = "Scores_ECBM.csv",row.names = FALSE)
write.csv(Scores_FEDD, file = "Scores_FEDD.csv",row.names = FALSE)
write.csv(Scores_FEDM, file = "Scores_FEDM.csv",row.names = FALSE)
          
####################################################### CREATE TOTAL STANDARDIZED SCORES AND STANDARDIZED SCORES PER TOPIC #######################################################
Final_score_ECBM<-standardized_score_and_topic(Scores_ECBM)
Final_score_ECBD<-standardized_score_and_topic(Scores_ECBD)
Final_score_FEDD<-standardized_score_and_topic(Scores_FEDD)
Final_score_FEDM<-standardized_score_and_topic(Scores_FEDM)

# Save the results
write.csv(Final_score_ECBM, file = "Final_score_ECBM.csv",row.names = FALSE)         
write.csv(Final_score_ECBD, file = "Final_score_ECBD.csv",row.names = FALSE)   
write.csv(Final_score_FEDD, file = "Final_score_FEDD.csv",row.names = FALSE)   
write.csv(Final_score_FEDM, file = "Final_score_FEDM.csv",row.names = FALSE)   

######################################################################################### CREATING TOPIC DUMMIES #########################################################################################
################################ Create Dummy variables of most commonly discussed topics per document ################################        
topics_ECBD_DUMMY <- most_common_topics_dummy(Scores_ECBD)
topics_ECBM_DUMMY <- most_common_topics_dummy(Scores_ECBM)
topics_FEDD_DUMMY <- most_common_topics_dummy(Scores_FEDD)
topics_FEDM_DUMMY <- most_common_topics_dummy(Scores_FEDM)

#Save the results               
write.csv(topics_ECBD_DUMMY, file = "topics_ECBD_DUMMY.csv",row.names = FALSE)
write.csv(topics_ECBM_DUMMY, file = "topics_ECBM_DUMMY.csv",row.names = FALSE)
write.csv(topics_FEDD_DUMMY, file = "topics_FEDD_DUMMY.csv",row.names = FALSE)
write.csv(topics_FEDM_DUMMY, file = "topics_FEDM_DUMMY.csv",row.names = FALSE)

######################################################################################### GENERATE WORD COUNT PER DOCUMENT  #########################################################################################        
WCPD_ECBD<- process_word_count(Clean_ECBD)
WCPD_ECBM<- process_word_count(Clean_ECBM)
WCPD_FEDD<- process_word_count(Clean_FEDD)
WCPD_FEDM<- process_word_count(Clean_FEDM)

#Save the results               
write.csv(WCPD_ECBD, file = "WCPD_ECBD.csv",row.names = FALSE)
write.csv(WCPD_ECBM, file = "WCPD_ECBM.csv",row.names = FALSE)
write.csv(WCPD_FEDD, file = "WCPD_FEDD.csv",row.names = FALSE)
write.csv(WCPD_FEDM, file = "WCPD_FEDM.csv",row.names = FALSE)

######################################################################################### CREATE SCORES OF SENTIMENT AND UNCERTAINTY FROM TEXT #########################################################################################
sentiment_score_ECBD <- sentiment(Decisions_ECB)
sentiment_score_ECBM <- sentiment(Minutes_ECB)
sentiment_score_FEDD <- sentiment(Decisions_FED)
sentiment_score_FEDM <- sentiment(Minutes_FED)


write.csv(sentiment_score_ECBD, file = "sentiment_score_ECBD.csv",row.names = FALSE)
write.csv(sentiment_score_ECBM, file = "sentiment_score_ECBM.csv",row.names = FALSE)
write.csv(sentiment_score_FEDD, file = "sentiment_score_FEDD.csv",row.names = FALSE)
write.csv(sentiment_score_FEDM, file = "sentiment_score_FEDM.csv",row.names = FALSE)
          
