###############################
# FOMC Links Scraping Script
###############################

# Clear workspace
rm(list = ls())

# Set working directory
setwd("C:/one drive/Počítač/UNI/Bachelors thesis/Cleaned files")

# Load required libraries
library(rvest)
library(dplyr)
library(stringr)
library(openxlsx)

#######################################
# Helper Functions
#######################################

# Function to construct full URLs from relative links
construct_full_url <- function(base_url, links) {
  ifelse(grepl("^http", links), links, paste0(base_url, links))
}

# Function to extract FOMC statements from 1994 to 2018
extract_statements <- function(start_year, end_year, base_url) {
  decisions <- c()
  years <- c()

  for (year in start_year:end_year) {
    page_link <- paste0(base_url, "/monetarypolicy/fomchistorical", year, ".htm")
    page <- tryCatch(read_html(page_link), error = function(e) NULL)
    if (is.null(page)) next

    headings <- page %>% html_nodes(xpath = "//h5 | //a")
    current_heading <- NA

    for (node in headings) {
      if (html_name(node) == "h5") {
        current_heading <- html_text(node, trim = TRUE)
      } else if (html_name(node) == "a") {
        if (grepl("Meeting", current_heading, ignore.case = TRUE)) {
          link_text <- html_text(node, trim = TRUE)
          link <- html_attr(node, "href")
          if (grepl("^Statement$", link_text, ignore.case = TRUE)) {
            decisions <- c(decisions, link)
            years <- c(years, year)
          }
        }
      }
    }
  }

  data.frame(year = years, link = construct_full_url(base_url, decisions), stringsAsFactors = FALSE)
}

# Function to extract FOMC statements from 2019 to 2024
extract_statements_recent <- function(base_url, page_link) {
  page <- tryCatch(read_html(page_link), error = function(e) NULL)
  if (is.null(page)) return(data.frame())

  links <- page %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    grep("/monetarypolicy/files/monetary", ., value = TRUE)

  years <- sub(".*/monetary([0-9]{4}).*", "\\1", links)
  data.frame(year = years, link = construct_full_url(base_url, links), stringsAsFactors = FALSE)
}

# Function to extract FOMC minutes for a specified year range
extract_minutes <- function(start_year, end_year, base_url) {
  minutes <- c()
  dates <- c()

  for (year in start_year:end_year) {
    page_link <- paste0(base_url, "/monetarypolicy/fomchistorical", year, ".htm")
    page <- tryCatch(read_html(page_link), error = function(e) NULL)
    if (is.null(page)) next

    nodes <- page %>% html_nodes(xpath = "//p[a[contains(text(), 'Minutes')]]")
    for (node in nodes) {
      link <- node %>% html_node("a") %>% html_attr("href")
      release_date <- node %>% html_text(trim = TRUE) %>% str_extract("[A-Za-z]+ \\d{1,2}, \\d{4}")
      if (!is.null(link) && !is.na(release_date)) {
        minutes <- c(minutes, construct_full_url(base_url, link))
        dates <- c(dates, release_date)
      }
    }
  }

  data.frame(Date = dates, Link = minutes, stringsAsFactors = FALSE)
}

#######################################
# FOMC Statements Processing
#######################################

# Define base URL
base_url <- "https://www.federalreserve.gov"

# Extract statements from 1994 to 2018
statements_1994_2018 <- extract_statements(1994, 2018, base_url)

# Extract statements from 2019 to 2024
page_link_recent <- paste0(base_url, "/monetarypolicy/fomccalendars.htm")
statements_2019_2024 <- extract_statements_recent(base_url, page_link_recent)

# Combine statements and save to file
all_statements <- bind_rows(statements_1994_2018, statements_2019_2024)
write.xlsx(all_statements, file = "FOMC Statements links.xlsx")

#######################################
# FOMC Minutes Processing
#######################################

# Extract minutes for different periods
minutes_1994_2006 <- extract_minutes(1994, 2006, base_url)
minutes_2007_2010 <- extract_minutes(2007, 2010, base_url)
minutes_2011_2018 <- extract_minutes(2011, 2018, base_url)

# Extract minutes from 2019 to 2024
page_recent <- read_html(page_link_recent)
minutes_links_recent <- page_recent %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  grep("/monetarypolicy/fomcminutes", ., value = TRUE) %>%
  construct_full_url(base_url, .)

release_dates_recent <- page_recent %>%
  html_nodes(xpath = "//div[contains(@class, 'fomc-meeting__minutes')]") %>%
  html_text(trim = TRUE) %>%
  str_extract("\\(Released [A-Za-z]+ \\d{1,2}, \\d{4}\\)") %>%
  str_remove_all("\\(Released |\\)") %>%
  na.omit()

minutes_2019_2024 <- data.frame(
  Date = release_dates_recent,
  Link = minutes_links_recent,
  stringsAsFactors = FALSE
)

# Combine minutes and save to file
all_minutes <- bind_rows(minutes_1994_2006, minutes_2007_2010, minutes_2011_2018, minutes_2019_2024)
write.xlsx(all_minutes, file = "FOMC Minutes links.xlsx")


###############################
# Libraries and Functions
###############################

# Clear workspace
rm(list = ls())

# Load required libraries
library(readxl)
library(dplyr)
library(openxlsx)
library(httr)
library(rvest)
library(stringr)
library(xml2)
library(pdftools)

### Function to extract clean text from URL (excluding navigation and links)
extract_text_FED_minutes <- function(link) {
  page <- read_html(link)  # Read the webpage content

  # Remove unwanted tags (e.g., <a>, <nav>, <footer>, etc.)
  unwanted_tags <- c("//a", "//nav", "//footer")
  for (tag in unwanted_tags) {
    nodes_to_remove <- xml_find_all(page, tag)
    xml_remove(nodes_to_remove)
  }

  # Extract the main content (focusing on <p> and <div> tags)
  main_content <- page %>%
    xml_find_all("//p | //div[contains(@class, 'main-content')]") %>%
    xml_text(trim = TRUE)

  cleaned_text <- paste(main_content, collapse = "\n")  # Combine into a single block
  return(cleaned_text)
}

### Function to clean the text
cleaning_FED <- function(data) {
  cleaned_files <- vector("list", length = nrow(data))  # Initialize list for cleaned text

  # Terms to remove
  to_delete <- c("For immediate release", "Stay Connected", "p.m. EDT", 
                 "p.m. EST", "Release Date:", "For release")

  for (i in 1:nrow(data)) {
    text <- data$Text[[i]]  # Get the text for the current row
    lines <- unlist(strsplit(text, "\n"))  # Split into lines

    # Remove unwanted terms
    cleaned_text <- lines[!Reduce(`|`, lapply(to_delete, str_detect, string = lines))]
    cleaned_files[[i]] <- paste(cleaned_text, collapse = " ")  # Recombine as a single string
  }

  # Convert the list of cleaned text into a data frame
  cleaned_files_df <- data.frame(Cleaned_Text = unlist(cleaned_files), stringsAsFactors = FALSE)
  return(cleaned_files_df)
}

### Function to extract all text from a URL and capture date
extract_text_for_date <- function(url) {
  tryCatch({
    page <- read_html(url)

    # Extract headings and paragraph text
    full_text <- page %>%
      html_nodes("main,p") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = " ")

    return(full_text)
  }, error = function(e) {
    return(NA)  # Return NA if there's an error
  })
}

### Function to extract text from both PDF and HTML URLs
extract_text_FED_decisions <- function(link) {
  if (grepl("\\.pdf$", link, ignore.case = TRUE)) {
    pdf_text_content <- pdf_text(link)  # Extract text from PDF
    cleaned_text <- paste(pdf_text_content, collapse = "\n")
  } else {
    page <- read_html(link)  # Process as HTML

    # Remove unwanted tags
    unwanted_tags <- c("//a", "//nav", "//footer", "//script", "//style")
    for (tag in unwanted_tags) {
      nodes_to_remove <- xml_find_all(page, tag)
      xml_remove(nodes_to_remove)
    }

    # Extract relevant content
    body_content <- page %>%
      xml_find_all("//tbody//tr//td | //t | //li | //ul | //body//p|//body//font//i") %>%
      xml_text(trim = TRUE)

    cleaned_text <- paste(body_content, collapse = "\n")  # Combine into a single block
  }
  return(cleaned_text)
}


###############################
# Cleaning FOMC Minutes
###############################

# Load the Excel file for minutes
Minutes_file <- "FOMC Minutes links.xlsx"
FOMC_Minutes <- read_excel(Minutes_file)

# Extract text from links and clean it
FOMC_Minutes <- FOMC_Minutes %>%
  mutate(Text = sapply(link, extract_text_FED_minutes))

cleaned_files <- cleaning_FED(FOMC_Minutes)  # Clean the text
Clean_FOMC_Minutes <- cbind(FOMC_Minutes$Date, cleaned_files)  # Combine with dates
colnames(Clean_FOMC_Minutes) <- c("Date", "Text")

# Save the cleaned minutes to a CSV file
write.csv(Clean_FOMC_Minutes, file = "FOMC Minutes.csv", row.names = FALSE)


###############################
# Cleaning FOMC Statements
###############################

# Load the Excel file for statements
Statements_file <- "C:/one drive/Počítač/UNI/Bachelors thesis/Cleaned files/FOMC Statements links.xlsx"
FOMC_Statements <- read_excel(Statements_file)

# Extract text from links and capture precise dates
FOMC_Statements <- FOMC_Statements %>%
  mutate(Text = sapply(link, extract_text_FED_decisions))

# Extract dates from text
dates <- str_extract(FOMC_Statements$Text, "([A-Za-z]+ \\d{1,2}(-[A-Za-z]+ \\d{1,2})?, \\d{4})|([A-Za-z]+ \\d{1,2}, \\d{4})")
Date <- as.data.frame(dates)

# Clean the text
cleaned_filess <- cleaning_FED(FOMC_Statements)

# Combine cleaned text with extracted dates
Clean_FOMC_Statements <- cbind(Date, cleaned_filess)
colnames(Clean_FOMC_Statements) <- c("Date", "Text")

# Save the cleaned statements to a CSV file
write.csv(Clean_FOMC_Statements, file = "FOMC Statements.csv", row.names = FALSE)

















###############################
# Cleaning the ECB Minutes and Decisions
###############################

# Clear workspace and set working directory
rm(list = ls())
setwd("C:/one drive/Počítač/UNI/Bachelors thesis/Cleaned files")

# Load required libraries
library(readxl)   # For reading Excel files
library(dplyr)    # For data manipulation
library(openxlsx) # For writing Excel files
library(httr)     # For HTTP requests
library(rvest)    # For web scraping
library(stringr)  # For string manipulation
library(xml2)     # For working with XML/HTML
library(pdftools) # For working with PDFs

# ------------------------------
# Function Definitions
# ------------------------------

### Function to extract text from an ECB link (excluding navigation and links)
extract_text_ECB <- function(link) {
  # Read the webpage content
  page <- read_html(link)
  
  # Remove unwanted tags (e.g., <a>, <nav>, <footer>, etc.)
  unwanted_tags <- c("//a", "//nav", "//footer")
  for (tag in unwanted_tags) {
    nodes_to_remove <- xml_find_all(page, tag)
    xml_remove(nodes_to_remove)
  }
  
  # Extract the main content (focusing on <p> and <div> tags within <main>)
  main_content <- page %>%
    xml_find_all("//main//div[contains(@class, 'section') or contains(@class, 'orderedlist')]") %>%
    xml_text(trim = TRUE)
  
  # Combine the extracted text into a single block
  cleaned_text <- paste(main_content, collapse = "\n")
  return(cleaned_text)
}

### Function to extract full text from a URL to also capture the date
extract_text_for_date <- function(link) {
  tryCatch({
    page <- read_html(link)
    
    # Extract the headings and paragraphs text
    full_text <- page %>%
      html_nodes("main,p") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = " ")
    return(full_text)
  }, error = function(e) {
    return(NA)  # Return NA if there's an error
  })
}

# ------------------------------
# Cleaning ECB Minutes (Accounts)
# ------------------------------

# Load the file containing links to ECB Minutes
Accounts_file <- "C:/one drive/Počítač/UNI/Bachelors thesis/Cleaned files/ECB Minute links.xlsx"
ECB_Minutes <- read_excel(Accounts_file)

# Extract text from each link and select relevant columns
Minutes_ECB <- ECB_Minutes %>%
  mutate(Text = sapply(link, extract_text_ECB)) %>%
  dplyr::select(Date, Text)

# Save the processed data to a CSV file
write.csv(Minutes_ECB, file = "ECB Accounts.csv", row.names = FALSE)

# ------------------------------
# Cleaning ECB Decisions
# ------------------------------

# Load the file containing links to ECB Decisions
Decisions_file <- "C:/one drive/Počítač/UNI/Bachelors thesis/Cleaned files/ECB Decision links.xlsx"
ECB_decisions <- read_excel(Decisions_file)

# Extract text from each link and select relevant columns
ECB_decisions <- ECB_decisions %>%
  mutate(Text = sapply(link, extract_text_ECB)) %>%
  dplyr::select(Date, Text)

# Save the processed data to a CSV file
write.csv(ECB_decisions, file = "ECB Decisions.csv", row.names = FALSE)


rm(list=ls())
setwd("C:/one drive/Počítač/UNI/Bachelors thesis/Cleaned files")
################## Word list to construct the Hawk-score ####################
# Function to convert list into data frame
wordlist2dataframe <- function(wordlist, category, topic =""){
  # Get rid of duplicates and sort the list alphabetically
  wordlist <- sort(unique(wordlist))
  # Create a data frame from the cleaned list
  wordlist_df <- data.frame("term"=wordlist,
                            "category"=rep(category,length(wordlist)),
                            "topic"=rep(topic,length(wordlist)))
  return(wordlist_df)
}


################################# Add words to the original data set
# Split the text into words
words <- unlist(strsplit(c(Sentences_minutes_FED$Cleaned_Text,Sentences_decisions_ECB$Cleaned_Text,
                           Sentences_decisions_FED$Cleaned_Text,Sentences_minutes_ECB$Cleaned_Text), " "))
# Count occurrences of each word
word_counts <- table(words)
# Convert to a data frame
word_count_df <- as.data.frame(word_counts)
# Rename columns
colnames(word_count_df) <- c("Word", "Count")
# Sort the DataFrame by count (optional)
word_count_df <- word_count_df[order(-word_count_df$Count), ]


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
                     "largest", "lift", "lifted", "lifting","lifts", "maintain", "maximum","more", "more than estimated", 
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
                         "greater than expected", "greater than usual","greatest","healthier", "healthier than estimated", 
                         "healthier than expected", "healthier than usual","improve", "improved", "improves", "improving",
                         "loose", "loosen", "loosened", "loosening", "loosens","looser", "looser than estimated", 
                         "looser than expected", "looser than usual","mitigate", "mitigated", "mitigates", "mitigating", 
                         "optimistic", "outperform","outperformed", "outperforming", "outperforms", "positive","recover", 
                         "recovered", "recovering", "recovers","reinforce", "reinforced", "reinforces", "reinforcing",
                         "restore", "restored", "restores", "restoring","satisfactory", "stabilise", "stabilised",
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
                   "lowers", "lowest", "mild","minimal","minor", "moderate", "moderated", "moderates", "moderating",
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

############################# HAWKISH keywords #############################

policy_hawk <- c("bank rate", "board member", "board members", "central bank", "central banks", "committee",
                 "committee member",  "committee members", "core rates", "deposit rates","deposit rate", "discount rate",
                 "deposit facility","euribor", "interbank interest rate", "lending facility", "funds rate",
                 "interbank rate", "interbank rates", "interest rate", "interest rates",
                 "libor", "lombard rate", "main refinancing operations",
                 "market rates", "monetary conditions",
                 "monetary policy", "monetary policy action", "monetary policy actions",
                 "monetary policy instrument", "monetary policy instruments",
                 "monetary policy stance", "monetary policy stances",
                 "monetary policies", "monetary stance", "monetary stances",
                 "money demand", "money supply",
                 "policy action", "policy actions", "policy decision", "policy decisions",
                 "policy instrument", "policy instruments", "policy stance", "policy stances",
                 "policy repo rate", "policy rate", "policy rates",
                 "pribor","operations", "rate", "rates","refinancing",
                 "repo rate", "repo rates", "reserve", "reserve bank", "reserve positions", "reverse repo rate", "reverse repo rates",
                 "selic rate","swap rates", "stance")
all_terms<- rbind(all_terms,wordlist2dataframe(policy_hawk, "Hawkish Keyword", topic = "Policy"))

growth_hawk <- c("activity", "aggregate demand", "capacity utilisation", "capacity utilization",
                 "capital expenditure", "capital formation", "capital investment", "capital spending",
                 "consumption", "consumption expenditure", "consumer spending",
                 "demand", "demand side", "disposable income",
                 "domestic demand", "domestic economy", "domestic growth",
                 "economic", "economic activity", "economic conditions",
                 "economic development","economic expnasion","economic growth", "economic output",
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
                  "loan","participants" ,"securities", "us dollar")
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

############################### DOVISH Keywords ############################

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

neutral_terms <- c("all of above", "at least", "best practice", "best practices",
                   "committee stresses that", "forward contracts", "greater transparency",
                   "greater focus","remain unchanged",
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
  topic <- all_terms$topic[i]
  
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

# Replace the space with an underscore for all_terms
all_terms<- all_terms%>%
  mutate(term = str_replace_all(term, " ", "_"))


write.csv(all_terms,file = "All terms.csv", row.names=FALSE)
write.csv(fourgrams,file = "Fourgrams.csv", row.names=FALSE)
write.csv(trigrams,file = "Trigrams.csv", row.names=FALSE)
write.csv(bigrams,file = "Bigrams.csv", row.names=FALSE)


























# Clear Environment and Set Working Directory
rm(list = ls())
setwd("C:/one drive/Počítač/UNI/Bachelors thesis/Cleaned files")

# Load Required Libraries
library(dplyr)
library(stringr)
library(tidyverse)
library(readr)
library(tidytext)
library(lubridate)
library(MASS)
library(tidyr)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# Load Files
Minutes_FED <- read.csv("FOMC Minutes.csv")
Minutes_ECB <- read.csv("ECB Accounts.csv")
Decisions_FED <- read.csv("FOMC Statements.csv")
Decisions_ECB <- read.csv("ECB Decisions.csv")
All_terms <- read.csv("All terms.csv")
Fourgrams <- read.csv("Fourgrams.csv")
Trigrams <- read.csv("Trigrams.csv")
Bigrams <- read.csv("Bigrams.csv")

Four_grams <- Fourgrams %>% mutate(term = str_replace_all(term, " ", "_"))
Tri_grams <- Trigrams %>% mutate(term = str_replace_all(term, " ", "_"))
Bi_grams <- Bigrams %>% mutate(term = str_replace_all(term, " ", "_"))

# ------------------- FUNCTIONS ------------------- #

# Function: Standardize Date Format
standardize_date <- function(date_string) {
  if (grepl("-", date_string)) {
    if (grepl("\\d+-\\d+ [A-Za-z]+ \\d{4}", date_string)) {
      date_string <- sub(".*-\\s*", "", date_string)
    } else if (grepl("[A-Za-z]+ \\d+-[A-Za-z]+ \\d{1,2}, \\d{4}", date_string)) {
      date_string <- sub(".*-\\s*", "", date_string)
    }
  }
  formatted_date <- as.Date(date_string, format = "%Y-%m-%d")
  if (is.na(formatted_date)) {
    formatted_date <- as.Date(date_string, format = "%d %B %Y")
    if (is.na(formatted_date)) {
      formatted_date <- as.Date(date_string, format = "%B %d, %Y")
    }
  }
  format(formatted_date, "%Y-%m-%d")
}

# Function: Clean Text
clean_text <- function(text) {
  text %>%
    tolower() %>%
    gsub("[[:punct:]]", " ", .) %>%
    gsub("[0-9]+", " ", .) %>%
    str_squish()
}

# Function: Split Text into Sentences
sentenceSplit <- function(text_df, extra_dividers = c(";", ":", "--")) {
  all_sentences <- vector("list", nrow(text_df))
  all_docs <- vector("list", nrow(text_df))

  for (dd in seq_len(nrow(text_df))) {
    rawtext <- text_df$Text[dd]
    all_sentences[[dd]] <- unlist(strsplit(rawtext, "(?<=\[.!?\])\\s+", perl = TRUE))
    for (divider in extra_dividers) {
      all_sentences[[dd]] <- unlist(str_split(all_sentences[[dd]], divider))
    }
    all_docs[[dd]] <- rep(text_df$Date[dd], length(all_sentences[[dd]]))
  }

  data.frame(
    Date = unlist(all_docs),
    sentence = str_squish(unlist(all_sentences)),
    nchar = nchar(unlist(all_sentences))
  )
}

# Functions to Process N-Grams
process_ngrams <- function(data, ngram_terms, target_terms, n) {
  data %>%
    unnest_tokens(ngram, Cleaned_Text, token = "ngrams", n = n) %>%
    group_by(Date, sentence_id) %>%
    mutate(order = row_number(),
           ngram = if_else(ngram %in% ngram_terms$term, str_replace_all(ngram, " ", "_"), ngram),
           belongs_to_ngrams = ngram %in% target_terms$term) %>%
    summarize(merged_sentence = str_c(if_else(belongs_to_ngrams, "", word(ngram, 1)), collapse = " "), .groups = "drop")
}

# Function: Assign Scores
calculate_scores <- function(cleaned_data, term_list) {
  cleaned_data %>%
    unnest_tokens(word, merged_sentence, token = "ngrams", n = 1) %>%
    left_join(term_list, by = c("word" = "term")) %>%
    mutate(score = case_when(
      category == "Hawkish Keyword" ~ 1,
      category == "Dovish Keyword" ~ -1,
      TRUE ~ 0
    ))
}

# ------------------- DATA PROCESSING ------------------- #

# Standardize Dates
Minutes_FED <- Minutes_FED %>% mutate(Date = sapply(Date, standardize_date))
Minutes_ECB <- Minutes_ECB %>% mutate(Date = sapply(Date, standardize_date))
Decisions_FED <- Decisions_FED %>% mutate(Date = sapply(Date, standardize_date))
Decisions_ECB <- Decisions_ECB %>% mutate(Date = sapply(Date, standardize_date))

# Split Data into Sentences
Sentences_minutes_FED <- sentenceSplit(Minutes_FED) %>%
  filter(nchar >= 10) %>%
  mutate(Cleaned_Text = sapply(sentence, clean_text))

Sentences_minutes_ECB <- sentenceSplit(Minutes_ECB) %>%
  filter(nchar >= 10) %>%
  mutate(Cleaned_Text = sapply(sentence, clean_text))

Sentences_decisions_FED <- sentenceSplit(Decisions_FED) %>%
  filter(nchar >= 10) %>%
  mutate(Cleaned_Text = sapply(sentence, clean_text))

Sentences_decisions_ECB <- sentenceSplit(Decisions_ECB) %>%
  filter(nchar >= 10) %>%
  mutate(Cleaned_Text = sapply(sentence, clean_text))

# Process N-Grams
ECBD <- process_ngrams(Sentences_decisions_ECB, Fourgrams, Four_grams, 4)
ECBM <- process_ngrams(Sentences_minutes_ECB, Fourgrams, Four_grams, 4)
FEDD <- process_ngrams(Sentences_decisions_FED, Fourgrams, Four_grams, 4)
FEDM <- process_ngrams(Sentences_minutes_FED, Fourgrams, Four_grams, 4)

# ------------------- SCORE ASSIGNMENT ------------------- #

Scores_ECBD <- calculate_scores(ECBD, All_terms)
Scores_ECBM <- calculate_scores(ECBM, All_terms)
Scores_FEDD <- calculate_scores(FEDD, All_terms)
Scores_FEDM <- calculate_scores(FEDM, All_terms)

write.csv(Scores_ECBD, "Scores_ECBD.csv", row.names = FALSE)
write.csv(Scores_ECBM, "Scores_ECBM.csv", row.names = FALSE)
write.csv(Scores_FEDD, "Scores_FEDD.csv", row.names = FALSE)
write.csv(Scores_FEDM, "Scores_FEDM.csv", row.names = FALSE)

# Calculate total standardized scores and scores per topic
Final_score_ECBM <- standardized_score_and_topic(Scores_ECBM)
Final_score_ECBD <- standardized_score_and_topic(Scores_ECBD)
Final_score_FEDD <- standardized_score_and_topic(Scores_FEDD)
Final_score_FEDM <- standardized_score_and_topic(Scores_FEDM)

# Save standardized scores
write.csv(Final_score_ECBM, file = "Final_score_ECBM.csv", row.names = FALSE)
write.csv(Final_score_ECBD, file = "Final_score_ECBD.csv", row.names = FALSE)
write.csv(Final_score_FEDD, file = "Final_score_FEDD.csv", row.names = FALSE)
write.csv(Final_score_FEDM, file = "Final_score_FEDM.csv", row.names = FALSE)
                     
 # Calculate and save unstandardized scores
Unstandardized_ECBM <- calculate_final_score(Scores_ECBM)
Unstandardized_ECBD <- calculate_final_score(Scores_ECBD)
Unstandardized_FEDD <- calculate_final_score(Scores_FEDD)
Unstandardized_FEDM <- calculate_final_score(Scores_FEDM)

write.csv(Unstandardized_ECBM, file = "Unstandardized_ECBM.csv", row.names = FALSE)
write.csv(Unstandardized_ECBD, file = "Unstandardized_ECBD.csv", row.names = FALSE)
write.csv(Unstandardized_FEDD, file = "Unstandardized_FEDD.csv", row.names = FALSE)
write.csv(Unstandardized_FEDM, file = "Unstandardized_FEDM.csv", row.names = FALSE)
                     
# Create word clouds
Wordcloud_ECBD <- create_wordcloud_per_topic(Scores_ECBD, "ECB Decisions | Topic:")
Wordcloud_ECBM <- create_wordcloud_per_topic(Scores_ECBM, "ECB Accounts | Topic:")
Wordcloud_FEDD <- create_wordcloud_per_topic(Scores_FEDD, "FOMC Statements | Topic:")
Wordcloud_FEDM <- create_wordcloud_per_topic(Scores_FEDM, "FOMC Minutes | Topic:")
TOTAL_WORDCLOUD <- create_wordcloud_per_topic(All_scores, "Topic:")
