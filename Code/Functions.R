########################################### All functions used in the analysis ###########################################

############################################################
# Function to extract text from URL for ECB links (excluding navigation and links)
############################################################
extract_text_ECB <- function(link) {
  # Read the webpage content
  page <- read_html(link)
  # Remove unwanted tags (e.g., <a>, <nav>, <footer>, etc.)
  unwanted_tags <- c("//a", "//nav", "//footer")
  # Loop over unwanted tags and remove each type
  for (tag in unwanted_tags) {
    nodes_to_remove <- xml_find_all(page, tag)
    xml_remove(nodes_to_remove)  # This will remove the list of nodes correctly
  }
  # Extract the main content (focusing on <p> and <div> tags within main )
  main_content <- page %>%
    xml_find_all("//main//div[contains(@class, 'section') or contains(@class, 'orderedlist')]") %>%
    xml_text(trim = TRUE)
  # Combine the extracted text into a single block
  cleaned_text <- paste(main_content, collapse = "\n")
  return(cleaned_text)
}


############################################################
# Function to extract text from URL for FED minutes links (excluding navigation and links)
############################################################
extract_text_FED_minutes <- function(link) {
  # Read the webpage content
  page <- read_html(link)
  # Remove unwanted tags (e.g., <a>, <nav>, <footer>, etc.)
  unwanted_tags <- c("//a", "//nav", "//footer")
  # Loop over unwanted tags and remove each type
  for (tag in unwanted_tags) {
    nodes_to_remove <- xml_find_all(page, tag)
    xml_remove(nodes_to_remove)  # This will remove the list of nodes correctly
  }
  # Extract the main content (focusing on <p> and <div> tags)
  main_content <- page %>%
    xml_find_all("//p | //div[contains(@class, 'main-content')]") %>%
    xml_text(trim = TRUE)
  # Combine the extracted text into a single block
  cleaned_text <- paste(main_content, collapse = "\n")
  return(cleaned_text)
}


############################################################
# Function to clean FED text
############################################################
cleaning_FED <- function(data) {
  # Create an empty list to store the cleaned text
  cleaned_files <- vector("list", length = nrow(data))
  # Terms to remove
  to_delete <- c("For immediate release", "Stay Connected", "p.m. EDT", 
                 "p.m. EST", "Release Date:", "For release")
  # Loop through each row of the input data frame
  for (i in 1:nrow(data)) {
    # Get the text of the i-th file
    text <- data$Text[[i]]
    # Ensure text is split into lines (if it's not already)
    lines <- unlist(strsplit(text, "\n"))
    # Remove unwanted terms
    cleaned_text <- lines[!Reduce(`|`, lapply(to_delete, str_detect, string = lines))]
    # Store the cleaned text in the list
    cleaned_files[[i]] <- paste(cleaned_text, collapse = " ")  # Recombine as a single string
  }
  # Convert the list of cleaned text into a data frame
  cleaned_files_df <- data.frame(Cleaned_Text = unlist(cleaned_files), stringsAsFactors = FALSE)
  return(cleaned_files_df)
}


############################################################
# Function to extract text from both PDF and HTML URLs
############################################################
extract_text_FED_decisions <- function(link) {
  # Check if the URL is a PDF
  if (grepl("\\.pdf$", link, ignore.case = TRUE)) {
    # If the URL points to a PDF, extract the text using pdftools
    pdf_text_content <- pdf_text(link)
    cleaned_text <- paste(pdf_text_content, collapse = "\n")
  } else {
    # Otherwise, process the URL as an HTML page
    # Read the webpage content
    page <- read_html(link)
    # Remove unwanted tags from the body (e.g., <a>, <nav>, <footer>)
    unwanted_tags <- c("//a", "//nav", "//footer", "//script", "//style")
    for (tag in unwanted_tags) {
      nodes_to_remove <- xml_find_all(page, tag)
      xml_remove(nodes_to_remove)
    }
    # Extract relevant content from <tbody>, <t>, <li>, <ul>, and any <p> tags
    body_content <- page %>%
      xml_find_all("//tbody//tr//td | //t | //li | //ul | //body//p|//body//font//i") %>%
      xml_text(trim = TRUE)
    # Combine the title, body title, and body content into a single block
    cleaned_text <- paste( body_content, collapse = "\n")
  }
  return(cleaned_text)
}


############################################################
# Function to extract date from text of a URL
############################################################
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


############################################################
# Function to convert list into data frame
############################################################
wordlist2dataframe <- function(wordlist, category, topic =""){
  # Get rid of duplicates and sort the list alphabetically
  wordlist <- sort(unique(wordlist))
  # Create a data frame from the cleaned list
  wordlist_df <- data.frame("term"=wordlist,
                               "category"=rep(category,length(wordlist)),
                               "topic"=rep(topic,length(wordlist)))
  return(wordlist_df)
}


############################################################
# Function to unify the date format 
############################################################
standardize_date <- function(date_string) {
  # Case 1: Handle date ranges by extracting the later date
  if (grepl("-", date_string)) {
    if (grepl("\\d+-\\d+ [A-Za-z]+ \\d{4}", date_string)) {
      # Format "1-2 June 2016" - extract only the later part (June 2, 2016)
      date_string <- sub(".*-\\s*", "", date_string)
    } else if (grepl("[A-Za-z]+ \\d+-[A-Za-z]+ \\d{1,2}, \\d{4}", date_string)) {
      # Format "April 30-May 1, 2013" - extract only the later part (May 1, 2013)
      date_string <- sub(".*-\\s*", "", date_string)
    }
  }
  # Case 2: Handle input already in "YYYY-MM-DD" format
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", date_string)) {
    formatted_date <- as.Date(date_string, format = "%Y-%m-%d")
  } else {
    # Attempt to parse date with multiple formats
    formatted_date <- as.Date(date_string, format = "%d %B %Y")
    if (is.na(formatted_date)) {
      formatted_date <- as.Date(date_string, format = "%B %d, %Y")
    }
    if (is.na(formatted_date)) {
      formatted_date <- dmy(date_string, quiet = TRUE)
    }
    if (is.na(formatted_date)) {
      formatted_date <- mdy(date_string, quiet = TRUE)
    }
  }
  # Format to yyyy-mm-dd
  formatted_date <- format(formatted_date, "%Y-%m-%d")
  return(formatted_date)
}


############################################################
# Function to clean text to only lowercase words without punctuation
############################################################
clean_text <- function(text) {
  # Convert to lowercase
  text <- tolower(text)
  # Remove all punctuation except words, numbers, and spaces
  text <- gsub("[[:punct:]]", " ", text)
  # Remove numbers
  text <- gsub("[0-9]+", " ", text)
  # Replace multiple spaces with a single space
  text <- str_squish(text)
    return(text)
}


############################################################
# Function to split the data set into sentences 
############################################################
sentenceSplit <- function(text_df, extra_dividers = c(";", ":","--")) {
  # Placeholder lists for storing all sentences and documents
  all_sentences <- vector("list", nrow(text_df))
  all_docs <- vector("list", nrow(text_df))
  # Split each document into sentences
  for (dd in seq_len(nrow(text_df))) {
    rawtext <- text_df$Text[dd]
    # Split sentence using boundaries
    all_sentences[[dd]] <- unlist(strsplit(rawtext, "(?<=[.!?])\\s+", perl = TRUE))
    # For any extra dividers specified, split up the sentences further
    for (divider in extra_dividers) {
      split_again <- unlist(str_split(all_sentences[[dd]], divider))
      all_sentences[[dd]] <- split_again
    }
    # Repeat document date for each sentence
    all_docs[[dd]] <- rep(text_df$Date[dd], length(all_sentences[[dd]]))
  }
  # Flatten the lists into vectors
  docsVector <- unlist(all_docs)
  sentencesVector <- str_squish(unlist(all_sentences))  # Remove extra spaces
  # Check that the lengths of docsVector and sentencesVector match
  stopifnot(length(docsVector) == length(sentencesVector))
  # Create DataFrame and merge back in original information
  sentence_df <- data.frame(Date = docsVector, sentence = sentencesVector)
  # Calculate character length of each sentence
  sentence_df$nchar <- nchar(sentence_df$sentence)
  return(sentence_df)
}


############################################################
# Function to process Fourgrams
############################################################
process_fourgrams <- function(data, fourgrams_terms, four_grams_terms) {
  data %>%
    unnest_tokens(fourgram, Cleaned_Text, token = "ngrams", n = 4) %>%
    group_by(Date, sentence_id) %>%
    mutate(order = row_number()) %>%
    # Check and replace fourgrams based on fourgrams_terms
    mutate(fourgram = if_else(fourgram %in% fourgrams_terms$term, 
                              str_replace_all(fourgram, " ", "_"), 
                              fourgram)) %>%
    arrange(Date, sentence_id, order) %>%
    # Check if fourgram contains underscores and set skip logic
    mutate(belongs_to_fourgrams = fourgram %in% four_grams_terms$term) %>%
    # Determine merged_text based on row position and fourgram status
    mutate(merged_text = case_when(
      # Skip rows based on belongs_to_fourgrams logic
      lag(belongs_to_fourgrams, default = FALSE) ~ "",    # Skip 1st row after underscore
      lag(belongs_to_fourgrams, 2, default = FALSE) ~ "", # Skip 2nd row after underscore
      lag(belongs_to_fourgrams, 3, default = FALSE) ~ "", # Skip 3rd row after underscore
      # If it's the last row of the sentence_id group, take the whole fourgram
      is.na(lead(sentence_id)) | lead(sentence_id) != sentence_id ~ fourgram,
      # Otherwise, take the first word of the fourgram
      TRUE ~ word(fourgram, 1)
    )) %>%
    # Keep rows with underscores and remove skipped rows
    filter(merged_text != "" | belongs_to_fourgrams) %>%
    # Concatenate the merged text into one sentence per Date and sentence_id
    summarize(merged_sentence = str_c(merged_text, collapse = " "), .groups = 'drop')
}


############################################################
# Function to process Trigrams
############################################################
process_trigrams <- function(data, trigrams_terms, tri_grams_terms) {
  data %>%
    unnest_tokens(trigram, merged_sentence, token = "ngrams", n = 3) %>%
    group_by(Date, sentence_id) %>%
    mutate(order = row_number()) %>%
    # Check and replace trigrams based on trigrams_terms
    mutate(trigram = if_else(trigram %in% trigrams_terms$term, 
                             str_replace_all(trigram, " ", "_"), 
                             trigram)) %>%
    arrange(Date, sentence_id, order) %>%
    # Check if trigram contains underscores and set skip logic
    mutate(belongs_to_trigrams = trigram %in% tri_grams_terms$term) %>%
    # Determine merged_text based on row position and trigram status
    mutate(merged_text = case_when(
      lag(belongs_to_trigrams, default = FALSE) ~ "",    # Skip 1st row after underscore
      lag(belongs_to_trigrams, 2, default = FALSE) ~ "", # Skip 2nd row after underscore
      # If it's the last row of the sentence_id group, take the whole trigram
      is.na(lead(sentence_id)) | lead(sentence_id) != sentence_id ~ trigram,
      # Otherwise, take the first word of the trigram
      TRUE ~ word(trigram, 1)
    )) %>%
    # Keep rows with underscores and remove skipped rows
    filter(merged_text != "" | belongs_to_trigrams) %>%
    # Concatenate the merged text into one sentence per Date and sentence_id
    summarize(merged_sentence = str_c(merged_text, collapse = " "), .groups = 'drop')
}


############################################################
# Function to process Bigrams
############################################################
process_bigrams <- function(data, bigrams_terms, bi_grams_terms) {
  data %>%
    unnest_tokens(bigram, merged_sentence, token = "ngrams", n = 2) %>%
    group_by(Date, sentence_id) %>%
    mutate(order = row_number()) %>%
    # Check and replace bigrams based on bigrams_terms
    mutate(bigram = if_else(bigram %in% bigrams_terms$term, 
                            str_replace_all(bigram, " ", "_"), 
                            bigram)) %>%
    arrange(Date, sentence_id, order) %>%
    # Check if bigram contains underscores and set skip logic
    mutate(belongs_to_bigrams = bigram %in% bi_grams_terms$term) %>%
    # Determine merged_text based on row position and bigram status
    mutate(merged_text = case_when(
      lag(belongs_to_bigrams, default = FALSE) ~ "",    # Skip 1st row after underscore
      # If it's the last row of the sentence_id group, take the whole bigram
      is.na(lead(sentence_id)) | lead(sentence_id) != sentence_id ~ bigram,
      # Otherwise, take the first word of the bigram
      TRUE ~ word(bigram, 1)
    )) %>%
    # Keep rows with underscores and remove skipped rows
    filter(merged_text != "" | belongs_to_bigrams) %>%
    # Concatenate the merged text into one sentence per Date and sentence_id
    summarize(merged_sentence = str_c(merged_text, collapse = " "), .groups = 'drop')
}


############################################################
# Function that assigns a score to all modifier and keyword combination
############################################################
calculate_scores <- function(Clean_file, All_terms) {
  # Step 1: Process and clean tokens
  Term <- Clean_file %>%
    unnest_tokens(word, merged_sentence, token = "ngrams", n = 1) %>%
    group_by(Date, sentence_id) %>%
    mutate(order = row_number()) %>%
    mutate(word = if_else(word %in% All_terms$term, word, NA_character_)) %>%
    filter(!is.na(word)) %>%
    left_join(All_terms %>% dplyr::select(term, category, topic), by = c("word" = "term"), relationship = "many-to-many")
  
  # Step 2: Identify categories
  keywords <- Term %>% filter(category %in% c("Hawkish Keyword", "Dovish Keyword"))
  modifiers <- Term %>% filter(category %in% c("High modifier", "Low modifier", "Positive modifier", "Negative modifier", "Neutral Phrase"))
  negators <- Term %>% filter(category == "Negator")
  
  # Step 3: Calculate distances between modifiers and keywords
  Score_with_distances <- modifiers %>%
    left_join(keywords, by = c("Date", "sentence_id", "word_count"), suffix = c("_modifier", "_keyword"), relationship = "many-to-many") %>%
    left_join(
      negators %>% 
        rename_with(~ paste0(., "_negator"), 
                    .cols = -c(Date, sentence_id, word_count)), # Exclude join keys
      by = c("Date", "sentence_id", "word_count"), 
      relationship = "many-to-many"
    ) %>%
    mutate(distance = abs(order_modifier - order_keyword)) %>%
    mutate(distance_negator = abs(order_negator - order_modifier)) %>%
    group_by(Date, sentence_id, word_modifier) %>%
    filter(
      is.na(order_negator) |
        is.na(order_keyword) |  # Include rows with no keyword
        distance == min(distance) |  # Include rows with the closest distance
        (distance == min(distance) & order_keyword < order_modifier) # Resolve ties
    ) %>%
    ungroup()
  
  # Step 4: Assign scores and handle cases without keywords
  Score <- Score_with_distances %>%
    group_by(Date, sentence_id) %>%
    mutate(
      has_keyword = any(category_keyword %in% c("Hawkish Keyword", "Dovish Keyword")),
      negator_in_front = if_else(
        !is.na(distance_negator) & distance_negator == 1 & order_negator < order_modifier,
        TRUE,
        FALSE
      )
    ) %>% # End mutate block
    ungroup() %>%
    mutate(
      category_keyword = if_else(!has_keyword, NA_character_, category_keyword),
      word_keyword = if_else(!has_keyword, NA_character_, word_keyword),
      score = case_when(
        # Neutral phrases score zero
        category_modifier == "Neutral Phrase" & category_keyword == "Hawkish Keyword" ~ 0,
        category_modifier == "Neutral Phrase" & category_keyword == "Dovish Keyword" ~ 0,
        
        # No keywords but modifiers are present
        !has_keyword  & category_modifier == "High modifier" ~ 1,
        !has_keyword  & category_modifier == "Positive modifier" ~ 1,
        !has_keyword  & category_modifier == "Low modifier" ~ -1,
        !has_keyword  & category_modifier == "Negative modifier" ~ -1,
        
        # Modifiers and keywords combinations
        category_modifier == "High modifier" & category_keyword == "Hawkish Keyword" ~ 1,
        category_modifier == "High modifier" & category_keyword == "Dovish Keyword" ~ -1,
        category_modifier == "Low modifier" & category_keyword == "Hawkish Keyword" ~ -1,
        category_modifier == "Low modifier" & category_keyword == "Dovish Keyword" ~ 1,
        
        category_modifier == "Positive modifier" & category_keyword == "Hawkish Keyword" & !topic_keyword %in% c("Prices", "Policy") ~ 1,
        category_modifier == "Positive modifier" & category_keyword == "Dovish Keyword" & !topic_keyword %in% c("Prices", "Policy") ~ -1,
        category_modifier == "Positive modifier" & category_keyword == "Dovish Keyword" & topic_keyword == "Prices" ~ 1,
        category_modifier == "Positive modifier" & topic_keyword == "Policy" ~ -1,
        category_modifier == "Positive modifier" & category_keyword == "Hawkish Keyword" & topic_keyword == "Prices" ~ -1,
        
        category_modifier == "Negative modifier" & category_keyword == "Hawkish Keyword" & !topic_keyword %in% c("Prices", "Policy") ~ -1,
        category_modifier == "Negative modifier" & category_keyword == "Dovish Keyword" & !topic_keyword %in% c("Prices", "Policy") ~ 1,
        category_modifier == "Negative modifier" & category_keyword == "Dovish Keyword" & topic_keyword == "Prices" ~ -1,
        category_modifier == "Negative modifier" & category_keyword == "Hawkish Keyword" & topic_keyword == "Prices" ~ 1,
        category_modifier == "Negative modifier" & topic_keyword == "Policy" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    # Step 5: Reverse score if negator is in front
    mutate(score = if_else(negator_in_front, -score, score))
  
  return(Score)
}

############################################################
# Function to calculate a score per document (not standardized and not per topic)
############################################################
calculate_final_score <- function(data) {
  sentence_scores <- data %>%
    group_by(Date, sentence_id) %>%
    summarize(
      summed_score = sum(score, na.rm = TRUE),
      word_count = first(word_count),
      .groups = 'drop'
    )%>%
    mutate(sentence_score = summed_score / word_count)
  
  sentence_count <- data %>%
    group_by(Date) %>%
    summarise(
      num_sentences = n_distinct(sentence_id),  # Count unique sentence IDs
      .groups = 'drop'
    )
  
  total_scores <- sentence_scores %>%
    left_join(sentence_count, by = "Date")%>%
    group_by(Date) %>%
    summarise(final_score = round(sum(sentence_score, na.rm = TRUE) / first(num_sentences), 3),  # Use num_sentences in the calculation
              .groups = 'drop'
    )
  return(total_scores)
}


############################################################
# Function to calculate a standardized score per document and per topic
############################################################
standardized_score_and_topic <- function(data) {
  # Step 1: Calculate sentence-level scores
  calculate_sentence_scores <- function(data) {
    data %>%
      group_by(Date, sentence_id, topic_keyword) %>%
      summarise(
        word_count = first(word_count),  # Retain word count per sentence
        score = sum(score, na.rm = TRUE) / word_count,  # Average score per topic per sentence
        .groups = 'drop'
      ) %>%
      group_by(Date, sentence_id) %>%
      mutate(sentence_score = sum(score, na.rm = TRUE)) %>%  # Total score per sentence
      ungroup()
  }
  # Step 2: Count sentences per document
  count_sentences <- function(data) {
    data %>%
      group_by(Date) %>%
      summarise(
        num_sentences = n_distinct(sentence_id),  # Count unique sentence IDs
        .groups = 'drop'
      )
  }
  # Step 3: Calculate topic fractions
  calculate_topic_fractions <- function(sentence_data, sentence_count) {
    sentence_data %>%
      left_join(sentence_count, by = "Date") %>%
      group_by(Date, sentence_id) %>%
      mutate(num_topics_per_sentence = n_distinct(topic_keyword)) %>%
      ungroup() %>%
      group_by(Date, topic_keyword) %>%
      mutate(
        topic_fraction_sentence = ifelse(
          sentence_score == 0 & score == 0, 
          1 / num_topics_per_sentence, 
          ifelse(sentence_score == 0, 
                 1 / num_topics_per_sentence, 
                 score / sentence_score)
        ),
        topic_fraction = sum(topic_fraction_sentence, na.rm = TRUE) / num_sentences
      ) %>%
      dplyr::select(Date, topic_keyword, topic_fraction) %>%
      distinct()
  }
  # Step 4: Calculate document scores
  calculate_document_scores <- function(sentence_data, sentence_count) {
    sentence_data %>%
      distinct(Date, sentence_id, .keep_all = TRUE) %>%  # Deduplicate sentence scores
      left_join(sentence_count, by = "Date") %>%
      group_by(Date) %>%
      summarise(
        total_document_score = round(sum(sentence_score, na.rm = TRUE) / first(num_sentences), 3),  # Average per document
        .groups = 'drop'
      ) %>%
      mutate(Standardized_score = (total_document_score - mean(total_document_score)) / sd(total_document_score))
  }
  # Step 5: Calculate standardized topic scores
  calculate_topic_scores <- function(topic_fraction, document_score) {
    topic_fraction %>%
      left_join(document_score, by = "Date") %>%
      mutate(standardized_topic_score = Standardized_score * topic_fraction) %>%
      dplyr::select(Date, topic_keyword, standardized_topic_score, Standardized_score) %>%
      tidyr::pivot_wider(
        names_from = topic_keyword, 
        values_from = standardized_topic_score,
        values_fill = 0  # Fill missing values with 0
      )
  }
  # Main process
  sentence_scores <- calculate_sentence_scores(data)
  sentence_count <- count_sentences(data)
  topic_fraction <- calculate_topic_fractions(sentence_scores, sentence_count)
  document_scores <- calculate_document_scores(sentence_scores, sentence_count)
  standardized_topic_scores <- calculate_topic_scores(topic_fraction, document_scores)
  
  return(standardized_topic_scores)
}



############################################################
# Function to assing the most commonly discussed topic per document, dummy variable creation
############################################################
most_common_topics_dummy <- function(data) {
  # Step 1: Group by Date and topic_keyword, and summarize the count
  summarized_data <- data %>%
    group_by(Date, topic_keyword) %>%
    summarize(count = n(), .groups = 'drop')
  
  # Step 2: Filter for the most common topic(s) per Date
  most_common <- summarized_data %>%
    group_by(Date) %>%
    filter(count == max(count)) %>%
    ungroup()
  
  # Step 3: Resolve ties by randomly selecting one topic per Date
  most_common <- most_common %>%
    group_by(Date) %>%
    slice_sample(n = 1) %>%  # Randomly select one row per Date
    ungroup()
  
  # Step 4: Create dummy variables for `topic_keyword`
  most_common$topic_keyword <- as.factor(most_common$topic_keyword)
  topic_dummies <- model.matrix(~ topic_keyword - 1, data = most_common)
  
  # Step 5: Add the dummy variables back to the most_common data frame
  result <- cbind(most_common, as.data.frame(topic_dummies))
  
  # Return the updated data frame
  return(result)
}



############################################################
# Function to retain word count per document from sentence word counts
############################################################
process_word_count <- function(data) {
  data %>%
    group_by(Date) %>%
    mutate(Document_word_count = sum(word_count)) %>%
    distinct(Date, .keep_all = TRUE) %>%
    dplyr::select(Date, Document_word_count)
}


############################################################
# Function to calculate sentiment and uncertainty score from text
############################################################
sentiment <- function(data) {
  word_classification <- data %>%
    mutate(Text = tolower(Text)) %>%  # Convert text to lowercase
    unnest_tokens(word, Text) %>%
    inner_join(get_sentiments("loughran"), by = "word", relationship = "many-to-many") %>%
    group_by(Date, sentiment) %>%
    summarize(count = n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = sentiment, values_from = count, values_fill = list(count = 0))
  sentiment_score<- word_classification%>%
    mutate(rowsum = positive + negative + uncertainty + constraining + litigious + superfluous)%>%
    mutate(Sentiment_score = ((positive-negative)/rowsum))%>%
    mutate(Standardized_sentiment_score = (Sentiment_score - mean(Sentiment_score)/sd(Sentiment_score)))%>%
    mutate(Uncertainty_score = (uncertainty/rowsum))%>%
    mutate(Standardized_uncertainty_score = (Uncertainty_score - mean(Uncertainty_score)/sd(Uncertainty_score)))%>%
    dplyr::select(Date,Standardized_sentiment_score, Standardized_uncertainty_score)
  
  return(sentiment_score)
}


############################################################
# Function to create word clouds of frequent words 
############################################################
create_wordcloud_per_topic <- function(data, x) {
  # Summarize data to get the frequency of each word per topic
  summarized_data <- data %>%
    group_by(topic_keyword, word_keyword) %>%
    summarize(frequency = n(), .groups = "drop")
  # Get the unique topics
  unique_topics <- unique(summarized_data$topic_keyword)
  # Define shades of blue for the word cloud, skipping the lightest blue
  blue_shades <- brewer.pal(9, "Blues")[4:9]  # Start from a slightly darker shade
  # Loop through each topic and create a word cloud
  for (topic in unique_topics) {
    # Filter data for the current topic
    topic_data <- summarized_data %>%
      filter(topic_keyword == topic)
    # Replace underscores with spaces for better readability
    topic_data$word_keyword <- gsub("_", " ", topic_data$word_keyword)
    # Adjust size of the plot
    par(plt = c(0.1, 0.8, 0.1, 0.8))  # Adjust the plot to fill more space
    # Create the word cloud with larger text and increased spacing
    wordcloud(
      words = topic_data$word_keyword,
      freq = topic_data$frequency,
      min.freq = 1,
      scale = c(2, 1),  # Increase the scale for larger text
      colors = blue_shades,  # Use darker shades of blue
      random.order = FALSE,  # Ensure words are not randomly placed
      rot.per = 0,           # Make all words horizontal
      max.words = Inf,       # Allow as many words as needed
      random.color = FALSE   # Ensure consistent colors
    )
    # Add a title to the plot based on the topic
    title(main = paste(x, topic), line = 0, cex.main = 1.2)  # Adjust title size and position
  }
}


############################################################
# Function to create lagged observations
############################################################
create_lagged_columns <- function(data, column_name, max_lag) {
  # Iterate over the range of lags
  for (lag in 1:max_lag) {
    # Create the lagged column
    lagged_column_name <- paste0(column_name, "_T", lag)
    data[[lagged_column_name]] <- dplyr::lag(data[[column_name]], n = lag)
  }
  return(data)
}


############################################################
# Function for different readability measures
############################################################
my_readability <- function (data){
  data%>%
  rowwise() %>%
  mutate(
    Flesch_Kincaid = textstat_readability(Text, measure = "Flesch.Kincaid")$Flesch.Kincaid) %>%
  ungroup()
}


