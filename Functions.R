########################################### All functions used in the analysis ###########################################
######################## Function to extract text from URL for ECB links (excluding navigation and links)
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


######################## Function to extract text from URL for FED minutes links (excluding navigation and links)
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


######################## Function to clean FED text
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


######################## Function to extract text from both PDF and HTML URLs
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


### Function to extract date from text of a URL
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

### Function to convert list into data frame
wordlist2dataframe <- function(wordlist, category, topic =""){
  # Get rid of duplicates and sort the list alphabetically
  wordlist <- sort(unique(wordlist))
  # Create a data frame from the cleaned list
  wordlist_df <- data.frame("term"=wordlist,
                               "category"=rep(category,length(wordlist)),
                               "topic"=rep(topic,length(wordlist)))
  return(wordlist_df)
}
