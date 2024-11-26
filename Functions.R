########################################### All functions used in the analysis ###########################################

### Function to extract text from URL for ECB links (excluding navigation and links)
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
