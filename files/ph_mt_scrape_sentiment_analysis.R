###############################################################
# Project: Investing in Narratives                            #
# Task: Scrape Manila Times Op-ed w/ "China" Aug 23,2024-     #
#       Jan 1, 2010; use Lexicoder Sentiment Dictionary (LSD) #
#       to get positive/negative mention                      #
# Author: jas                                                 #
# IDE and R Version: R Studio; 4.3.1                          #
###############################################################

# ARCHIVED RESEARCH CODE
# Original analytical workflow developed in August 2024.
# This script documents the methods used in the original research.
# It has not been retested with current software or package versions.
# Authentication credentials have been removed.
# Internal directory paths have been anonymized.
# The analysis used the Lexicoder Sentiment Dictionary (LSD2015)
# available through the quanteda R package in August 2024.
# The script references the Lexicoder preprocessing file
# (LSDpreprocess2015v2.txt), which is not included here.
# Due to a formatting mismatch, these preprocessing rules
# may not have executed as intended. The effect on the
# original sentiment estimates has not been assessed. ~11/09/2026

# Call R version
R.Version()

# Clear workspace
rm(list=ls())

#Load libraries
library(rvest)
library(httr)
library(stringr)  # Load stringr for easy text searching/string manipulation
library(textstem)   # For lemmatization
library(quanteda)   # For text analysis and sentiment
library(dplyr)    # For data manipulation
library(tidyr)    # For separating columns
library(readr)
library(ggplot2)

##################
# Single article #
##################

# Define the login URL and credentials
login_url <- "https://www.manilatimes.net/premium"  # Update this with the actual login URL
username <- "[enter username]"
password <- "[enter password]"

# Create a session and load the login page
session <- html_session(login_url)

# Identify the login form by its id "signInForm"
login_form <- html_form(session)[[1]]  # Assuming it's the first form on the page

# If the form lacks an action attribute, manually set it
login_form$action <- "https://www.manilatimes.net/premium"  # Replace with the correct action URL

# Fill in the login form with your credentials
filled_form <- login_form %>%
  html_form_set(
    login_input = username,   # Replace 'login_input' with the actual name of the username field if different
    password = password       # Replace 'password' with the actual name of the password field if different
  )

# Submit the login form
logged_in_session <- submit_form(session, filled_form)

# Now, use the authenticated session to access the restricted content
url <- "https://www.manilatimes.net/2024/08/21/opinion/contributors/china-pulse-high-quality-development-and-high-standard-opening-up/1965522"
page <- read_html(logged_in_session %>% jump_to(url))

# Extract the title of the article using the correct CSS selector
title <- page %>% html_node("h1.article-title") %>% html_text(trim = TRUE)

# If the simple selector doesn't work, use the full class list
# title <- page %>% html_node("h1.article-title.font-700.roboto-slab-3.tdb-title-text") %>% html_text(trim = TRUE)

# Extract the main content of the article
content <- page %>% html_nodes("div.article-body p") %>% html_text(trim = TRUE)

# Combine the content paragraphs into a single text
full_content <- paste(content, collapse = "\n")

# Print the extracted title and content
cat("Title: ", title, "\n\n")
cat("Content: ", full_content, "\n")

###############################
# Multiple articles, one page #
###############################

# Clear workspace
rm(list=ls())

# Set the working directory
setwd("[file path]")

# Define the login URL and credentials
login_url <- "https://www.manilatimes.net/premium"  # Update this with the actual login URL
username <- "[enter username]"
password <- "[enter password]"

# Create a session and load the login page
session <- html_session(login_url)

# Identify the login form by its id "signInForm"
login_form <- html_form(session)[[1]]  # Assuming it's the first form on the page

# If the form lacks an action attribute, manually set it
login_form$action <- "https://www.manilatimes.net/premium"  # Replace with the correct action URL

# Fill in the login form with your credentials
filled_form <- login_form %>%
  html_form_set(
    login_input = username,   # Replace 'login_input' with the actual name of the username field if different
    password = password       # Replace 'password' with the actual name of the password field if different
  )

# Submit the login form
logged_in_session <- submit_form(session, filled_form)

# Now, use the authenticated session to access the page containing multiple articles
url <- "https://www.manilatimes.net/opinion/page/2"
page <- read_html(logged_in_session %>% jump_to(url))

# Extract all the article links on the page
article_links <- page %>% html_nodes("div.article-title-h4 a") %>% html_attr("href")

# Loop through each article link
for (link in article_links) {
  # Check if the link is a full URL or a relative path
  if (grepl("^https?://", link)) {
    article_url <- link
  } else {
    article_url <- paste0("https://www.manilatimes.net", link)
  }
  
  # Print the URL being processed
  cat("Processing URL: ", article_url, "\n")
  
  # Skip the iteration if the URL is NA
  if (is.na(article_url) || article_url == "") {
    cat("Invalid URL found. Skipping...\n")
    next
  }
  
  # Access the article page
  article_page <- read_html(logged_in_session %>% jump_to(article_url))
  
  # Extract the title of the article
  title <- article_page %>% html_node("h1.article-title") %>% html_text(trim = TRUE)
  
  # Extract the main content of the article
  content <- article_page %>% html_nodes("div.article-body p") %>% html_text(trim = TRUE)
  
  # Combine the content paragraphs into a single text
  full_content <- paste(content, collapse = "\n")
  
  # Create a valid filename from the title (replace spaces with underscores)
  filename <- paste0(gsub("[^a-zA-Z0-9]", "_", title), ".txt")
  
  # Save the article content to a text file
  writeLines(c(paste("Title:", title), full_content), con = filename)
  
  # Print a message indicating the article has been saved
  cat("Saved:", filename, "\n")
}

####################################
# Only articles that mention China #
####################################

# Set the working directory
setwd("[file path]")

# Call R version
R.Version()

# Clear workspace
rm(list=ls())

# Define the login URL and credentials
login_url <- "https://www.manilatimes.net/premium"  # Update this with the actual login URL
username <- "[enter username]"
password <- "[enter password]"

# Create a session and load the login page
session <- html_session(login_url)

# Identify the login form by its id "signInForm"
login_form <- html_form(session)[[1]]  # Assuming it's the first form on the page

# If the form lacks an action attribute, manually set it
login_form$action <- "https://www.manilatimes.net/premium"  # Replace with the correct action URL

# Fill in the login form with your credentials
filled_form <- login_form %>%
  html_form_set(
    login_input = username,   # Replace 'login_input' with the actual name of the username field if different
    password = password       # Replace 'password' with the actual name of the password field if different
  )

# Submit the login form
logged_in_session <- submit_form(session, filled_form)

# Now, use the authenticated session to access the page containing multiple articles
url <- "https://www.manilatimes.net/opinion/page/2"
page <- read_html(logged_in_session %>% jump_to(url))

# Extract all the article links on the page
article_links <- page %>% html_nodes("div.article-title-h4 a") %>% html_attr("href")

# Loop through each article link
for (link in article_links) {
  # Use the link directly since it's a full URL
  article_url <- link
  
  # Print the URL being processed
  cat("Processing URL: ", article_url, "\n")
  
  # Skip the iteration if the URL is NA
  if (is.na(article_url) || article_url == "") {
    cat("Invalid URL found. Skipping...\n")
    next
  }
  
  # Access the article page
  article_page <- read_html(logged_in_session %>% jump_to(article_url))
  
  # Extract the title of the article
  title <- article_page %>% html_node("h1.article-title") %>% html_text(trim = TRUE)
  
  # Extract the main content of the article
  content <- article_page %>% html_nodes("div.article-body p") %>% html_text(trim = TRUE)
  
  # Combine the content paragraphs into a single text
  full_content <- paste(content, collapse = "\n")
  
  # Check if the article contains the word "China"
  if (str_detect(full_content, "\\bChina\\b")) {
    # Create a valid filename from the title (replace spaces with underscores)
    filename <- paste0(gsub("[^a-zA-Z0-9]", "_", title), ".txt")
    
    # Add the "Link:" and the article URL at the end of the content
    full_content_with_link <- paste(full_content, "\n\nLink:", article_url)
    
    # Save the article content to a text file in the specified directory
    writeLines(c(paste("Title:", title), full_content_with_link), con = filename)
    
    # Print a message indicating the article has been saved
    cat("Saved:", filename, "\n")
  } else {
    # Print a message indicating the article was skipped
    cat("Skipped (no mention of 'China'):", title, "\n")
  }
}

###############################################################
# Multiple pages, only mention China Aug 23, 2024-Jan 1, 2010 #
###############################################################

# Set the working directory
setwd("[file path]")

# Clear workspace
rm(list=ls())

# Define the login URL and credentials
login_url <- "https://www.manilatimes.net/premium"  # Update this with the actual login URL
username <- "[enter username]"
password <- "[enter password]"

# Create a session and load the login page
session <- html_session(login_url)

# Identify the login form by its id "signInForm"
login_form <- html_form(session)[[1]]  # Assuming it's the first form on the page

# If the form lacks an action attribute, manually set it
login_form$action <- "https://www.manilatimes.net/premium"  # Replace with the correct action URL

# Fill in the login form with your credentials
filled_form <- login_form %>%
  html_form_set(
    login_input = username,   # Replace 'login_input' with the actual name of the username field if different
    password = password       # Replace 'password' with the actual name of the password field if different
  )

# Submit the login form
logged_in_session <- submit_form(session, filled_form)

# Initialize the counter for files with "China"
file_counter <- 1

# Function to safely fetch a webpage with retry logic
safe_fetch <- function(url, retries = 3, sleep_time = 5) {
  for (i in 1:retries) {
    result <- tryCatch({
      read_html(logged_in_session %>% jump_to(url))
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(result)) {
      return(result)
    } else {
      cat("Failed to load page:", url, "Retrying in", sleep_time, "seconds...\n")
      Sys.sleep(sleep_time)
    }
  }
  cat("Failed to load page after", retries, "attempts:", url, "\n")
  return(NULL)
}

# Function to check if a link should be excluded
should_exclude <- function(link) {
  # List of specific URLs to exclude, these are stay, non-Opeds
  exclude_urls <- c(
    "https://www.manilatimes.net/2024/08/24/news/cambodias-high-level-official-to-visit-ph-strengthen-bilateral-ties/1966487",
    "https://www.manilatimes.net/2024/08/24/news/ph-calls-on-china-to-cease-all-dangerous-actions-after-second-flare-incident-over-wps/1966484",
    "https://www.manilatimes.net/2024/08/24/videos/watchchina-fires-flares-at-passing-ph-cessna-plane-over-west-philippine-sea/1966483",
    "https://www.manilatimes.net/2024/08/24/tmt-newswire/pr-newswire/sustainable-tourism-creating-future-ite-hcmc-2024-positions-itself-as-asias-premier-tourism-cooperation-event/1966512",
    "https://www.manilatimes.net/2024/08/24/business/green-industries/coastal-greenbelts-vs-massive-flooding/1966443",
    "https://www.manilatimes.net/2024/08/22/campus-press/metrobank-gt-foundations-celebrate-success-of-pandemic-era-scholars/1965869",
    "https://www.manilatimes.net/2024/08/22/business/stock-watch/stock-indices-as-of-august-21-2024/1965879",
    "https://www.manilatimes.net/2024/08/24/world/russia-ukraine-exchange-230-prisoners-of-war/1966513",
    "https://www.manilatimes.net/2024/08/24/sports/psc-chief-bachmann-urges-fans-to-support-ph-paralympians/1966514",
    "https://www.manilatimes.net/2024/08/24/sports/hd-spikers-survive-capital1-tushovas-50-point-explosion-punch-semis-ticket/1966515",   
    "https://www.manilatimes.net/2024/08/24/sports/victolero-says-magnolia-needs-to-be-more-consistent-on-defense/1966517",
    "https://www.manilatimes.net/2024/08/24/legal-advice/landowner-may-not-forcibly-eject-a-usurper/1966410",
    "https://www.manilatimes.net/2024/08/24/sports/aaron-fuller-double-double-lifts-ros-past-ginebra-in-candon-city/1966518"
    )
  
  return(link %in% exclude_urls)
}

# Loop through the pages you want to scrape (2 to 965)
for (page_num in 2:965) {
  
  # Construct the URL for each page
  url <- paste0("https://www.manilatimes.net/opinion/page/", page_num)
  
  # Print the URL being processed
  cat("Processing page:", url, "\n")
  
  # Safely fetch the page with retry logic
  page <- safe_fetch(url)
  
  if (is.null(page)) next
  
  # Extract all the article links on the page
  article_links <- page %>% html_nodes("div.article-title-h4 a") %>% html_attr("href")
  
  # Loop through each article link
  for (link in article_links) {
    # Print the URL being processed
    cat("Processing URL: ", link, "\n")
    
    # Skip the iteration if the URL is NA or matches an exclusion pattern
    if (is.na(link) || link == "" || should_exclude(link)) {
      cat("Excluded or invalid URL:", link, "\n")
      next
    }
    
    # Safely fetch the article page with retry logic
    article_page <- safe_fetch(link)
    
    if (is.null(article_page)) next
    
    # Extract the title of the article
    title <- article_page %>% html_node("h1.article-title") %>% html_text(trim = TRUE)
    
    # Extract the main content of the article
    content <- article_page %>% html_nodes("div.article-body p") %>% html_text(trim = TRUE)
    
    # Combine the content paragraphs into a single text
    full_content <- paste(content, collapse = "\n")
    
    # Count the occurrences of "China" in the content
    china_count <- str_count(full_content, "\\bChina\\b")
    
    # Check if the article contains the word "China" X times or more
    if (china_count >= 1) {
      # Clean the title for the filename: Remove special characters and limit length to 100 characters
      cleaned_title <- gsub("[^a-zA-Z0-9]", "_", title)
      cleaned_title <- str_sub(cleaned_title, 1, 100)
      
      # Create the file name with the counter
      filename <- paste0(file_counter, "_", cleaned_title, ".txt")
      
      # Add the "Link:" and the article URL at the end of the content
      full_content_with_link <- paste(full_content, "\n\nLink:", link)
      
      # Save the article content to a text file in the specified directory
      tryCatch({
        writeLines(c(paste("Title:", title), full_content_with_link), con = filename)
        cat("Saved:", filename, "\n")
        # Increment the counter after saving
        file_counter <- file_counter + 1
      }, error = function(e) {
        cat("Failed to save file:", filename, "\n")
      })
      
    } else {
      # Print a message indicating the article was skipped
      cat("Skipped (less than 1 mentions of 'China'):", title, "(", china_count, "mentions)\n")
    }
    
    # Pause between requests to avoid overloading the server
    Sys.sleep(1.5)  # Pause for 2 seconds
  }
}

###########################
# Results: 2,087 articles #
###########################

#Grabbed a bunch of stray articles randomly places, now sort only links with `/opinion/`

# Set the working directory to where .txt files are stored
setwd("[file path]")

# Specify the destination directories
opinion_dir <- "[file path]"  # Replace with the path for files containing "/opinion/"
no_opinion_dir <- "[file path]"  # Replace with the path for files not containing "/opinion/"

# Ensure both destination directories exist
if (!dir.exists(opinion_dir)) {
  dir.create(opinion_dir)
}

if (!dir.exists(no_opinion_dir)) {
  dir.create(no_opinion_dir)
}

# List all .txt files in the directory
file_list <- list.files(pattern = "*.txt")

# Loop through each file and check for "/opinion/"
for (file_name in file_list) {
  # Read the content of the file
  file_content <- readLines(file_name, warn = FALSE)
  
  # Check if "/opinion/" exists in the content
  if (any(grepl("/opinion/", file_content, fixed = TRUE))) {
    # If found, move the file to the opinion directory
    file.rename(file_name, file.path(opinion_dir, file_name))
    cat("Moved to Opinion Folder:", file_name, "\n")
  } else {
    # If not found, move the file to the no opinion directory
    file.rename(file_name, file.path(no_opinion_dir, file_name))
    cat("Moved to No Opinion Folder:", file_name, "\n")
  }
}

######################################
# Opinion: 1,552 || Non-Opinion: 535 #
######################################

########################
#~ Sentiment Analysis ~#
########################

# Clear workspace
rm(list=ls())

# Set the working directory
setwd("[file path]")

# List all .txt files in the directory
file_list <- list.files(pattern = "*.txt")

# Load the LSD preprocessing rules and filter out 'punc' and 'caps' modules
preprocess_rules <- readLines("[file path]", encoding = "UTF-8")

# Function to apply LSD preprocessing rules with error handling for UTF-8
apply_lsd_preprocessing <- function(text, rules) {
  for (rule in rules) {
    # Skip lines that are not valid UTF-8
    if (!validUTF8(rule)) {
      cat("Skipping invalid UTF-8 rule:", rule, "\n")
      next
    }
    
    # Split the rule into the pattern and the replacement
    rule_parts <- strsplit(rule, " -> ", fixed = TRUE)[[1]]
    
    if (length(rule_parts) == 2) {
      pattern <- rule_parts[1]
      replacement <- rule_parts[2]
      
      # Apply the rule to the text
      text <- gsub(pattern, replacement, text)
    }
  }
  return(text)
}

# Function to check if a string is valid UTF-8
validUTF8 <- function(text) {
  return(identical(text, iconv(text, from = "UTF-8", to = "UTF-8")))
}

# Function to preprocess the text
preprocess_text <- function(text) {
  cat("Original text:\n", paste(text, collapse = "\n"), "\n\n")  # Print original text
  
  # Step 1: Remove Title (First Line)
  text <- text[-1]  # Remove the first line (Title)
  cat("After removing title:\n", paste(text, collapse = "\n"), "\n\n")  # Print after title removal
  
  # Step 2: Remove Link and associated URL (but we'll store the link for later extraction)
  link_line <- str_extract(paste(text, collapse = " "), "Link:.*")
  text <- gsub("Link:.*", "", text)
  cat("After removing Link and URL:\n", paste(text, collapse = "\n"), "\n\n")  # Print after removing links
  
  # Combine lines into a single text
  text <- paste(text, collapse = " ")
  
  # Step 3: Lowercasing
  text <- tolower(text)
  cat("After lowercasing:\n", text, "\n\n")  # Print after lowercasing
  
  # Step 4: Remove punctuation except apostrophes
  text <- gsub("[[:punct:]]+", " ", text)  # Replace punctuation with a space
  text <- gsub(" '", "'", text)  # Fix spaces before apostrophes
  text <- gsub("' ", "'", text)  # Fix spaces after apostrophes
  cat("After removing punctuation:\n", text, "\n\n")  # Print after removing punctuation
  
  # Step 5: Remove numbers
  text <- gsub("\\d+", "", text)
  cat("After removing numbers:\n", text, "\n\n")  # Print after removing numbers
  
  # Step 6: Apply LSD preprocessing rules (Skipping 'punc' and 'caps')
  text <- apply_lsd_preprocessing(text, preprocess_rules)
  cat("After applying LSD preprocessing rules:\n", text, "\n\n")  # Print after applying LSD rules
  
  # Step 7: Lemmatization
  cat("Text before lemmatization:\n", text, "\n\n")  # Print before lemmatization
  text <- lemmatize_strings(text)
  cat("After lemmatization:\n", text, "\n\n")  # Print after lemmatization
  
  # Step 8: Remove remaining URLs (just in case)
  text <- gsub("http[s]?://\\S+|www\\.\\S+", "", text)
  cat("After removing remaining URLs:\n", text, "\n\n")  # Print after removing remaining URLs
  
  return(list(text = text, link_line = link_line))
}

# Function to extract the month and year from the link
extract_month_year_from_link <- function(link_line) {
  # Extract the URL from the link line
  link <- str_extract(link_line, "https?://[^ ]+")
  
  if (length(link) == 0 || is.na(link)) {
    return(list(year = NA, month = NA))  # Return NAs if the link is missing
  }
  
  # Extract year and month from the URL
  year <- str_extract(link, "/\\d{4}/")
  month <- str_extract(link, "(?<=/\\d{4}/)\\d{2}")
  
  # Clean up extracted values
  year <- gsub("/", "", year)
  month <- gsub("/", "", month)
  
  return(list(year = year, month = month))
}

# Function to count the number of times "China" is mentioned in the text
count_china_mentions <- function(text) {
  # Count occurrences of the word "China" (case insensitive)
  count <- str_count(text, regex("\\bChina\\b", ignore_case = TRUE))
  return(count)
}

# Function to calculate sentiment using the LSD dictionary
calculate_sentiment <- function(text) {
  # Convert text to a quanteda corpus
  corpus <- corpus(text)
  
  # Tokenize the text
  tokens <- tokens(corpus)
  
  # Create a document-feature matrix (dfm)
  dfm <- dfm(tokens)
  
  # Lookup sentiment using the LSD dictionary
  dfm_sentiment <- dfm_lookup(dfm, dictionary = data_dictionary_LSD2015)
  
  # Calculate sentiment score (positive - negative)
  sentiment_score <- sum(dfm_sentiment[, "positive"]) - sum(dfm_sentiment[, "negative"])
  
  # Return the overall sentiment score
  return(sentiment_score)
}

# Function to calculate sentiment within a window of +/- 8 words around "China"
calculate_window_sentiment <- function(text, window_size = 8) {
  # Tokenize the text
  tokens <- tokens(text)
  
  # Convert tokens to a character vector for subsetting
  token_chars <- unlist(as.list(tokens))
  
  # Locate positions of "China" in the tokens
  china_positions <- which(tolower(token_chars) == "china")
  
  if (length(china_positions) == 0) {
    return(NA)  # Return NA if "China" is not found
  }
  
  window_sentiments <- c()
  
  for (pos in china_positions) {
    # Define the window around "China"
    window_start <- max(1, pos - window_size)
    window_end <- min(length(token_chars), pos + window_size)
    token_window <- token_chars[window_start:window_end]
    
    # Create a dfm for the window
    dfm_window <- dfm(tokens(token_window))
    
    # Lookup sentiment using the LSD dictionary
    dfm_window_sentiment <- dfm_lookup(dfm_window, dictionary = data_dictionary_LSD2015)
    
    # Calculate sentiment score (positive - negative) for the window
    window_sentiment <- sum(dfm_window_sentiment[, "positive"]) - sum(dfm_window_sentiment[, "negative"])
    
    # Store the window sentiment
    window_sentiments <- c(window_sentiments, window_sentiment)
  }
  
  # Return the average sentiment across all windows
  return(mean(window_sentiments, na.rm = TRUE))
}

# Function to count the number of tokens (words) in the text
count_words <- function(text) {
  # Tokenize the text
  tokens <- tokens(text)
  
  # Count the number of tokens (words)
  word_count <- ntoken(tokens)
  
  return(word_count)
}

# Initialize a dataframe to store the results
results_df <- data.frame(FileName = character(), Year = integer(), Month = integer(), 
                         ChinaMentions = integer(), Sentiment = numeric(), WindowSentiment = numeric(), WordCount = integer(), stringsAsFactors = FALSE)

# Loop through each file, preprocess the text, count "China" mentions, calculate sentiment, and store in the dataframe
for (file_name in file_list) {
  # Read the content of the file
  full_content <- readLines(file_name, warn = FALSE)
  
  # Preprocess the text
  preprocessed_result <- preprocess_text(full_content)
  preprocessed_text <- preprocessed_result$text
  link_line <- preprocessed_result$link_line
  
  # Extract the month and year from the link
  date_info <- extract_month_year_from_link(link_line)
  
  # Count the mentions of "China"
  china_count <- count_china_mentions(preprocessed_text)
  
  # Calculate overall sentiment
  sentiment_score <- calculate_sentiment(preprocessed_text)
  
  # Calculate window sentiment around "China"
  window_sentiment <- calculate_window_sentiment(preprocessed_text)
  
  # Count the number of words
  word_count <- count_words(preprocessed_text)
  
  # Add the file name, date info, counts, and sentiment to the results dataframe
  results_df <- rbind(results_df, data.frame(FileName = file_name, Year = date_info$year, Month = date_info$month,
                                             ChinaMentions = china_count, Sentiment = sentiment_score, WindowSentiment = window_sentiment, WordCount = word_count, stringsAsFactors = FALSE))
}

# Write the results to a CSV file
write.csv(results_df, "china_mentions_sentiment_all.csv", row.names = FALSE)

# Print the results dataframe to inspect
print(results_df)


#################################
### Scrape numbr of articles  ###
#################################

# Clear workspace
rm(list=ls())

# Define the range of pages to scrape *figured out by looking at website, this will change day-to-day
start_page <- 15
end_page <- 967

# Initialize vectors to store all links
all_links <- c()

# Loop through each page
for (page_num in start_page:end_page) {
  # Construct the URL for each page
  url <- paste0("https://www.manilatimes.net/opinion/page/", page_num)
  
  # Read the HTML content of the page
  page <- read_html(url)
  
  # Extract all links from the page
  page_links <- page %>% html_nodes("a") %>% html_attr("href")
  
  # Append the links to the all_links vector
  all_links <- c(all_links, page_links)
  
  # Print the URL being processed
  cat("Processed page:", url, "\n")
}

# Define the links to drop
links_to_drop <- c(
  "https://www.manilatimes.net/opinion/columns",
  "https://www.manilatimes.net/opinion/contributors",
  "https://www.manilatimes.net/opinion/editorial",
  "https://www.manilatimes.net/opinion/editorial-cartoon",
  "https://www.manilatimes.net/opinion/letters-to-the-editor",
  "https://www.manilatimes.net/opinion/world"
)

# Filter links to keep only those containing '/opinion/' and exclude '/editorial-cartoon/'
opinion_links <- all_links[str_detect(all_links, "/opinion/") & !str_detect(all_links, "/editorial-cartoon/")]

# Drop specified links
filtered_links <- setdiff(opinion_links, links_to_drop)

# Further drop links containing '/opinion/page/'
links_no_pagination <- filtered_links[!str_detect(filtered_links, "/opinion/page/")]

# Drop links containing '/2024/08'
final_filtered_links <- links_no_pagination[!str_detect(links_no_pagination, "/2024/08")]

# Remove duplicates
unique_filtered_links <- unique(final_filtered_links)

# Function to extract year and month from a URL
extract_year_month <- function(url) {
  year <- str_extract(url, "(?<=/)(\\d{4})(?=/)")
  month <- str_extract(url, "(?<=/\\d{4}/)(\\d{2})(?=/)")
  return(paste(year, month, sep = "-"))
}

# Apply the extraction function to each URL
date_info <- sapply(unique_filtered_links, extract_year_month)

# Create a data frame with year-month counts
date_df <- data.frame(
  YearMonth = date_info,
  stringsAsFactors = FALSE
)

# Count occurrences of each YearMonth combination
count_by_month_year <- date_df %>%
  group_by(YearMonth) %>%
  summarise(Count = n(), .groups = 'drop')

# Split YearMonth into separate year and month columns
count_by_month_year <- count_by_month_year %>%
  separate(YearMonth, into = c("Year", "Month"), sep = "-")

# Add the specified row
count_by_month_year <- rbind(count_by_month_year, data.frame(Year = "2024", Month = "08", Count = 218))

# Filter out years before 2010
count_by_month_year <- count_by_month_year %>%
  filter(Year >= 2010)

# Save to CSV file
write.csv(count_by_month_year, file = "monthly_article_counts.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Print the result
print(count_by_month_year)

### Expected to grab 14,265 articles, but only grabbed 14,255. This is 99.9% accurate. Fine for now.
### Note: 14,265 is before dropping Aug 2024 (to count manually because its not all days that month) 
### and dropping less than year 2010.

# ==============================================================================
# NOTE: Final Raw Data Outputs
# ------------------------------------------------------------------------------
# There are two main cvs files needed to perform the analysis
# - "china_mentions_sentiment_all.csv"
#    -> LSD sentiment anlaysis, China mentions p/ article, token (word) count
# - "monthly_article_counts.csv"
#    -> count total number of articles by month-year from Aug 23, 2024-Jan 1 2010
# ==============================================================================

