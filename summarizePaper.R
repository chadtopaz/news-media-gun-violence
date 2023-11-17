# Load necessary libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(pbmcapply)
library(googlesheets4)

# This function splits the text into appropriately sized chunks
split_into_chunks <- function(full_text, target_tokens_per_chunk = 5000, overlap_tokens = 200) {
  # Calculate the approximate target length for each chunk in characters
  average_token_length <- 4
  target_length <- target_tokens_per_chunk * average_token_length
  overlap_length <- overlap_tokens * average_token_length
  
  # Use strsplit with a regex to split the text into sentences
  sentences <- unlist(strsplit(full_text, "(?<=[.!?])\\s+", perl=TRUE))
  
  chunks <- list()
  current_chunk <- ""
  current_chunk_length <- 0
  overlap_sentences <- ""
  
  for (sentence in sentences) {
    sentence_length <- nchar(sentence)
    
    if (current_chunk_length + sentence_length > target_length) {
      # Add the current chunk with overlap to the list
      chunks <- c(chunks, paste(overlap_sentences, current_chunk, sep=" "))
      # Prepare overlap for the next chunk
      overlap_sentences <- substring(current_chunk, nchar(current_chunk) - overlap_length + 1)
      current_chunk <- sentence
      current_chunk_length <- sentence_length
    } else {
      current_chunk <- paste(current_chunk, sentence, sep="")
      current_chunk_length <- current_chunk_length + sentence_length
    }
  }
  
  # Add the last chunk if it's not empty
  if (nchar(current_chunk) > 0) {
    chunks <- c(chunks, paste(overlap_sentences, current_chunk, sep=" "))
  }
  
  return(chunks)
}


# This function calls the OpenAI API with the provided prompt
# It includes error handling, rate limiting, etc.
call_openai_api <- function(prompt, model, max_tokens) {
  
  Sys.sleep(10)
  
  # Set API key
  api_key <- "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"
  
  # Handle rate limiting (naive implementation)
  
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = model,
      temperature = 0.7,
      max_tokens = max_tokens,
      messages = list(list(role = "user", content = prompt))
    )
  )
  
  # Check for errors in the response
  if (status_code(response) >= 400) {
    stop("Failed to call OpenAI API. Status code: ", status_code(response))
  }
  
  # Parse the response
  response_content <- content(response, as = "parsed", type = "application/json")
  
  # Return the text completion
  return(response_content$choices[[1]]$message$content)
}


summarize_single_chunk <- function(chunk, model, max_tokens = 700) {
  
  prompt <- paste0(
    "Summarize the following text by providing a comprehensive and detailed technical analysis. ",
    "Focus on the following aspects with high specificity:\n",
    "- Data sources used and their relevance\n",
    "- Detailed methodology, including study design and data analysis techniques\n",
    "- Main findings with specific results\n",
    "- Conclusions and their implications\n\n",
    "It is crucial to maintain a high level of specificity and detail to accurately reflect the technical complexity of the study. Do not omit any technical details or data points, as they are necessary for a full understanding of the research.\n\n",
    chunk
  )
  
  # Call API
  summary <- call_openai_api(prompt, model, max_tokens)
  
  return(summary)
}

# This function summarizes each chunk using the OpenAI API

summarize_chunks <- function(chunks, model) {
  summaries <- pbmclapply(chunks, summarize_single_chunk, model = model, mc.cores = 1)
  return(unlist(summaries)) # Unlist the list of summaries into a vector
}

# This function combines individual summaries into one master summary
combine_summaries <- function(summaries, model) {
  combined_summary <- paste(summaries, collapse=" ") # Combine all summaries into one text
  
  prompt <- paste0(
    "Synthesize these summaries of portions of a research article into a comprehensive and detailed summary. ",
    "Focus on the following aspects with high specificity:\n",
    "- Data sources used and their relevance\n",
    "- Detailed methodology, including study design and data analysis techniques\n",
    "- Main findings with specific results\n",
    "- Conclusions and their implications\n\n",
    "Maintain a high level of specificity and detail to accurately reflect the technical complexity of the study. ",
    "Eliminate redundancy and ensure the summary is organized logically.\n\n",
    combined_summary
  )
  
  # Call the OpenAI API using the previously defined function
  # Ensure that we adjust the max tokens parameter based on the prompt length
  summary <- call_openai_api(prompt, model, NA)
  
  return(summary) # Return the text completion
}


# Implementation
process_document <- function(file) {
  
  # Get text
  full_text <- readLines(file, warn = FALSE) %>%
    paste(collapse = "\n") %>%
    iconv(from = "", to = "UTF-8", sub = "")
  
  # Split the document into chunks considering token limits
  chunks <- split_into_chunks(full_text)
  
  # Summarize each chunk
  summaries <- summarize_chunks(chunks, "gpt-4")
  
  # Combine summaries into a master summary, considering the token limits
  master_summary <- combine_summaries(summaries, "gpt-4")
  
  return(master_summary)
}

text_file_path <- "Retrieved Papers/pdf/"
files <- paste0(text_file_path, list.files(text_file_path))

# Run
summaries <- c()
for (i in 1:length(files)) {
  print(paste0("Working on i = ", i))
  thisSummary <- process_document(files[i])
  summaries <- c(summaries, thisSummary)
}

# Write to Google Sheets
gs4_auth()
sheet_url <- "https://docs.google.com/spreadsheets/d/1xNunZbWyGE7dE8wiKYjB_SvbIngrXL1sCTtjepnpn8s/edit?usp=sharing"
uniqueID <- range_read(ss = sheet_url, 
                       sheet = "Retrieved", 
                       range = "B1:B",
                       col_names = TRUE)
fileorder <- list.files(text_file_path) %>%
  str_replace_all(".pdf","")
tmp <- data.frame(`Unique ID` = fileorder, E = str_squish(summaries), check.names = FALSE)
summariesdf <- inner_join(uniqueID, tmp) %>% select(-`Unique ID`)
range_write(data = summariesdf, 
            ss = sheet_url, 
            sheet = "Retrieved", 
            range = "E2",
            col_names = FALSE)
