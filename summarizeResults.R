# Load necessary libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(pbmcapply)
library(googlesheets4)
library(tictoc)

# Load summaries
load("summaries.Rdata")

# This function analyzes the summary for the research questions using the OpenAI API
getResults <- function(summary, questions) {
  
  prompt <- paste(
    "Based on the provided summary of a research article, answer each of the following questions with a single item response. The response should be a concise, direct answer, and if the information is not included or addressed in the summary, respond with 'NA' only, with no additional explanation or text. Make sure that no answers to individual questions contain semicolons. Then teturn the five responses all together with a semicolon delimiter.\n\n",
    "**Questions:**\n\n",
    paste(questions, collapse = "\n\n"),
    "\n\n**Summary:**\n\n",
    summary,
    sep = ""
  )
  
  # Now that we have ensured the token limit is respected, make the API call
  answer <- call_openai_api(prompt, "gpt-4")
  
  # Return the answer
  return(answer)
}

call_openai_api <- function(prompt, model) {
  
  # Sys.sleep(5)
  
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
      temperature = 0.25,
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

questions <- c(
  "Does the summary specify which specific types of gun violence incidents (such as mass shootings, domestic violence, gang-related shootings, police-perpetrated killings, etc.) the news media most frequently reports on? This would need to involve some comparison of rates of different types of gun violence. If so, please summarize those findings. Respond with 'NA' only if not specified, with no other explanation or other text.",
  "Does the summary identify specific factors (such as the scale of the incident, the socioeconomic status of those involved, racial demographics, location, etc.) that influence the media's coverage of certain gun violence incidents over others? If so, please summarize those specific factors. Respond with 'NA' only if not specified, with no other explanation or other text.",
  "Does the summary explain the linguistic styles (like emotive language, descriptive language, technical jargon, etc.) and narrative frames (such as tragedy frame, crime frame, public health frame, etc.) commonly used by the news media in reporting gun violence? If so, please provide a summary of these styles and frames. Respond with 'NA' only if not specified, with no other explanation or other text.",
  "Does the summary discuss the specific factors (such as political agenda, audience targeting, journalistic standards, etc.) that influence how the news media selects language and narrative framing when covering gun violence? If so, please summarize these specific factors. Respond with 'NA' only if not specified, with no other explanation or other text.",
  "Does the summary describe the specific impacts on society (like changes in public perception, policy influence, fear levels, desensitization, etc.) that result from the media’s coverage of gun violence, including the narrative frames used? If so, please summarize these impacts. Respond with 'NA' only if not specified, with no other explanation or other text."
)

results <- rep(NA, length(summaries))
for (i in 1:length(results)) {
  cat(paste0("Working on i = ", i,"\n"))
  tic()
  results[i] <- getResults(summaries[i], questions)
  timer <- toc(quiet = TRUE)
  Sys.sleep(max(0, as.numeric(10 - (timer$toc - timer$tic))))
}  

# Parse results
results_df <- data.frame(result = results) %>%
  separate(result, into = c("Q1", "Q2", "Q3", "Q4", "Q5"), sep = ";", fill = "right") %>%
  mutate(across(everything(), ~str_squish(.)))

# Put in order and write
gs4_auth()
sheet_url <- "https://docs.google.com/spreadsheets/d/1xNunZbWyGE7dE8wiKYjB_SvbIngrXL1sCTtjepnpn8s/edit?usp=sharing"
uniqueID <- range_read(ss = sheet_url, 
                       sheet = "Retrieved", 
                       range = "B1:B",
                       col_names = TRUE)
fileorder <- "Retrieved Papers/pdf/" %>%
  list.files %>%
  str_replace_all(".pdf","")
results_df <- cbind(data.frame(`Unique ID` = fileorder, check.names = FALSE), results_df)
results_df <- inner_join(uniqueID, results_df) %>% select(-`Unique ID`)

# Write to Google Sheets
range_write(data = results_df, 
            ss = sheet_url, 
            sheet = "Retrieved", 
            range = "F2",
            col_names = FALSE)