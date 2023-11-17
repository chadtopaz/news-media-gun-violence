# Load libraries
library(tidyverse)
library(googlesheets4)
library(httr)
library(jsonlite)
library(pbmcapply)
library(parallel)

# Auth for data
gs4_auth()

# Read in data
sheet_url <- "https://docs.google.com/spreadsheets/d/1xNunZbWyGE7dE8wiKYjB_SvbIngrXL1sCTtjepnpn8s/edit?usp=sharing"
woslit <- read_sheet(sheet_url, sheet = "Web of Science")
gslit <- read_sheet(sheet_url, sheet = "Google Scholar Deduplicated")

# Set openAI API key
apiKey <- "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"

queryGPTwos <- function(i, woslit, apiKey) {
  
  instructions <- paste(
    "Review the title and abstract for a study on U.S. news media's coverage of gun violence. ",
    "Decide on 'include' or 'exclude' based on these criteria:",
    "Inclusion:",
    "- Primary focus on U.S. news media coverage of gun violence incidents in the U.S.",
    "- Analysis of which incidents are covered, how they are covered, and impact of that coverage on individuals or society.",
    "Exclusion:",
    "- Non-empirical studies, focus not on news media coverage itself, or studies outside the U.S.",
    "- Non-news media types or unreviewed sources (e.g., commentary, social media, films).",
    sep = "\n"
  )
  title_abstract <- paste0("\n\nTitle and Abstract:\n", woslit$`Article Title`[i], "\n", woslit$Abstract[i])
  prompt <- paste(instructions, title_abstract)
  
  max_retries <- 5
  attempt <- 1
  
  repeat {
    response <- tryCatch({
      POST(
        url = "https://api.openai.com/v1/chat/completions",
        add_headers(Authorization = paste("Bearer", apiKey)),
        content_type_json(),
        encode = "json",
        body = list(
          model = "gpt-4",
          temperature = 1,
          messages = list(list(role = "user", content = prompt))
        )
      )
    }, error = function(e) return(NULL))
    
    if (!is.null(response) && status_code(response) == 200) {
      return(content(response))
    } else if (!is.null(response) && grepl("Rate limit reached", content(response, "text"))) {
      Sys.sleep(attempt*2) # Backoff
      attempt <- attempt + 1
      if (attempt > max_retries) return(NULL)
    } else {
      Sys.sleep(runif(2))
    }
  }
}


# It's better to reduce the number of cores to handle the rate limit more effectively
# and to avoid overloading the server
wosresults <- pbmclapply(1:nrow(woslit), queryGPTwos, woslit, apiKey, mc.cores = 1)

# It took 37 min 43 sec

# Now convert the list to a character vector, replacing NULLs with NAs
wosresultsvector <- sapply(wosresults, function(x) {
  if (is.null(x)) {
    NA
  } else {
    x$choices[[1]]$message$content %>% 
      str_to_title # Assuming this is the desired text transformation
  }
})
wosresultsvector <- wosresultsvector %>% 
  map_if(~ !(.x %in% c("Include", "Exclude")), ~NA_character_) %>%
  unlist

wosGPTresultsdf <- data.frame(BX = wosresultsvector)
range_write(data = wosGPTresultsdf, 
            ss = sheet_url, 
            sheet = "Web of Science", 
            range = "BX2",
            col_names = FALSE)

queryGPTgs <- function(i, gslit, apiKey) {
  
  instructions <- paste(
    "Review the title and abstract for a study on U.S. news media's coverage of gun violence. ",
    "Decide on 'include' or 'exclude' based on these criteria:",
    "Inclusion:",
    "- Primary focus on U.S. news media coverage of gun violence incidents in the U.S.",
    "- Analysis of which incidents are covered, how they are covered, and impact of that coverage on individuals or society.",
    "Exclusion:",
    "- Non-empirical studies, focus not on news media coverage itself, or studies outside the U.S.",
    "- Non-news media types or unreviewed sources (e.g., commentary, social media, films).",
    sep = "\n"
  )
  title_abstract <- paste0("\n\nTitle and Abstract:\n", gslit$Title[i], "\n", gslit$Abstract...26[i])
  prompt <- paste(instructions, title_abstract)
  
  max_retries <- 5
  attempt <- 1
  
  repeat {
    response <- tryCatch({
      POST(
        url = "https://api.openai.com/v1/chat/completions",
        add_headers(Authorization = paste("Bearer", apiKey)),
        content_type_json(),
        encode = "json",
        body = list(
          model = "gpt-4",
          temperature = 1,
          messages = list(list(role = "user", content = prompt))
        )
      )
    }, error = function(e) return(NULL))
    
    if (!is.null(response) && status_code(response) == 200) {
      return(content(response))
    } else if (!is.null(response) && grepl("Rate limit reached", content(response, "text"))) {
      Sys.sleep(attempt*2) # Backoff
      attempt <- attempt + 1
      if (attempt > max_retries) return(NULL)
    } else {
      Sys.sleep(runif(2))
    }
  }
}


# It's better to reduce the number of cores to handle the rate limit more effectively
# and to avoid overloading the server
gsresults <- pbmclapply(1:nrow(gslit), queryGPTgs, gslit, apiKey, mc.cores = 1)

# It took 4 min 20 sec

# Now convert the list to a character vector, replacing NULLs with NAs
gsresultsvector <- sapply(gsresults, function(x) {
  if (is.null(x)) {
    NA
  } else {
    x$choices[[1]]$message$content %>% 
      str_to_title # Assuming this is the desired text transformation
  }
})

gsGPTresultsdf <- data.frame(AF = gsresultsvector)
range_write(data = gsGPTresultsdf, 
            ss = sheet_url, 
            sheet = "Google Scholar Deduplicated", 
            range = "AF2",
            col_names = FALSE)