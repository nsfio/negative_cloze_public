## negation cloze data - spring 24 - university of delaware ##
## getting gpt probabilities ##

# loading libs

library(tidyverse)
library(openxlsx)
library(reticulate)
library(httr)

# API key (do not distribute any version of this script with this variable filled out)
  # if you are a reviewer and do not have access to an OpenAI API key, please contact the authors through the       appropriate channels

key <- ""

# reading stimuli file in 
  # make sure wd is set

stim <- read.xlsx("stimuli.xlsx")

# function to get completions

get_completions_by_item <- function(stimuli, api_key){ # function input should be the stim tibble and API key
  
  # initialize output tibble
  model_completions <- tibble(
    item = character(),
    stim_text = character(),
    completion = character(),
    gpt_prob = numeric()
  )
  
  # going through each row in the stimuli tibble
  for (i in 1:nrow(stimuli)) {
    item <- stimuli$item[i]
    stim_text <- stimuli$stim_text[i]
    
    # Create the request body for each item
    request <- list(
      model = "gpt-3.5-turbo-instruct",
      prompt = paste0(stim_text, " "),
      max_tokens = 1,
      logprobs = 1, 
      n = 10,
      stop = "."
    )
    
    response <- POST(
      url = "https://api.openai.com/v1/completions",
      add_headers(Authorization = paste("Bearer", api_key)),
      body = request,
      encode = "json"
    )
    
    content <- content(response, as = "parsed")
    
    logprobs <- content$choices[[1]]$logprobs$top_logprobs[[1]]
    completions <- names(logprobs)
    
    # Populate the model_completions tibble
    for (completion in completions) {
      model_completions <- model_completions %>% add_row(
        item = item,
        stim_text = stim_text,
        completion = completion,
        gpt_prob = exp(logprobs[[completion]])
      )
    }
  }
  
  return(model_completions)
  
}

gpt_completions <- get_completions_by_item(stim, key)

write.xlsx(gpt_completions, "gpt_completions.xlsx")