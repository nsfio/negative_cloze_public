## negation cloze data - spring 24 - university of delaware ##
# this script takes the raw qualtrics csv file and returns a structured file that can be plugged into the analysis script
# the first 2 parts of this code (setting wd and loading data in) were written when I hadn't been converted to the here() package yet, so kindly set working directories and specific file names as necessary for your machine

# loading libs

library(tidyverse)
library(openxlsx)

# loading data (specify as necessary locally)

setwd()

  # treating 3-row headers

  c_names <- read_csv("qualtrics_data.csv", n_max = 0) %>% names() # getting column names
  glimpse(c_names) # checking names vector
  data <- read_csv("qualtrics_data.csv", skip = 3, col_names = c_names) # reading csv file again, this time ignoring the first 3 rows and using the c_names vector for the column names

# cleaning data
  
  # selecting relevant columns
  
  info <- data %>% 
    select(StartDate:Q6)

  responses <- data %>% 
    select(matches("^(0[1-9]|1[0-9]|20)[a-d]$"))
  
  relevant_data <- bind_cols(info, responses)

  # turning df into long format
  
  long_data <- relevant_data %>% 
    pivot_longer(matches("^(0[1-9]|1[0-9]|20)[a-d]$"), names_to = "item", values_to = "raw_response")
  
  # discarding survey preview responses, incomplete participations, self-reported non-native speakers
  
  long_data <- long_data %>% 
    filter(Status != "Survey Preview" & Finished == TRUE & Q22 == "Yes")
  
  # checking answers to the open-ended neurological/language impairments
  
  unique(long_data$Q32) # answers are all fine
  
  # checking duration distribution
  
  summary(long_data$`Duration (in seconds)`) # choice: not excluding participations based on duration
  
  # getting age info
  
  summary(long_data$Q21) # mean = 19.12 yo
  
  # checking true N
  
  length(unique(long_data$ResponseId)) # 48
  
  # creating a column with standardized responses (all lower case, no special characters beyond hyphens)
  
  long_data <- long_data %>% 
    mutate(standardized_case_response = str_to_lower(raw_response)) %>% 
    mutate(standardized_case_response = str_replace_all(standardized_case_response, "[^a-z -]", ""))
  
  # anonymization
  
  long_data <- long_data %>% 
    mutate(IPAddress = NULL, 
           RecipientLastName = NULL, 
           RecipientFirstName = NULL,
           RecipientEmail = NULL,
           ExternalReference = NULL,
           LocationLatitude = NULL,
           LocationLongitude = NULL,
           Q2 = NULL,
           Q4 = NULL,
           Q6 = NULL,
           Q21 = NULL,
           Q22 = NULL,
           Q32 = NULL,
           Q197 = NULL)

  # exporting data file
  
write.xlsx(long_data, "cleaned_cloze_data.xlsx")
  
  