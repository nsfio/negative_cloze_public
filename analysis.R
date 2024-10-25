## negation cloze data - spring 24 - university of delaware ##
# this script takes the long-format .xlsx outputted by cleaning.R and uses it to compute CPs
# second part computes correlations between CPs and GPT 3.5 metrics

# loading libs

library(tidyverse)
library(openxlsx)
library(broom)
library(ggpubr)

# Loading data 

cloze_data <- read.xlsx("cleaned_cloze_data.xlsx")

# Creating a simplified data frame to count word occurrences

simplified_df <- cloze_data %>% 
  select(item, standardized_case_response) %>% 
  filter(is.na(standardized_case_response) == FALSE)

# Counting word occurrences

results <- simplified_df %>% 
  count(item, standardized_case_response)

results_w_probs <- results %>% 
  group_by(item) %>% 
  mutate(prob = n / sum(n))

# Checking that items and conditions are represented

unique(results_w_probs$item)

# Writing results file

write.xlsx(results_w_probs, "results.xlsx")

# Recalculating cps after manually checking for spelling errors and nonsense completions

  # make sure that wd is set to this script's directory

results <- read.xlsx("results.xlsx")

results <- results %>% 
  group_by(item) %>% 
  mutate(cp = n / sum(n))

# Rewriting the file (with a different name as a precaution)

write.xlsx(results, "results_ready.xlsx")

# Calculating correlations (updated on Jul 1st, 2024)
  # make sure wd is set

human_data <- read.xlsx("results_ready.xlsx")
gpt_data <- read.xlsx("revised_gpt_completions.xlsx")

# Dealing with duplicate values

gpt_data$completion <- trimws(gpt_data$completion) # getting rid of spaces

gpt_data <- gpt_data %>% # removing duplicates
  group_by(item, stim_text, completion) %>%
  summarize(gpt_prob = sum(gpt_prob, na.rm = TRUE)) %>%
  ungroup()

# Writing gpt_data with duplicates removed
  # (manual) alternative analysis at this point: combine partial word tokens (e.g., diff-difference, danger-dangerous)
write.xlsx(gpt_data, "gpt_completions_no_duplicates.xlsx")

# Combining human and LLM data sets (see if you can make this compatible with the cor code later)

human_data <- human_data %>% 
  rename(completion = standardized_case_response)

cor_data <- full_join(human_data, gpt_data, by = c("item", "completion"))

# Dropping unnecessary columns

cor_data <- cor_data %>% 
  mutate(stim_text = NULL,
         n = NULL)

# Writing file

write.xlsx(cor_data, "cp_gpt_results.xlsx")

# ------------------------------ Correlation analysis --------------------------------------------------------------


# cloze-gpt correlation (rerun code above to continue using cor_data or use read line below accordingly)

cor_data <- read.xlsx("cp_gpt_results.xlsx")

  # scatter plot visualization

  current_plot <- cor_data %>% 
    ggplot(aes(y = gpt_prob, x = cp)) +
      geom_point(alpha = 0.5, shape = 16) +
      geom_smooth(method = "lm", color = "black") +
      stat_cor(p.accuracy = .001, r.accuracy = .01) +
      labs(title = "LLM vs. cloze probability") +
      ylab("GPT 3.5 probability") +
      xlab("Cloze probability") +
      theme_bw()
  
  current_plot
  
  ggsave(file="general_corr.svg", plot=current_plot, width=10, height=8)
  
  # general correlation (not a great test, but that's what we have!)
  
  gen_cor <- cor.test(cor_data$cp, cor_data$gpt_prob) # (r = .39; CI = .25-.52; p < .0001) - no change
  gen_cor 

  # breaking correlations by condition
  
    # start by creating a condition column based on the item suffix (this throws an error but works?)
  cor_data <- cor_data %>% 
    mutate(condition = case_when(
      str_detect(item, regex("[a]$")) ~ "context - affirmative",
      str_detect(item,  regex("[b]$")) ~ "no context - affirmative",
      str_detect(item, regex("[c]$")) ~ "context - negative",
      str_detect(item, regex("[d]$")) ~ "no context - negative"
    ))
  
  cor_data <- cor_data %>% 
    mutate(context = case_when(
      str_detect(item, regex("[a]$")) ~ "context",
      str_detect(item,  regex("[b]$")) ~ "no context",
      str_detect(item, regex("[c]$")) ~ "context",
      str_detect(item, regex("[d]$")) ~ "no context"
    ))
  
  cor_data <- cor_data %>% 
    mutate(polarity = case_when(
      str_detect(item, regex("[a]$")) ~ "affirmative",
      str_detect(item,  regex("[b]$")) ~ "affirmative",
      str_detect(item, regex("[c]$")) ~ "negative",
      str_detect(item, regex("[d]$")) ~ "negative"
    ))
  
  cor_data$context <- as.factor(cor_data$context)
  cor_data$polarity <- as.factor(cor_data$polarity)
  
  
    # visualization by condition
  
  current_plot <- cor_data %>% 
    ggplot(aes(y = gpt_prob, x = cp)) +
    geom_point(alpha = 0.5, shape = 16) +
    geom_smooth(method = "lm", color = "black") +
    stat_cor(p.accuracy = .001, r.accuracy = .01) +
    labs(title = "LLM vs. cloze probability", subtitle = "Split by context presence and sentence polarity") +
    ylab("GPT 3.5 probability") +
    xlab("Cloze probability") +
    theme_bw() +
    facet_wrap(vars(condition))
  
  current_plot
  
  ggsave(file="corr_by_conditions.svg", plot=current_plot, width=10, height=8)
  
    # correlations by condition
  
  cor_table <- cor_data %>% 
    group_by(condition) %>% 
    summarise(correlation = tidy(cor.test(cp, gpt_prob)))
  
  cor_table
  
  # adjusting p-vals for multiple comparisons
  
  pvals <- c(gen_cor$p.value ,cor_table$correlation$p.value)
  adjusted_pvals <- p.adjust(pvals, method = "fdr")
  format(adjusted_pvals, scientific = FALSE) # order: gen cor, cont-aff, cont-neg, no cont-aff, no cont-neg
  
  adjusted_pvals # same pattern: only conditions A and D show significant correlations
  
# EXTRA ANALYSES ------------------
  
  # correlation analysis controlling for partial tokens (partial tokens grouped with next highest prob valid    whole word)
  
  gpt_data_pt <- read.xlsx("gpt_completions_no_duplicates_partial_tokens.xlsx")
  
  cor_data_pt <- full_join(human_data, gpt_data_pt, by = c("item", "completion"))
  
  cor_data_pt <- cor_data_pt %>% 
    mutate(stim_text = NULL,
           n = NULL,
           gpt_prob = NULL)
  
  # scatter plot visualization
  
  current_plot <- cor_data_pt %>% 
    ggplot(aes(y = gpt_prob_fixed, x = cp)) +
    geom_point(alpha = 0.5, shape = 16) +
    geom_smooth(method = "lm", color = "black") +
    stat_cor(p.accuracy = .001, r.accuracy = .01) +
    labs(title = "LLM vs. cloze probability") +
    ylab("GPT 3.5 probability") +
    xlab("Cloze probability") +
    theme_bw()
  
  current_plot
  
  ggsave(file="general_corr_pt.svg", plot=current_plot, width=10, height=8)
  
  # general correlation (not a great test, but that's what we have!)
  
  gen_cor <- cor.test(cor_data_pt$cp, cor_data_pt$gpt_prob_fixed) # (r = .23; CI = .07-.37; p = .003) - reduction, no qualitative change
  gen_cor 
  
  # breaking correlations by condition
  
  # start by creating a condition column based on the item suffix (this throws an error but works?)
  cor_data_pt <- cor_data_pt %>% 
    mutate(condition = case_when(
      str_detect(item, regex("[a]$")) ~ "context - affirmative",
      str_detect(item,  regex("[b]$")) ~ "no context - affirmative",
      str_detect(item, regex("[c]$")) ~ "context - negative",
      str_detect(item, regex("[d]$")) ~ "no context - negative"
    ))
  
  cor_data_pt <- cor_data_pt %>% 
    mutate(context = case_when(
      str_detect(item, regex("[a]$")) ~ "context",
      str_detect(item,  regex("[b]$")) ~ "no context",
      str_detect(item, regex("[c]$")) ~ "context",
      str_detect(item, regex("[d]$")) ~ "no context"
    ))
  
  cor_data_pt <- cor_data_pt %>% 
    mutate(polarity = case_when(
      str_detect(item, regex("[a]$")) ~ "affirmative",
      str_detect(item,  regex("[b]$")) ~ "affirmative",
      str_detect(item, regex("[c]$")) ~ "negative",
      str_detect(item, regex("[d]$")) ~ "negative"
    ))
  
  cor_data_pt$context <- as.factor(cor_data_pt$context)
  cor_data_pt$polarity <- as.factor(cor_data_pt$polarity)
  
  
  # visualization by condition
  
  current_plot <- cor_data_pt %>% 
    ggplot(aes(y = gpt_prob_fixed, x = cp)) +
    geom_point(alpha = 0.5, shape = 16) +
    geom_smooth(method = "lm", color = "black") +
    stat_cor(p.accuracy = .001, r.accuracy = .01) +
    labs(title = "LLM vs. cloze probability", subtitle = "Split by context presence and sentence polarity") +
    ylab("GPT 3.5 probability") +
    xlab("Cloze probability") +
    theme_bw() +
    facet_wrap(vars(condition))
  
  current_plot
  
  ggsave(file="corr_by_conditions_pt.svg", plot=current_plot, width=10, height=8)
  
  # correlations by condition
  
  cor_data_pt <- cor_data_pt %>% 
    group_by(condition) %>% 
    summarise(correlation = tidy(cor.test(cp, gpt_prob_fixed)))
  
  cor_table
  
  # adjusting p-vals for multiple comparisons
  
  pvals <- c(gen_cor$p.value ,cor_table$correlation$p.value)
  adjusted_pvals <- p.adjust(pvals, method = "fdr")
  format(adjusted_pvals, scientific = FALSE) # order: gen cor, cont-aff, cont-neg, no cont-aff, no cont-neg
  
  adjusted_pvals # no change, same pattern: only conditions A and D show significant correlations
  
  # what was the average top cp?
  
 top_cps <-  cor_data %>% 
    group_by(item) %>% 
    summarise(max_cp = max(cp, na.rm = TRUE))
  
  summary(top_cps)
  hist(top_cps$max_cp)
  
  # what was the average top gpt_prob?
  
  top_gpt <- gpt_data %>% 
    group_by(item) %>% 
    summarise(max_prob = max(gpt_prob))

  summary(top_gpt)  
  hist(top_gpt$max_prob)
  