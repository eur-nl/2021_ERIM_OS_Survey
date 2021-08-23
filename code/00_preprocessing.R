
# RNG ---------------------------------------------------------------------

seed_synth <- 135249315 
set.seed(seed_synth)

# Install packages --------------------------------------------------------

# install.packages("here")
# install.packages("tidyverse")

# Load packages --------------------------------------------------------

library(here)
library(tidyverse)

# load custom function to split variable into multiple columns
source(here("code", "split_into_multiple.R"))

# Load data ----------------------------------------------------------------

# pseudonymized data from the 2021 ERIM Open Science Survey (retrieved on June 8th 2021) after manual cleaning 
ERIM_OS <-
  read_csv(
    here("data", "PSEUDONYM_manual_20210608_ERIM_OS_Survey.csv"),
    col_names = TRUE,
    show_col_types = FALSE
  )

# separate questions into clusters:
# 0 = demographics [columns 2, 3, 4, 5, 6, 35]
# 1 = OS, general [columns 7, 37]
# 2 = preregistration [columns 8, 9, 10]
# 3 = open materials/code [columns 11, 12, 13, 14]
# 4 = open data [columns 15, 16, 17, 18]
# 5 = pre-publication archiving [columns 19, 20, 21]
# 6 = open access [columns 22, 23]
# 7 = OS adoption/barriers [column 36]
# 8 = tool awareness [columns 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34]
# 9 = role of ERIM [columns 38, 39]

# assign a cluster number to each question
levels_question <- c(
  "0" = "Which faculty are you from?",                                                                             
  "0" = "Which department are you affiliated to? [RSM]",                                                                                                                                                              
  "0" = "Which department are you affiliated to? [ESE]",                                                                                                                                                              
  "0" = "What is your position?",                                                                                                                                                                                     
  "0" = "Are you member of any research institute affiliated with RSM or ESE?",                                                                                                                                       
  "1" = "What is your experience with open science practices?",                                                                                                                                                       
  "2" = "In your opinion, how important for your field is it that researchers preregister their studies?",                                                                                                            
  "2" = "What is your experience with study preregistration?",                                                                                                                                                        
  "2" = "The following are possible concerns that researchers could have about preregistering their studies. Which of these concerns would apply to you?",                                                            
  "3" = "In your opinion, how important for your field is it that materials and/or code are openly available?",                                                                                                       
  "3" = "What is your experience with using open materials and/or code?",                                                                                                                                             
  "3" = "What is your experience with sharing open materials and/or code?",                                                                                                                                           
  "3" = "The following are possible concerns that researchers could have about making their materials and/or code openly available. Which of these concerns would apply to you?",                                     
  "4" = "In your opinion, how important for your field is it that data from published research are openly available?",                                                                                                
  "4" = "What is your experience with using open data?",                                                                                                                                                              
  "4" = "What is your experience with sharing open data?",                                                                                                                                                            
  "4" = "The following are possible concerns that researchers could have about making their data openly available. Which of these concerns would apply to you?",                                                      
  "5" = "In your opinion, how important is pre-publication archiving for your field?",                                                                                                                                
  "5" = "What is your experience with pre-publication archiving?",                                                                                                                                                    
  "5" = "The following are possible concerns that researchers could have about uploading a manuscript to a pre-publication archive before submitting it for peer review. Which of these concerns would apply to you?",
  "6" = "Approximately what proportion of your publications from the last 5 years are open access?",                                                                                                                  
  "6" = "Many open access journals charge a fee for processing the article for publication. How have you managed payment of these fees?",                                                                             
  "8" = "Please indicate your awareness of each of the open science resources listed below [Open Science Framework]",                                                                                                 
  "8" = "Please indicate your awareness of each of the open science resources listed below [GitHub]",                                                                                                                 
  "8" = "Please indicate your awareness of each of the open science resources listed below [EUR data repository/Figshare]",                                                                                           
  "8" = "Please indicate your awareness of each of the open science resources listed below [4TU Center for Research Data]",                                                                                           
  "8" = "Please indicate your awareness of each of the open science resources listed below [EUR SurfDrive]",                                                                                                          
  "8" = "Please indicate your awareness of each of the open science resources listed below [EUR Dropbox (not personal)]",                                                                                             
  "8" = "Please indicate your awareness of each of the open science resources listed below [FAIR data principles]",                                                                                                   
  "8" = "Please indicate your awareness of each of the open science resources listed below [EUR RePub]",                                                                                                              
  "8" = "Please indicate your awareness of each of the open science resources listed below [Zenodo]",                                                                                                                 
  "8" = "Please indicate your awareness of each of the open science resources listed below [Other 1]",                                                                                                                
  "8" = "Please indicate your awareness of each of the open science resources listed below [Other 2]",                                                                                                                
  "1" = "During March 2021 ERIM launched an ORCID campaign. Did you participate in it and got your own ORCID iD?",                                                                                                    
  "7" = "The following are possible barriers to the uptake of open science practices. Please place a tick beside any statement that you agree is a barrier in your field.",                                           
  "1" = "Are you sharing your knowledge about open science practices with others?",                                                                                                                                   
  "9" = "Do you expect that ERIM supports you in learning open science practices?",                                                                                                                                   
  "9" = "Which of the following open science practices would you like ERIM to provide information or support for?"
)

# Clean data --------------------------------------------------------

# subset of data with variables that can be plotted
ERIM_OS_clean <- 
  ERIM_OS %>% 
  select(-c(11, 12, 17, 22, 26, 29, 40, 42, 45, 47, 50)) %>%  # discard columns with free text
  rowid_to_column(var = "participant") %>%  # assign ID to each participant
  # convert to long format
  pivot_longer(
    3:tail(names(.), n = 1),
    names_to = "question",
    values_to = "value"
  ) %>% 
  # Multiple options can be selected for some questions
  # We need to separate answers into different columns
  bind_cols(
    split_into_multiple(
      .$value,
      pattern = ";",
      into_prefix = "value"
    )
  ) %>% 
  # delete column with redundant information
  select(-value) %>% 
  # convert all columns to factors
  mutate(
    across(
      .cols = everything(),
      .fns = ~ as_factor(.)
    )
  ) %>% 
  # add column with cluster
  mutate(
    cluster = fct_recode(question, !!!levels_question),
    .after = "Finished"
  )

ERIM_OS_clean

# save as .csv
write_csv(
  ERIM_OS_clean,
  here("data", "preproc", "CLEAN_20210608_ERIM_OS_Survey.csv")
)

# Cluster 0 ----------------------------------------------------------------

num_cluster <- 0

cluster <-
  ERIM_OS_clean %>%
  filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == num_cluster # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  select(-c(Finished, cluster)) %>% # drop unused columns
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  drop_na() %>% # drop rows with missing values
  rename("item" = "value_1") %>%
  mutate(question = factor(
    question,
    levels = c(
      "Which faculty are you from?",
      "Which department are you affiliated to? [RSM]",
      "Which department are you affiliated to? [ESE]",
      "What is your position?",
      "Are you member of any research institute affiliated with RSM or ESE?"
    ),
    ordered = TRUE
  )) %>%
  group_by(question, item) %>%
  summarize(number_responses = n()) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    prop = number_responses / sum(number_responses), # proportion
    perc = round(prop * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 1 ----------------------------------------------------------------

num_cluster <- 1

cluster <-
  ERIM_OS_clean %>%
  filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == num_cluster # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  select(-c(Finished, cluster)) %>% # drop unused columns
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  rename("item" = "value_1") %>%
  mutate(question = factor(
    question,
    levels = c(
      "What is your experience with open science practices?",
      "Are you sharing your knowledge about open science practices with others?",
      "During March 2021 ERIM launched an ORCID campaign. Did you participate in it and got your own ORCID iD?"
    ),
    ordered = TRUE
  )) %>%
  group_by(question, item) %>%
  summarize(number_responses = n()) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    prop = number_responses / sum(number_responses), # proportion
    perc = round(prop * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 2 ----------------------------------------------------------------

num_cluster <- 2

cluster <-
  ERIM_OS_clean %>%
  filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == num_cluster # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  select(-c(Finished, cluster)) %>% # drop unused columns
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  rename("item" = "value_1") %>%
  mutate(question = factor(
    question,
    levels = c(
      "In your opinion, how important for your field is it that researchers preregister their studies?",
      "What is your experience with study preregistration?",
      "The following are possible concerns that researchers could have about preregistering their studies. Which of these concerns would apply to you?"
    ),
    ordered = TRUE
  )) %>%
  group_by(question, item) %>%
  summarize(number_responses = n()) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    prop = number_responses / sum(number_responses), # proportion
    perc = round(prop * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 3 ----------------------------------------------------------------

num_cluster <- 3

cluster <-
  ERIM_OS_clean %>%
  filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == num_cluster # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  dplyr::select(-c(Finished, cluster)) %>% # drop unused columns
  dplyr::select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  rename("item" = "value_1") %>%
  mutate(question = factor(
    question,
    levels = c(
      "In your opinion, how important for your field is it that materials and/or code are openly available?",
      "What is your experience with using open materials and/or code?",
      "What is your experience with sharing open materials and/or code?",
      "The following are possible concerns that researchers could have about making their materials and/or code openly available. Which of these concerns would apply to you?"
    ),
    ordered = TRUE
  )) %>%
  group_by(question, item) %>%
  summarize(number_responses = n()) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    prop = number_responses / sum(number_responses), # proportion
    perc = round(prop * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 4 ----------------------------------------------------------------

num_cluster <- 4

cluster <-
  ERIM_OS_clean %>%
  filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == num_cluster # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  select(-c(Finished, cluster)) %>% # drop unused columns
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  rename("item" = "value_1") %>%
  mutate(question = factor(
    question,
    levels = c(
      "In your opinion, how important for your field is it that data from published research are openly available?",
      "What is your experience with using open data?",
      "What is your experience with sharing open data?",
      "The following are possible concerns that researchers could have about making their data openly available. Which of these concerns would apply to you?"
    ),
    ordered = TRUE
  )) %>%
  group_by(question, item) %>%
  summarize(number_responses = n()) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    prop = number_responses / sum(number_responses), # proportion
    perc = round(prop * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 5 ----------------------------------------------------------------

num_cluster <- 5

cluster <-
  ERIM_OS_clean %>%
  filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == num_cluster # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  select(-c(Finished, cluster)) %>% # drop unused columns
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  rename("item" = "value_1") %>%
  mutate(question = factor(
    question,
    levels = c(
      "In your opinion, how important is pre-publication archiving for your field?",
      "What is your experience with pre-publication archiving?",
      "The following are possible concerns that researchers could have about uploading a manuscript to a pre-publication archive before submitting it for peer review. Which of these concerns would apply to you?"
    ),
    ordered = TRUE
  )) %>%
  group_by(question, item) %>%
  summarize(number_responses = n()) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    prop = number_responses / sum(number_responses), # proportion
    perc = round(prop * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 6 ----------------------------------------------------------------

num_cluster <- 6

cluster <-
  ERIM_OS_clean %>%
  filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == num_cluster # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  select(-c(Finished, cluster)) %>% # drop unused columns
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  rename("item" = "value_1") %>%
  mutate(question = factor(
    question,
    levels = c(
      "Approximately what proportion of your publications from the last 5 years are open access?",
      "Many open access journals charge a fee for processing the article for publication. How have you managed payment of these fees?"
    ),
    ordered = TRUE
  )) %>%
  group_by(question, item) %>%
  summarize(number_responses = n()) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    prop = number_responses / sum(number_responses), # proportion
    perc = round(prop * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 7 ----------------------------------------------------------------

num_cluster <- 7

cluster <-
  ERIM_OS_clean %>%
  filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == num_cluster # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  select(-c(Finished, cluster)) %>% # drop unused columns
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  rename("item" = "value_1") %>%
  mutate(question = factor(question)) %>%
  group_by(question, item) %>%
  summarize(number_responses = n()) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    prop = number_responses / sum(number_responses), # proportion
    perc = round(prop * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 8 ----------------------------------------------------------------

num_cluster <- 8

cluster <-
  ERIM_OS_clean %>%
  filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == num_cluster # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  select(-c(Finished, cluster)) %>% # drop unused columns
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  filter(!is.na(value_1)) %>% # keep rows where response to value_1 is not NAs
  rename("item" = "value_1") %>%
  mutate(question = recode_factor(
    question,
    "Please indicate your awareness of each of the open science resources listed below [Open Science Framework]" = "Open Science Framework",    
    "Please indicate your awareness of each of the open science resources listed below [GitHub]" = "GitHub",      
    "Please indicate your awareness of each of the open science resources listed below [EUR data repository/Figshare]" = "EUR data repository/Figshare",
    "Please indicate your awareness of each of the open science resources listed below [4TU Center for Research Data]" = "4TU Center for Research Data",
    "Please indicate your awareness of each of the open science resources listed below [EUR SurfDrive]" = "EUR SurfDrive",
    "Please indicate your awareness of each of the open science resources listed below [EUR Dropbox (not personal)]" = "EUR Dropbox (not personal)",
    "Please indicate your awareness of each of the open science resources listed below [FAIR data principles]" = "FAIR data principles",
    "Please indicate your awareness of each of the open science resources listed below [EUR RePub]" = "EUR RePub",
    "Please indicate your awareness of each of the open science resources listed below [Zenodo]" = "Zenodo",                 
    "Please indicate your awareness of each of the open science resources listed below [Other 1]" = "Other 1",
    "Please indicate your awareness of each of the open science resources listed below [Other 2]" = "Other 2"
  )
  ) %>%
  group_by(question, item) %>%
  summarize(number_responses = n()) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    prop = number_responses / sum(number_responses), # proportion
    perc = round(prop * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 9 ----------------------------------------------------------------

num_cluster <- 9

cluster <-
  ERIM_OS_clean %>%
  filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == num_cluster # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  select(-c(Finished, cluster)) %>% # drop unused columns
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  filter(!is.na(value_1)) %>% # keep rows where response to value_1 is not NAs
  rename("item" = "value_1") %>%
  mutate(question = factor(
    question,
    levels = c(
      "Do you expect that ERIM supports you in learning open science practices?",
      "Which of the following open science practices would you like ERIM to provide information or support for?"
    ),
    ordered = TRUE
  )) %>%
  group_by(question, item) %>%
  summarize(number_responses = n()) %>%
  ungroup() %>%
  group_by(question) %>%
  mutate(
    prop = number_responses / sum(number_responses), # proportion
    perc = round(prop * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# save
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster", num_cluster, ".csv"))
)

# END ----------------------------------------------------------------
