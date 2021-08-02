
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

# END ----------------------------------------------------------------
