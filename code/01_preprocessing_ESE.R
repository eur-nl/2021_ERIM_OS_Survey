
# RNG ---------------------------------------------------------------------

seed_synth <- 135249315
set.seed(seed_synth)

# Install packages --------------------------------------------------------

# install.packages("here")
# install.packages("tidyverse")

# Load packages --------------------------------------------------------

library(here)
library(tidyverse)

# Load preprocessed data ----------------------------------------------------------------

ERIM_OS_clean <-
  read_csv(
    here("data", "preproc", "Total", "CLEAN_20210608_ERIM_OS_Survey.csv"),
    col_names = TRUE,
    show_col_types = FALSE
  )

# identify respondents who work at ESE
ESE_respondents <-
  ERIM_OS_clean %>% 
  filter(value_1 == "Erasmus School of Economics (ESE)") %>% 
  pull(participant)

# subset ESE survey data 
ESE <- 
  ERIM_OS_clean %>% 
  filter(participant %in% ESE_respondents)

# save as .csv
write_csv(
  ESE,
  here("data", "preproc", "ESE", "ESE_ERIM_OS_Survey.csv")
)

# Cluster 1 ----------------------------------------------------------------

num_cluster <- 1

cluster <-
  ESE %>%
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
  here("data", "preproc", "ESE", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 2 ----------------------------------------------------------------

num_cluster <- 2

cluster <-
  ESE %>%
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
  here("data", "preproc", "ESE", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 3 ----------------------------------------------------------------

num_cluster <- 3

cluster <-
  ESE %>%
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
  here("data", "preproc", "ESE", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 4 ----------------------------------------------------------------

num_cluster <- 4

cluster <-
  ESE %>%
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
  here("data", "preproc", "ESE", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 5 ----------------------------------------------------------------

num_cluster <- 5

cluster <-
  ESE %>%
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
  here("data", "preproc", "ESE", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 6 ----------------------------------------------------------------

num_cluster <- 6

cluster <-
  ESE %>%
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
  here("data", "preproc", "ESE", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 7 ----------------------------------------------------------------

num_cluster <- 7

cluster <-
  ESE %>%
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
  here("data", "preproc", "ESE", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 8 ----------------------------------------------------------------

num_cluster <- 8

cluster <-
  ESE %>%
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
  here("data", "preproc", "ESE", paste0("cluster", num_cluster, ".csv"))
)

# Cluster 9 ----------------------------------------------------------------

num_cluster <- 9

cluster <-
  ESE %>%
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
  here("data", "preproc", "ESE", paste0("cluster", num_cluster, ".csv"))
)

# END ----------------------------------------------------------------
