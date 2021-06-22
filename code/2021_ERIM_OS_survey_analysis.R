
# RNG ---------------------------------------------------------------------

seed_synth <- 135249315 
set.seed(seed_synth)

# Install packages --------------------------------------------------------

# install.packages("here")
# install.packages("tidyverse")
# install.packages("viridisLite")

# Load packages --------------------------------------------------------

library(here)
library(tidyverse)
library(viridisLite)

# load custom function to split variable into multiple columns
source(here("code", "split_into_multiple.R"))

# Load data ----------------------------------------------------------------

# pseudonymized data from the 2021 ERIM Open Science Survey (retrieved on June 8th 2021) after manual cleaning 
ERIM_OS <-
  read_csv(
    here("data", "PSEUDONYM_manual_20210608_ERIM_OS_Survey.csv"),
    col_names = TRUE
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
  dplyr::select(-c(11, 12, 17, 22, 26, 29, 40, 42, 45, 47, 50)) %>%  # discard columns with free text
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
  dplyr::select(-value) %>% 
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

# incomplete questionnaires [summary] ----------------------------------------------------------------

# percentage of incomplete questionnaires
ERIM_OS_clean_unfinished <- 
  ERIM_OS_clean %>% 
  # mutate(Finished = as.logical(Finished)) %>%
  group_by(participant) %>%
  summarize(completed_survey = length(which(Finished == TRUE))/length(Finished)) %>%
  count(completed_survey) %>% 
  mutate(
    completed_survey = ifelse(completed_survey == 0, FALSE, TRUE),
    perc = round(n / sum(n) * 100, 2), # calculate percentage
    lab_perc = paste(round(n / sum(n) * 100, 2), "%", sep = "") # percentage as text (for plot labels)
  )

ERIM_OS_clean_unfinished

# cluster 0: demographics [summary] ----------------------------------------------------------------

ERIM_OS_clean_cluster0 <-
  ERIM_OS_clean %>% 
  dplyr::filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == "0" # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  dplyr::select(-c(Finished, cluster)) %>% # drop unused columns
  dplyr::select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  drop_na() %>% # drop rows with missing values
  rename("item" = "value_1") %>% 
  group_by(question, item) %>%
  summarize(number_responses = n()) %>%
  group_by(question) %>%
  mutate(
    perc = round(number_responses / sum(number_responses) * 100, 2), # calculate percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

ERIM_OS_clean_cluster0



















# cluster 0: graphs (donuts) ----------------------------------------------------------------

# questions as plot titles
donuts_title_survey_cluster0 <- levels(survey_cluster0$question)

# preallocate object with all plots
donuts_survey_cluster0 <- NULL

# loop through questions and make donut plots
for (i_donut in 1:length(levels(survey_cluster0$question))) {

  # subset data
  temp_survey_cluster0 <-
    survey_cluster0 %>%
    filter(question == levels(question)[i_donut]) %>%
    droplevels()

  # donut graph
  donuts_survey_cluster0[[i_donut]] <-
    temp_survey_cluster0 %>%
    ggdonutchart(
      "perc",
      label = "lab_perc",
      lab.pos = "out",
      color = "black",
      fill = "item",
      lab.font = c(8, "plain", "black"),
      palette = viridis(length(unique(temp_survey_cluster0$item)), option = "cividis"),
      ggtheme = theme_pubr()
    ) +
    ggtitle(donuts_title_survey_cluster0[i_donut]) +
    theme(
      plot.title = element_text(hjust = .5),
      legend.title = element_blank(),
      legend.position = "right"
    )

  # save to file
  ggsave(
    filename = paste0("donuts_survey_cluster0_question", i_donut, ".png"),
    plot = donuts_survey_cluster0[[i_donut]],
    device = "png",
    path = here("img/survey/cluster0"),
    width = 8,
    height = 8,
    units = "in",
    dpi = 300
  )
  
}

# cluster 1: OS, general knowledge ----------------------------------------------------------------

# list of possible answers
survey_cluster1_answers <- 
  c(# several questions
    "I don’t know/prefer not to answer",
    # question 5
    "Until now, I was unaware of open science practices", 
    "I am aware of open science, but have not used open science practices in my research",
    "I have some experience with open science practices",
    "I have extensive experience with open science practices",
    # question 6
    "No",
    "Yes, I share my knowledge with my colleagues informally",
    "Yes, I share my knowledge with my colleagues by offering seminars and/or giving talks",
    "Yes, I share my knowledge with my students or fellow research students",
    "Yes, I teach open science practices in postgraduate units",
    "Yes, I teach open science practices in undergraduate units",
    "Yes, I share my knowledge with others through forum discussions",
    "Yes, I share my knowledge with others through writing posting (e.g. blogs, webs, magazines)",
    # question 27
    "Yes"
  )

survey_cluster1 <-
  survey_data %>%
  filter(field_code == "1") %>%
  droplevels() %>%
  select_if(~ sum(!is.na(.)) > 0) %>%
  mutate(
    question = factor(
      question,
      levels = c(
        "OS experience",
        "Sharing OS knowledge",
        "Funders requiring open publications/data"
      )
    )
  ) %>%
  select(-c(date_time, field_code)) %>%
  pivot_longer(3:8, names_to = "response", values_to = "item") %>%
  group_by(item, question) %>%
  summarize(response = n()) %>%
  ungroup() %>%
  # keep NAs?
  filter(!is.na(item)) %>%
  select(question, item, response) %>%
  group_by(question) %>%
  arrange(desc(item)) %>%
  mutate(
    perc = round(response / sum(response) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>%
  ungroup() %>%
  arrange(question) %>% 
  mutate(item = ifelse(item %in% survey_cluster1_answers, item, "Other"))

# cluster 1: graphs (donuts) ----------------------------------------------------------------

# questions as plot titles
donuts_title_survey_cluster1 <- levels(survey_cluster1$question)

# preallocate object with all plots
donuts_survey_cluster1 <- NULL

# loop through questions and make donut plots
for (i_donut in 1:length(levels(survey_cluster1$question))) {

  # subset data
  temp_survey_cluster1 <-
    survey_cluster1 %>%
    filter(question == levels(question)[i_donut]) %>%
    droplevels()

  # donut graph
  donuts_survey_cluster1[[i_donut]] <-
    temp_survey_cluster1 %>%
    ggdonutchart(
      "perc",
      label = "lab_perc",
      lab.pos = "out",
      color = "black",
      fill = "item",
      lab.font = c(8, "plain", "black"),
      palette = viridis(length(unique(temp_survey_cluster1$item)), option = "cividis"),
      ggtheme = theme_pubr()
    ) +
    ggtitle(donuts_title_survey_cluster1[i_donut]) +
    theme(
      plot.title = element_text(hjust = .5),
      legend.title = element_blank(),
      legend.position = "right"
    )
  
}

# save to file (separately for each question, due to different image size)
# question 5 (#1 in cluster)
ggsave(
  filename = "donuts_survey_cluster1_question1.png",
  plot = donuts_survey_cluster1[[1]],
  device = "png",
  path = here("img/survey/cluster1"),
  width = 12,
  height = 12,
  units = "in",
  dpi = 300
)

# question 6 (#2 in cluster)
ggsave(
  filename = "donuts_survey_cluster1_question2.png",
  plot = donuts_survey_cluster1[[2]],
  device = "png",
  path = here("img/survey/cluster1"),
  width = 16,
  height = 16,
  units = "in",
  dpi = 300
)

# question 27 (#3 in cluster)
ggsave(
  filename = "donuts_survey_cluster1_question3.png",
  plot = donuts_survey_cluster1[[3]],
  device = "png",
  path = here("img/survey/cluster1"),
  width = 8,
  height = 8,
  units = "in",
  dpi = 300
)

# cluster 2: preregistration ----------------------------------------------------------------

# list of possible answers
survey_cluster2_answers <- 
  c(# several questions
    "I don’t know/prefer not to answer",
    # question 7
    "Extremely important", 
    "Somewhat important",
    "Somewhat unimportant",
    "Not at all important",
    "Researchers in my discipline do not conduct research studies",
    # question 8
    "Until now, I was unaware of study preregistration",
    "I am aware of study preregistration, but have not used it in my research",
    "I have some experience with study preregistration, but do not use it regularly",
    "I regularly preregister my studies",
    # question 9
    "It might delay data collection",
    "I need to look at my data before I can decide how to best analyse it",
    "Preregistration prevents exploratory research",
    "Preregistration stifles research creativity or flexibility",
    "Preregistration might lead to other people taking my research idea and implementing my plan",
    "Preregistration might make it more difficult to find statistically significant results",
    "Preregistration might make it more difficult to publish in certain journals",
    "I do not share any of these concerns"
  )

survey_cluster2 <-
  survey_data %>%
  filter(field_code == "2") %>%
  droplevels() %>%
  select_if(~ sum(!is.na(.)) > 0) %>%
  mutate(
    question = factor(
      question,
      levels = c(
        "Importance of preregistration in your field",
        "Experience with preregistration",
        "Concerns related to preregistration"
      )
    )
  ) %>%
  select(-c(date_time, field_code)) %>%
  pivot_longer(3:6, names_to = "response", values_to = "item") %>%
  group_by(item, question) %>%
  summarize(response = n()) %>%
  ungroup() %>%
  # keep NAs?
  filter(!is.na(item)) %>%
  select(question, item, response) %>%
  group_by(question) %>%
  arrange(desc(item)) %>%
  mutate(
    perc = round(response / sum(response) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>%
  ungroup() %>%
  arrange(question) %>% 
  mutate(item = ifelse(item %in% survey_cluster2_answers, item, "Other"))

# cluster 2: graphs (donuts) ----------------------------------------------------------------

# questions as plot titles
donuts_title_survey_cluster2 <- levels(survey_cluster2$question)

# preallocate object with all plots
donuts_survey_cluster2 <- NULL

# loop through questions and make donut plots
for (i_donut in 1:length(levels(survey_cluster2$question))) {

  # subset data
  temp_survey_cluster2 <-
    survey_cluster2 %>%
    filter(question == levels(question)[i_donut]) %>%
    droplevels()

  # donut graph
  donuts_survey_cluster2[[i_donut]] <-
    temp_survey_cluster2 %>%
    ggdonutchart(
      "perc",
      label = "lab_perc",
      lab.pos = "out",
      color = "black",
      fill = "item",
      lab.font = c(8, "plain", "black"),
      palette = viridis(length(unique(temp_survey_cluster2$item)), option = "cividis"),
      ggtheme = theme_pubr()
    ) +
    ggtitle(donuts_title_survey_cluster2[i_donut]) +
    theme(
      plot.title = element_text(hjust = .5),
      legend.title = element_blank(),
      legend.position = "right"
    )
  
}

# save to file (separately for each question, due to different image size)
# question 7 (#1 in cluster)
ggsave(
  filename = "donuts_survey_cluster2_question1.png",
  plot = donuts_survey_cluster2[[1]],
  device = "png",
  path = here("img/survey/cluster2"),
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)

# question 8 (#2 in cluster)
ggsave(
  filename = "donuts_survey_cluster2_question2.png",
  plot = donuts_survey_cluster2[[2]],
  device = "png",
  path = here("img/survey/cluster2"),
  width = 12,
  height = 12,
  units = "in",
  dpi = 300
)

# question 9 (#3 in cluster)
ggsave(
  filename = "donuts_survey_cluster2_question3.png",
  plot = donuts_survey_cluster2[[3]],
  device = "png",
  path = here("img/survey/cluster2"),
  width = 12,
  height = 12,
  units = "in",
  dpi = 300
)

# cluster 3: open materials/code ----------------------------------------------------------------

# list of possible answers
survey_cluster3_answers <- 
  c(# several questions
    "I don’t know/prefer not to answer",
    # question 10
    "Extremely important", 
    "Somewhat important",
    "Somewhat unimportant",
    "Not at all important",
    "Researchers in my discipline do not use materials and/or code",
    # question 11
    "Until now, I hadn't heard of open materials and/or code",
    "I am aware of open materials and/or code, but have not used it in my research",
    "I have some experience with open materials and/or code, but do not use them regularly",
    "I regularly use open materials and/or code",
    # question 12
    "Other researchers might criticise my materials and/or code",
    "Other researchers might find errors in my published work",
    "Others might find it difficult to understand my materials and/or code",
    "Reuse of my materials and/or code could violate the epistemological framework of my research",
    "Sharing these materials and/or code could result in others asking me to provide assistance for their research",
    "I might lose control over how my materials and/or code are being used",
    "I might not receive appropriate credit for developing the materials and/or code if I make them openly available",
    "There could be issues related to intellectual property",
    "I do not share any of these concerns"
  )

survey_cluster3 <-
  survey_data %>%
  filter(field_code == "3") %>%
  droplevels() %>%
  select_if(~ sum(!is.na(.)) > 0) %>%
  mutate(
    question = factor(
      question,
      levels = c(
        "Importance of open materials/code in your field",
        "Experience with open materials/code",
        "Concerns related to open materials/code"
      )
    )
  ) %>%
  select(-c(date_time, field_code)) %>%
  pivot_longer(3:7, names_to = "response", values_to = "item") %>%
  group_by(item, question) %>%
  summarize(response = n()) %>%
  ungroup() %>%
  # keep NAs?
  filter(!is.na(item)) %>%
  select(question, item, response) %>%
  group_by(question) %>%
  arrange(desc(item)) %>%
  mutate(
    perc = round(response / sum(response) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>%
  ungroup() %>%
  arrange(question) %>% 
  mutate(item = ifelse(item %in% survey_cluster3_answers, item, "Other"))

# cluster 3: graphs (donuts) ----------------------------------------------------------------

# questions as plot titles
donuts_title_survey_cluster3 <- levels(survey_cluster3$question)

# preallocate object with all plots
donuts_survey_cluster3 <- NULL

# loop through questions and make donut plots
for (i_donut in 1:length(levels(survey_cluster3$question))) {

  # subset data
  temp_survey_cluster3 <-
    survey_cluster3 %>%
    filter(question == levels(question)[i_donut]) %>%
    droplevels()

  # donut graph
  donuts_survey_cluster3[[i_donut]] <-
    temp_survey_cluster3 %>%
    ggdonutchart(
      "perc",
      label = "lab_perc",
      lab.pos = "out",
      color = "black",
      fill = "item",
      lab.font = c(8, "plain", "black"),
      palette = viridis(length(unique(temp_survey_cluster3$item)), option = "cividis"),
      ggtheme = theme_pubr()
    ) +
    ggtitle(donuts_title_survey_cluster3[i_donut]) +
    theme(
      plot.title = element_text(hjust = .5),
      legend.title = element_blank(),
      legend.position = "right"
    )
  
}

# save to file (separately for each question, due to different image size)
# question 10 (#1 in cluster)
ggsave(
  filename = "donuts_survey_cluster3_question1.png",
  plot = donuts_survey_cluster3[[1]],
  device = "png",
  path = here("img/survey/cluster3"),
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)

# question 11 (#2 in cluster)
ggsave(
  filename = "donuts_survey_cluster3_question2.png",
  plot = donuts_survey_cluster3[[2]],
  device = "png",
  path = here("img/survey/cluster3"),
  width = 12,
  height = 12,
  units = "in",
  dpi = 300
)

# question 12 (#3 in cluster)
ggsave(
  filename = "donuts_survey_cluster3_question3.png",
  plot = donuts_survey_cluster3[[3]],
  device = "png",
  path = here("img/survey/cluster3"),
  width = 20,
  height = 20,
  units = "in",
  dpi = 300
)

# cluster 4: open data ----------------------------------------------------------------

# list of possible answers
survey_cluster4_answers <- 
  c(# several questions
    "I don’t know/prefer not to answer",
    # question 13
    "Extremely important", 
    "Somewhat important",
    "Somewhat unimportant",
    "Not at all important",
    "Research publications in my field are not based on data",
    # question 14
    "Until now, I was unaware of open data",
    "I am aware of open data, but have not used this in my research",
    "I have some experience using open data, but do not use them regularly",
    "I regularly use open data",
    # question 15
    "Other researchers might criticise my data and/or research practices",
    "There could be issues related to intellectual property",
    "There could be issues related to ethics",
    "There could be issues related to privacy",
    "I think it is unfair for researchers beyond the original team to benefit (e.g., through future publications, career advancement) from my data collection",
    "I might not receive appropriate credit for my data collection",
    "I might lose control over how my data are being used",
    "Other researchers could use my data for another study that I intended to conduct in the future",
    "It devalues research data collection",
    "It will take too much time or effort to share research data",
    "I do not share any of these concerns"
  )

survey_cluster4 <-
  survey_data %>%
  filter(field_code == "4") %>%
  droplevels() %>%
  select_if(~ sum(!is.na(.)) > 0) %>%
  mutate(
    question = factor(
      question,
      levels = c(
        "Importance of open data in your field",
        "Experience with open data",
        "Concerns related to open data"
      )
    )
  ) %>%
  select(-c(date_time, field_code)) %>%
  pivot_longer(3:9, names_to = "response", values_to = "item") %>%
  group_by(item, question) %>%
  summarize(response = n()) %>%
  ungroup() %>%
  # keep NAs?
  filter(!is.na(item)) %>%
  select(question, item, response) %>%
  group_by(question) %>%
  arrange(desc(item)) %>%
  mutate(
    perc = round(response / sum(response) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>%
  ungroup() %>%
  arrange(question) %>% 
  mutate(item = ifelse(item %in% survey_cluster4_answers, item, "Other"))

# cluster 4: graphs (donuts) ----------------------------------------------------------------

# questions as plot titles
donuts_title_survey_cluster4 <- levels(survey_cluster4$question)

# preallocate object with all plots
donuts_survey_cluster4 <- NULL

# loop through questions and make donut plots
for (i_donut in 1:length(levels(survey_cluster4$question))) {

  # subset data
  temp_survey_cluster4 <-
    survey_cluster4 %>%
    filter(question == levels(question)[i_donut]) %>%
    droplevels()

  # donut graph
  donuts_survey_cluster4[[i_donut]] <-
    temp_survey_cluster4 %>%
    ggdonutchart(
      "perc",
      label = "lab_perc",
      lab.pos = "out",
      color = "black",
      fill = "item",
      lab.font = c(8, "plain", "black"),
      palette = viridis(length(unique(temp_survey_cluster4$item)), option = "cividis"),
      ggtheme = theme_pubr()
    ) +
    ggtitle(donuts_title_survey_cluster4[i_donut]) +
    theme(
      plot.title = element_text(hjust = .5),
      legend.title = element_blank(),
      legend.position = "right"
    )
  
}

# save to file (separately for each question, due to different image size)
# question 13 (#1 in cluster)
ggsave(
  filename = "donuts_survey_cluster4_question1.png",
  plot = donuts_survey_cluster4[[1]],
  device = "png",
  path = here("img/survey/cluster4"),
  width = 8,
  height = 8,
  units = "in",
  dpi = 300
)

# question 14 (#2 in cluster)
ggsave(
  filename = "donuts_survey_cluster4_question2.png",
  plot = donuts_survey_cluster4[[2]],
  device = "png",
  path = here("img/survey/cluster4"),
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)

# question 15 (#3 in cluster)
ggsave(
  filename = "donuts_survey_cluster4_question3.png",
  plot = donuts_survey_cluster4[[3]],
  device = "png",
  path = here("img/survey/cluster4"),
  width = 14,
  height = 14,
  units = "in",
  dpi = 300
)

# cluster 5: open access ----------------------------------------------------------------

# list of possible answers
survey_cluster5_answers <- 
  c(# several questions
    "I don’t know/prefer not to answer",
    # question 16
    "Extremely important", 
    "Somewhat important",
    "Somewhat unimportant",
    "Not at all important",
    # question 17
    "Until now, I was unaware of \"pre-publication archiving\"",
    "I am aware of pre-publication archiving but have not used it",
    "I have some experience with pre-publication archiving",
    "I have extensive experience with preprint archiving",
    # question 18
    "Some journals might not publish findings that are uploaded to a pre-publication archive",
    "Other people might copy my research and publish it before I do",
    "Non-peer-reviewed findings might add noise to the literature",
    "Making my work available pre-publication might reduce the number of citations to the ultimately published work",
    "Availability of the pre-publication manuscript might highlight differences (e.g., errors in analysis; revisions to hypotheses) between the original conception of the research and the ultimately published work",
    "I do not share any of these concerns",
    # question 19
    "All", "Most", "Half", "Some", "None",
    # question 20
    "My open access publications did not involve fees",
    "I paid the fees from my own research fund or from my institutional support",
    "I collaborated with others and other co-author paid the fees with their research funding",
    "I paid the fees with my personal money",
    "I could not afford to pay the fees and withdrew the manuscript",
    "I received a fee waiver from the journal"
  )

survey_cluster5 <-
  survey_data %>%
  filter(field_code == "5") %>%
  droplevels() %>%
  select_if(~ sum(!is.na(.)) > 0) %>%
  mutate(
    question = factor(
      question,
      levels = c(
        "Importance of pre-publication archiving in your field",
        "Experience with pre-publication archiving",
        "Concerns related to pre-publication archiving",
        "Proportion of your publications (last 5 years) that are open access",
        "APC payment for open access"
      )
    )
  ) %>%
  select(-c(date_time, field_code)) %>%
  pivot_longer(3:8, names_to = "response", values_to = "item") %>%
  group_by(item, question) %>%
  summarize(response = n()) %>%
  ungroup() %>%
  # keep NAs?
  filter(!is.na(item)) %>%
  select(question, item, response) %>%
  group_by(question) %>%
  arrange(desc(item)) %>%
  mutate(
    perc = round(response / sum(response) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>%
  ungroup() %>%
  arrange(question) %>% 
  mutate(item = ifelse(item %in% survey_cluster5_answers, item, "Other"))

# cluster 5: graphs (donuts) ----------------------------------------------------------------

# questions as plot titles
donuts_title_survey_cluster5 <- levels(survey_cluster5$question)

# preallocate object with all plots
donuts_survey_cluster5 <- NULL

# loop through questions and make donut plots
for (i_donut in 1:length(levels(survey_cluster5$question))) {

  # subset data
  temp_survey_cluster5 <-
    survey_cluster5 %>%
    filter(question == levels(question)[i_donut]) %>%
    droplevels()

  # donut graph
  donuts_survey_cluster5[[i_donut]] <-
    temp_survey_cluster5 %>%
    ggdonutchart(
      "perc",
      label = "lab_perc",
      lab.pos = "out",
      color = "black",
      fill = "item",
      lab.font = c(8, "plain", "black"),
      palette = viridis(length(unique(temp_survey_cluster5$item)), option = "cividis"),
      ggtheme = theme_pubr()
    ) +
    ggtitle(donuts_title_survey_cluster5[i_donut]) +
    theme(
      plot.title = element_text(hjust = .5),
      legend.title = element_blank(),
      legend.position = "right"
    )
  
}

# save to file (separately for each question, due to different image size)
# question 16 (#1 in cluster)
ggsave(
  filename = "donuts_survey_cluster5_question1.png",
  plot = donuts_survey_cluster5[[1]],
  device = "png",
  path = here("img/survey/cluster5"),
  width = 8,
  height = 8,
  units = "in",
  dpi = 300
)

# question 17 (#2 in cluster)
ggsave(
  filename = "donuts_survey_cluster5_question2.png",
  plot = donuts_survey_cluster5[[2]],
  device = "png",
  path = here("img/survey/cluster5"),
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)

# question 18 (#3 in cluster)
ggsave(
  filename = "donuts_survey_cluster5_question3.png",
  plot = donuts_survey_cluster5[[3]],
  device = "png",
  path = here("img/survey/cluster5"),
  width = 14,
  height = 14,
  units = "in",
  dpi = 300
)

# question 19 (#4 in cluster)
ggsave(
  filename = "donuts_survey_cluster5_question4.png",
  plot = donuts_survey_cluster5[[4]],
  device = "png",
  path = here("img/survey/cluster5"),
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)

# question 20 (#5 in cluster)
ggsave(
  filename = "donuts_survey_cluster5_question5.png",
  plot = donuts_survey_cluster5[[5]],
  device = "png",
  path = here("img/survey/cluster5"),
  width = 14,
  height = 14,
  units = "in",
  dpi = 300
)

# cluster 6: OS adoption/barriers ----------------------------------------------------------------

# list of possible answers
survey_cluster6_answers <- 
  c(# several questions
    "I don’t know/prefer not to answer",
    # question 21
    "Study preregistration",
    "Open materials/code",
    "Open data",
    "Pre-publication archiving",
    "Open access publishing",
    "Open access archiving in a repository",
    "None",
    # question 24
    "Lack of funding for open access publishing",
    "Lack of credit in my institution for engaging in open science",
    "Lack of recognition in my field about the value of open science practices",
    "Lack of mandates from funders, institutions or other regulators",
    "Lack of information about open science practices",
    "Lack of professional staff that provide support for open science practices",
    "Lack of research funding to support open science practices",
    "Lack of training required to implement open science practices",
    "Lack of supporting infrastructure (e.g., open data platforms)",
    "Lack of time to engage in open science practices",
    "Lack of time to learn open science practices",
    "Lack of expertise to engage in open science practices (e.g., assignment of metadata)",
    "Researchers are discouraged from engaging in open science practices by their colleagues",
    "HDR students are discouraged from engaging in open science practices by thesis supervisors",
    "The open science community is intimidating",
    "Researchers don't want to be told how to do their research",
    "Lack of interest from researchers",
    "I do not perceive any barriers"
  )

survey_cluster6 <-
  survey_data %>%
  filter(field_code == "6") %>%
  droplevels() %>%
  select_if(~ sum(!is.na(.)) > 0) %>%
  mutate(
    question = factor(
      question,
      levels = c(
        "Common OS practices in your field",
        "Barriers to OS in your field"
      )
    )
  ) %>%
  select(-c(date_time, field_code)) %>%
  pivot_longer(3:18, names_to = "response", values_to = "item") %>%
  group_by(item, question) %>%
  summarize(response = n()) %>%
  ungroup() %>%
  # keep NAs?
  filter(!is.na(item)) %>%
  select(question, item, response) %>%
  group_by(question) %>%
  arrange(desc(item)) %>%
  mutate(
    perc = round(response / sum(response) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>%
  ungroup() %>%
  arrange(question) %>% 
  mutate(item = ifelse(item %in% survey_cluster6_answers, item, "Other"))

# cluster 6: graphs (donuts) ----------------------------------------------------------------

# questions as plot titles
donuts_title_survey_cluster6 <- levels(survey_cluster6$question)

# preallocate object with all plots
donuts_survey_cluster6 <- NULL

# loop through questions and make donut plots
for (i_donut in 1:length(levels(survey_cluster6$question))) {

  # subset data
  temp_survey_cluster6 <-
    survey_cluster6 %>%
    filter(question == levels(question)[i_donut]) %>%
    droplevels()

  # donut graph
  donuts_survey_cluster6[[i_donut]] <-
    temp_survey_cluster6 %>%
    ggdonutchart(
      "perc",
      label = "lab_perc",
      lab.pos = "out",
      color = "black",
      fill = "item",
      lab.font = c(8, "plain", "black"),
      palette = viridis(length(unique(temp_survey_cluster6$item)), option = "cividis"),
      ggtheme = theme_pubr()
    ) +
    ggtitle(donuts_title_survey_cluster6[i_donut]) +
    theme(
      plot.title = element_text(hjust = .5),
      legend.title = element_blank(),
      legend.position = "right"
    )
  
}

# save to file (separately for each question, due to different image size)
# question 21 (#1 in cluster)
ggsave(
  filename = "donuts_survey_cluster6_question1.png",
  plot = donuts_survey_cluster6[[1]],
  device = "png",
  path = here("img/survey/cluster6"),
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)

# question 24 (#2 in cluster)
ggsave(
  filename = "donuts_survey_cluster6_question2.png",
  plot = donuts_survey_cluster6[[2]],
  device = "png",
  path = here("img/survey/cluster6"),
  width = 24,
  height = 24,
  units = "in",
  dpi = 300
)

# cluster 7: tool awareness ----------------------------------------------------------------

survey_cluster7_all <-
  survey_data %>%
  filter(field_code == "7") %>%
  droplevels() %>%
  select_if(~ sum(!is.na(.)) > 0) %>%
  mutate(
    question = factor(
      question,
      levels = c(
        "Awareness: OSF",
        "Awareness: Github",
        "Awareness: Figshare",
        "Awareness: Code Ocean",
        "Awareness: Data Cite (re3data)",
        "Awareness: LMU Open Science Toolbox",
        "Awareness: ORCID",
        "Awareness: FAIR data principles",
        "Awareness: Institutional repositories",
        "Awareness: Other"
      )
    )
  ) %>%
  select(participant, question, item = value_1) %>%
  group_by(item, question) %>%
  summarize(response = n()) %>%
  ungroup() %>%
  # keep NAs?
  filter(!is.na(item)) %>%
  select(question, item, response) %>%
  group_by(question) %>%
  arrange(desc(item)) %>%
  mutate(
    perc = round(response / sum(response) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>%
  ungroup() %>%
  arrange(question)

# for now, visualization will exclude responses to Awareness: Other
# because poorly formatted
survey_cluster7 <-
  survey_cluster7_all %>%
  filter(question != "Awareness: Other") %>%
  droplevels()

# cluster 7: graphs (donuts) ----------------------------------------------------------------

# questions as plot titles
donuts_title_survey_cluster7 <- levels(survey_cluster7$question)

# preallocate object with all plots
donuts_survey_cluster7 <- NULL

# loop through questions and make donut plots
for (i_donut in 1:length(levels(survey_cluster7$question))) {

  # subset data
  temp_survey_cluster7 <-
    survey_cluster7 %>%
    filter(question == levels(question)[i_donut]) %>%
    droplevels()

  # donut graph
  donuts_survey_cluster7[[i_donut]] <-
    temp_survey_cluster7 %>%
    ggdonutchart(
      "perc",
      label = "lab_perc",
      lab.pos = "out",
      color = "black",
      fill = "item",
      lab.font = c(8, "plain", "black"),
      palette = viridis(length(unique(temp_survey_cluster7$item)), option = "cividis"),
      ggtheme = theme_pubr()
    ) +
    ggtitle(donuts_title_survey_cluster7[i_donut]) +
    theme(
      plot.title = element_text(hjust = .5),
      legend.title = element_blank(),
      legend.position = "right"
    )

  # save to file
  ggsave(
    filename = paste0("donuts_survey_cluster7_question", i_donut, ".png"),
    plot = donuts_survey_cluster7[[i_donut]],
    device = "png",
    path = here("img/survey/cluster7"),
    width = 8,
    height = 8,
    units = "in",
    dpi = 300
  )
  
}

# cluster 8: role of IGDORE ----------------------------------------------------------------

# list of possible answers
survey_cluster8_answers <- 
  c(# several questions
    "I don’t know/prefer not to answer",
    # question 28
    "No", "Yes",
    # question 29
    "Preregistration",
    "Open Materials and/or Code",
    "Open Data",
    "Pre-publication archiving",
    "Open access publishing",
    "Open access archiving in a repository",
    "None",
    # question 30
    "Open Materials and/or Codes"
  )

survey_cluster8 <-
  survey_data %>%
  filter(field_code == "8") %>%
  droplevels() %>%
  select_if(~ sum(!is.na(.)) > 0) %>%
  mutate(
    question = factor(
      question,
      levels = c(
        "IGDORE support in learning OS",
        "IGDORE support in learning OS practices",
        "You support IGDORE members with OS"
      )
    )
  ) %>%
  select(-c(date_time, field_code)) %>%
  pivot_longer(3:8, names_to = "response", values_to = "item") %>%
  group_by(item, question) %>%
  summarize(response = n()) %>%
  ungroup() %>%
  # keep NAs?
  filter(!is.na(item)) %>%
  select(question, item, response) %>%
  group_by(question) %>%
  arrange(desc(item)) %>%
  mutate(
    perc = round(response / sum(response) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>%
  ungroup() %>%
  arrange(question) %>% 
  mutate(item = ifelse(item %in% survey_cluster8_answers, item, "Other"))

# cluster 8: graphs (donuts) ----------------------------------------------------------------

# questions as plot titles
donuts_title_survey_cluster8 <- levels(survey_cluster8$question)

# preallocate object with all plots
donuts_survey_cluster8 <- NULL

# loop through questions and make donut plots
for (i_donut in 1:length(levels(survey_cluster8$question))) {

  # subset data
  temp_survey_cluster8 <-
    survey_cluster8 %>%
    filter(question == levels(question)[i_donut]) %>%
    droplevels()

  # donut graph
  donuts_survey_cluster8[[i_donut]] <-
    temp_survey_cluster8 %>%
    ggdonutchart(
      "perc",
      label = "lab_perc",
      lab.pos = "out",
      color = "black",
      fill = "item",
      lab.font = c(8, "plain", "black"),
      palette = viridis(length(unique(temp_survey_cluster8$item)), option = "cividis"),
      ggtheme = theme_pubr()
    ) +
    ggtitle(donuts_title_survey_cluster8[i_donut]) +
    theme(
      plot.title = element_text(hjust = .5),
      legend.title = element_blank(),
      legend.position = "right"
    )
}

# save to file (separately for each question, due to different image size)
# question 28 (#1 in cluster)
ggsave(
  filename = "donuts_survey_cluster8_question1.png",
  plot = donuts_survey_cluster8[[1]],
  device = "png",
  path = here("img/survey/cluster8"),
  width = 8,
  height = 8,
  units = "in",
  dpi = 300
)

# question 29 (#2 in cluster)
ggsave(
  filename = "donuts_survey_cluster8_question2.png",
  plot = donuts_survey_cluster8[[2]],
  device = "png",
  path = here("img/survey/cluster8"),
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)

# question 30 (#3 in cluster)
ggsave(
  filename = "donuts_survey_cluster8_question3.png",
  plot = donuts_survey_cluster8[[3]],
  device = "png",
  path = here("img/survey/cluster8"),
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)

# upload graphs to Google Drive ----------------------------------------------------------------

# loop through cluster subfolders
for (i_cluster in levels(as_factor(survey_data$field_code))) {
  
  # list files in current subfolder
  local_files <- 
    list.files(
    path = here("img/survey", paste0("cluster", i_cluster)), 
    full.names = TRUE
  )
  
  # destination subfolder on Google Drive
  dest_folder <- 
    drive_mkdir(
      paste0("cluster", i_cluster),
      path = "IGDORE/2020_survey/2020-11_figures/",
      overwrite = TRUE
    )

  # upload graphs from current local subfolder to Google Drive
  files <- 
    map(
      local_files, 
      ~ drive_upload(
        .x, 
        path = dest_folder,
        verbose = FALSE)
    )
  
}

# END ----------------------------------------------------------------


