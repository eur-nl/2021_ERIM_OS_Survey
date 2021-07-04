
# RNG ---------------------------------------------------------------------

seed_synth <- 135249315
set.seed(seed_synth)

# Install packages --------------------------------------------------------

# install.packages("here")
# install.packages("tidyverse")
# install.packages("ggpubr")
# install.packages("viridis")

# Load packages --------------------------------------------------------

library(here)
library(tidyverse)
library(ggpubr)
library(viridis)

# Custom ggplot theme ----------------------------------------------------------------

theme_custom <-
  theme_pubr(base_size = 12) +
  theme(
    strip.text = element_text(
      hjust = .5,
      size = 20
    ),
    plot.title = element_text(size = 22, hjust = .5),
    legend.box.background = element_rect(color = "transparent"),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# Data ----------------------------------------------------------------

cluster <-
  read_csv(
    here("data", "preproc", "CLEAN_20210608_ERIM_OS_Survey.csv")
  ) %>%
  # convert all columns to factors
  mutate(
    across(
      .cols = everything(),
      .fns = ~ as_factor(.)
    )
  ) %>%
  dplyr::filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == "1" # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  dplyr::select(-c(Finished, cluster)) %>% # drop unused columns
  dplyr::select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
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
    perc = round(number_responses / sum(number_responses) * 100, 2), # calculate percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# extract questions
questions <- levels(cluster$question)

# Question 1 ----------------------------------------------------------------

data_cluster1_question1 <-
  cluster %>%
  filter(question == questions[1]) %>%
  droplevels() %>%
  dplyr::select(-number_responses) %>%
  # reorder responses
  mutate(item = factor(
    item,
    levels = c(
      "Until now, I was unaware of open science practices",
      "I am aware, but have not used open science practices in my research",
      "I have some experience with open science practices",
      "I have extensive experience with open science practices",
      "I don’t know/prefer not to answer"
    ),
    ordered = TRUE
  ))

donut_cluster1_question1 <-
  data_cluster1_question1 %>%
  ggdonutchart(
    "perc",
    label = "lab_perc",
    lab.pos = "out",
    lab.adjust = .4,
    color = "black",
    fill = "item",
    palette = plasma(length(unique(.$item))),
    ggtheme = theme_custom
  ) +
  ggtitle(questions[1]) +
  guides(fill = guide_legend(nrow = length(unique(filter(cluster, question == questions[1])$item)), byrow = TRUE))

donut_cluster1_question1

# save to file
ggsave(
  filename = "donut_cluster1_question1.png",
  plot = donut_cluster1_question1,
  device = "png",
  path = here("img"),
  scale = 1,
  width = 8,
  height = 8,
  units = "in",
  dpi = 600
)

# Question 2 ----------------------------------------------------------------

data_cluster1_question2 <-
  cluster %>%
  filter(question == questions[2]) %>%
  droplevels() %>%
  dplyr::select(-number_responses) %>%
  # reorder responses
  mutate(item = factor(
    item,
    levels = c(
      "No",
      "I share my knowledge with my colleagues informally",
      "I share my knowledge with my colleagues by offering seminars and/or giving talks",
      "I share my knowledge with my students or fellow research students",
      "I teach open science practices in postgraduate units",
      "I teach open science practices in undergraduate units",
      "I share my knowledge with others through forum discussions (e.g. Facebook groups)",
      "I share my knowledge with others through writing posting (e.g. blogs, webs, magazines)",
      "I don’t know/prefer not to answer",
      "Other"
    ),
    ordered = TRUE
  ))

donut_cluster1_question2 <-
  data_cluster1_question2 %>%
  ggdonutchart(
    "perc",
    label = "lab_perc",
    lab.pos = "out",
    lab.adjust = .4,
    color = "black",
    fill = "item",
    palette = plasma(length(unique(.$item))),
    ggtheme = theme_custom
  ) +
  ggtitle(questions[2]) +
  ggtitle("Are you sharing your knowledge about\nopen science practices with others?") + # title is too long, must be manually split into two lines
  guides(fill = guide_legend(nrow = length(unique(filter(cluster, question == questions[2])$item)), byrow = TRUE))

donut_cluster1_question2

# save to file
ggsave(
  filename = "donut_cluster1_question2.png",
  plot = donut_cluster1_question2,
  device = "png",
  path = here("img"),
  scale = 1,
  width = 8,
  height = 8,
  units = "in",
  dpi = 600
)

# Question 3 ----------------------------------------------------------------

data_cluster1_question3 <-
  cluster %>%
  filter(question == questions[3]) %>%
  droplevels() %>%
  drop_na() %>%
  # drop rows with missing values
  dplyr::select(-number_responses) %>%
  # reorder responses
  mutate(item = factor(
    item,
    levels = c(
      "No, I did not participate and I do not have an ORCID iD",
      "No, I did not participate since I already had an ORCID iD",
      "Yes, I participated and got an ORCID iD",
      "I don't know/prefer not to answer"
    ),
    ordered = TRUE
  ))

donut_cluster1_question3 <-
  data_cluster1_question3 %>%
  ggdonutchart(
    "perc",
    label = "lab_perc",
    lab.pos = "out",
    lab.adjust = .4,
    color = "black",
    fill = "item",
    palette = plasma(length(unique(.$item))),
    ggtheme = theme_custom
  ) +
  ggtitle("During March 2021 ERIM launched an ORCID campaign.\nDid you participate in it and got your own ORCID iD?") + # title is too long, must be manually split into two lines
  guides(fill = guide_legend(nrow = length(unique(filter(cluster, question == questions[3])$item)), byrow = TRUE))

donut_cluster1_question3

# save to file
ggsave(
  filename = "donut_cluster1_question3.png",
  plot = donut_cluster1_question3,
  device = "png",
  path = here("img"),
  scale = 1,
  width = 8,
  height = 8,
  units = "in",
  dpi = 600
)

# END ----------------------------------------------------------------
