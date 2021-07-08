
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
      cluster == "3" # keep only questions of relevant cluster
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
    perc = round(number_responses / sum(number_responses) * 100, 2), # calculate percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# extract questions
questions <- levels(cluster$question)

# Question 1 ----------------------------------------------------------------

data_cluster3_question1 <-
  cluster %>%
  filter(question == questions[1]) %>%
  droplevels() %>%
  dplyr::select(-number_responses) %>%
  # reorder responses
  mutate(item = factor(
    item,
    levels = c(
      "Extremely important",
      "Very important",
      "Moderately important",
      "Slightly important",
      "Not at all important",
      "Researchers in my discipline do not use materials and/or code",
      "I don’t know/prefer not to answer"
    ),
    ordered = TRUE
  ))

donut_cluster3_question1 <-
  data_cluster3_question1 %>%
  ggdonutchart(
    "perc",
    label = "lab_perc",
    lab.pos = "out",
    color = "black",
    fill = "item",
    palette = plasma(length(unique(.$item))),
    ggtheme = theme_custom
  ) +
  ggtitle("In your opinion, how important for your field is it\nthat materials and/or code are openly available?") + # title is too long, must be manually split into two lines
  guides(fill = guide_legend(nrow = length(unique(filter(cluster, question == questions[1])$item)), byrow = TRUE))

donut_cluster3_question1

# save to file
ggsave(
  filename = "donut_cluster3_question1.png",
  plot = donut_cluster3_question1,
  device = "png",
  path = here("img"),
  scale = 1,
  width = 8,
  height = 8,
  units = "in",
  dpi = 600
)

# Question 2 ----------------------------------------------------------------

data_cluster3_question2 <-
  cluster %>%
  filter(question == questions[2]) %>%
  droplevels() %>%
  dplyr::select(-number_responses) %>%
  # reorder responses
  mutate(item = factor(
    item,
    levels = c(
      "Until now, I hadn't heard of open materials and/or code",
      "I am aware of open materials and/or code, but have not used it in my research",
      "I have some experience with open materials and/or code, but do not use them regularly",
      "I regularly use and/or create open materials and/or code",
      "I don’t know/prefer not to answer"
    ),
    ordered = TRUE
  ))

donut_cluster3_question2 <-
  data_cluster3_question2 %>%
  ggdonutchart(
    "perc",
    label = "lab_perc",
    lab.pos = "out",
    color = "black",
    fill = "item",
    palette = plasma(length(unique(.$item))),
    ggtheme = theme_custom
  ) +
  ggtitle("What is your experience with\nusing open materials and/or code?") + # title is too long, must be manually split into two lines
  guides(fill = guide_legend(nrow = length(unique(filter(cluster, question == questions[2])$item)), byrow = TRUE))

donut_cluster3_question2

# save to file
ggsave(
  filename = "donut_cluster3_question2.png",
  plot = donut_cluster3_question2,
  device = "png",
  path = here("img"),
  scale = 1,
  width = 8,
  height = 8,
  units = "in",
  dpi = 600
)

# Question 3 ----------------------------------------------------------------

data_cluster3_question3 <-
  cluster %>%
  filter(question == questions[3]) %>%
  droplevels() %>%
  dplyr::select(-number_responses) %>%
  # reorder responses
  mutate(item = factor(
    item,
    levels = c(
      "Until now, I hadn't heard of open materials and/or code",
      "I am aware of open materials and/or code, but have not shared my own",
      "I have some experience with open materials and/or code, but do not share mine regularly",
      "I regularly share open materials and/or code",
      "I don’t know/prefer not to answer"
    ),
    ordered = TRUE
  ))

donut_cluster3_question3 <-
  data_cluster3_question3 %>%
  ggdonutchart(
    "perc",
    label = "lab_perc",
    lab.pos = "out",
    color = "black",
    fill = "item",
    palette = plasma(length(unique(.$item))),
    ggtheme = theme_custom
  ) +
  ggtitle("What is your experience with\nsharing open materials and/or code?") + # title is too long, must be manually split into two lines
  guides(fill = guide_legend(nrow = length(unique(filter(cluster, question == questions[3])$item)), byrow = TRUE))

donut_cluster3_question3

# save to file
ggsave(
  filename = "donut_cluster3_question3.png",
  plot = donut_cluster3_question3,
  device = "png",
  path = here("img"),
  scale = 1,
  width = 8,
  height = 8,
  units = "in",
  dpi = 600
)

# Question 4 ----------------------------------------------------------------




















