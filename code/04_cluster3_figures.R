
# RNG ---------------------------------------------------------------------

seed_synth <- 135249315
set.seed(seed_synth)

# Install packages --------------------------------------------------------

# install.packages("here")
# install.packages("tidyverse")
# install.packages("ggrepel")
# install.packages("viridis")

# Load packages --------------------------------------------------------

library(here)
library(tidyverse)
library(ggrepel)
library(viridis)

source(here("code", "theme_custom.R")) # custom ggplot2 theme

options(ggrepel.max.overlaps = Inf) # always show all labels, regardless of overlaps

# Data ----------------------------------------------------------------

cluster <-
  read_csv(
    here("data", "preproc", "CLEAN_20210608_ERIM_OS_Survey.csv"),
    show_col_types = FALSE
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
    prop = number_responses / sum(number_responses), # calculate proportion
    perc = round(prop * 100, 2), # calculate percentage
    ymax = cumsum(prop), # top of each rectangle
    ymin = c(0, head(ymax, n = -1)), # bottom of each rectangle
    lab_pos = (ymax + ymin) / 2, # label position
    lab_perc = paste(perc, "%", sep = ""), # percentage as text (for labels)
  ) %>%
  ungroup()

# extract questions
questions <- levels(cluster$question)

# save for final report
write_csv(
  cluster,
  here("data", "preproc", "cluster3.csv")
)

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
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = item)) +
  geom_rect() +
  geom_label_repel(x = 3.5, aes(y = lab_pos, label = lab_perc, color = "white"), size = 6, fill = "white", color = "black", box.padding = 0.5) +
  scale_fill_viridis_d(option = "plasma") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  ggtitle(str_wrap(questions[1], width = 40)) + # if title is too long, split into two lines with specified max width
  guides(fill = guide_legend(nrow = length(unique(filter(cluster, question == questions[1])$item)), byrow = TRUE)) +
  theme_custom
  
donut_cluster3_question1

# save to file
ggsave(
  filename = "donut_cluster3_question1.png",
  plot = donut_cluster3_question1,
  device = "png",
  path = here("img"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 96
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
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = item)) +
  geom_rect() +
  geom_label_repel(x = 3.5, aes(y = lab_pos, label = lab_perc, color = "white"), size = 6, fill = "white", color = "black", box.padding = 0.5) +
  scale_fill_viridis_d(option = "plasma") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  ggtitle(str_wrap(questions[2], width = 40)) +
  guides(fill = guide_legend(nrow = length(unique(filter(cluster, question == questions[2])$item)), byrow = TRUE)) +
  theme_custom
  
donut_cluster3_question2

# save to file
ggsave(
  filename = "donut_cluster3_question2.png",
  plot = donut_cluster3_question2,
  device = "png",
  path = here("img"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 96
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
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = item)) +
  geom_rect() +
  geom_label_repel(x = 3.5, aes(y = lab_pos, label = lab_perc, color = "white"), size = 6, fill = "white", color = "black", box.padding = 0.5) +
  scale_fill_viridis_d(option = "plasma") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  ggtitle(str_wrap(questions[3], width = 40)) +
  guides(fill = guide_legend(nrow = length(unique(filter(cluster, question == questions[3])$item)), byrow = TRUE)) +
  theme_custom

donut_cluster3_question3

# save to file
ggsave(
  filename = "donut_cluster3_question3.png",
  plot = donut_cluster3_question3,
  device = "png",
  path = here("img"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 96
)

# Question 4 ----------------------------------------------------------------

data_cluster3_question4 <-
  cluster %>%
  filter(question == questions[4]) %>%
  droplevels() %>%
  dplyr::select(-number_responses) %>%
  # reorder responses
  mutate(item = factor(
    item,
    levels = c(
      "Reuse of my materials and/or code could violate the epistemological framework of my research",
      "I might lose control over how my materials and/or code are being used",
      "There could be issues related to intellectual property",
      "Sharing these materials and/or code could result in others asking me to provide assistance for their research",
      "Others might find it difficult to understand my materials and/or code",
      "I might not receive appropriate credit for developing the materials and/or code if I make them openly available",
      "Other researchers might criticise my materials and/or code",
      "Other researchers might find errors in my published work",
      "I do not share any of these concerns",
      "Other",
      "I don’t know/prefer not to answer"
    ),
    ordered = TRUE
  ))

donut_cluster3_question4 <-
  data_cluster3_question4 %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = item)) +
  geom_rect() +
  geom_label_repel(x = 3.5, aes(y = lab_pos, label = lab_perc, color = "white"), size = 6, fill = "white", color = "black", box.padding = 0.5) +
  scale_fill_viridis_d(option = "plasma") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  ggtitle(str_wrap(questions[4], width = 40)) +
  guides(fill = guide_legend(nrow = length(unique(filter(cluster, question == questions[4])$item)), byrow = TRUE)) +
  theme_custom

donut_cluster3_question4

# save to file
ggsave(
  filename = "donut_cluster3_question4.png",
  plot = donut_cluster3_question4,
  device = "png",
  path = here("img"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 96
)

# END ----------------------------------------------------------------
