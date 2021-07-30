
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
      cluster == "0" # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  dplyr::select(-c(Finished, cluster)) %>% # drop unused columns
  dplyr::select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
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
  here("data", "preproc", "cluster0.csv")
)

# Question 1 ----------------------------------------------------------------

data_cluster0_question1 <-
  cluster %>%
  filter(question == questions[1]) %>%
  droplevels() %>%
  dplyr::select(-number_responses)

donut_cluster0_question1 <-
  data_cluster0_question1 %>% 
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = item)) +
  geom_rect() +
  geom_label_repel(x = 3.5, aes(y = lab_pos, label = lab_perc, color = "white"), size = 6, fill = "white", color = "black", box.padding = 0.5) +
  scale_fill_viridis_d(option = "plasma") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  ggtitle(str_wrap(questions[1], width = 40)) + # if title is too long, split into two lines with specified max width
  guides(fill = guide_legend(nrow = length(unique(filter(cluster, question == questions[1])$item)), byrow = TRUE)) +
  theme_custom

donut_cluster0_question1

# save to file
ggsave(
  filename = "donut_cluster0_question1.png",
  plot = donut_cluster0_question1,
  device = "png",
  path = here("img"),
  scale = 1,
  width = 8,
  height = 8,
  units = "in",
  dpi = 600
)

# Question 2 ----------------------------------------------------------------

data_cluster0_question2 <-
  cluster %>%
  filter(question == questions[2]) %>%
  droplevels() %>%
  dplyr::select(-number_responses)

donut_cluster0_question2 <-
  data_cluster0_question2 %>% 
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = item)) +
  geom_rect() +
  geom_label_repel(x = 3.5, aes(y = lab_pos, label = lab_perc, color = "white"), size = 6, fill = "white", color = "black", box.padding = 0.5) +
  scale_fill_viridis_d(option = "plasma") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  ggtitle(str_wrap(questions[2], width = 40)) +
  guides(fill = guide_legend(nrow = length(unique(filter(cluster, question == questions[2])$item)), byrow = TRUE)) +
  theme_custom

donut_cluster0_question2

# save to file
ggsave(
  filename = "donut_cluster0_question2.png",
  plot = donut_cluster0_question2,
  device = "png",
  path = here("img"),
  scale = 1,
  width = 8,
  height = 8,
  units = "in",
  dpi = 600
)

# Question 3 ----------------------------------------------------------------

data_cluster0_question3 <-
  cluster %>%
  filter(question == questions[3]) %>%
  droplevels() %>%
  dplyr::select(-number_responses)

donut_cluster0_question3 <-
  data_cluster0_question3 %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = item)) +
  geom_rect() +
  geom_label_repel(x = 3.5, aes(y = lab_pos, label = lab_perc, color = "white"), size = 6, fill = "white", color = "black", box.padding = 0.5) +
  scale_fill_viridis_d(option = "plasma") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  ggtitle(str_wrap(questions[3], width = 40)) +
  theme_custom

donut_cluster0_question3

# save to file
ggsave(
  filename = "donut_cluster0_question3.png",
  plot = donut_cluster0_question3,
  device = "png",
  path = here("img"),
  scale = 1,
  width = 8,
  height = 8,
  units = "in",
  dpi = 600
)

# Question 4 ----------------------------------------------------------------

data_cluster0_question4 <- 
  cluster %>%
  filter(question == questions[4]) %>%
  droplevels() %>%
  dplyr::select(-number_responses) %>% 
  # reorder responses
  mutate(item = factor(
    item,
    levels = c(
      "PhD student",
      "Postdoc or researcher",
      "Tenure track",
      "Tenured faculty member"
    ),
    ordered = TRUE
  ))

donut_cluster0_question4 <-
  data_cluster0_question4 %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = item)) +
  geom_rect() +
  geom_label_repel(x = 3.5, aes(y = lab_pos, label = lab_perc, color = "white"), size = 6, fill = "white", color = "black", box.padding = 0.5) +
  scale_fill_viridis_d(option = "plasma") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  ggtitle(str_wrap(questions[4], width = 40)) +
  theme_custom

donut_cluster0_question4

# save to file
ggsave(
  filename = "donut_cluster0_question4.png",
  plot = donut_cluster0_question4,
  device = "png",
  path = here("img"),
  scale = 1,
  width = 8,
  height = 8,
  units = "in",
  dpi = 600
)

# Question 5 ----------------------------------------------------------------

data_cluster0_question5 <- 
  cluster %>%
  filter(question == questions[5]) %>%
  droplevels() %>%
  dplyr::select(-number_responses) %>% 
  # reorder responses
  mutate(item = factor(
    item,
    levels = c(
      "ERIM",
      "Tinbergen",
      "ERIM,Tinbergen",
      "Neither"
    ),
    ordered = TRUE
  ))

donut_cluster0_question5 <-
  data_cluster0_question5 %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = item)) +
  geom_rect() +
  geom_label_repel(x = 3.5, aes(y = lab_pos, label = lab_perc, color = "white"), size = 6, fill = "white", color = "black", box.padding = 0.5) +
  scale_fill_viridis_d(option = "plasma") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  ggtitle(str_wrap(questions[5], width = 40)) +
  theme_custom

donut_cluster0_question5

# save to file
ggsave(
  filename = "donut_cluster0_question5.png",
  plot = donut_cluster0_question5,
  device = "png",
  path = here("img"),
  scale = 1,
  width = 8,
  height = 8,
  units = "in",
  dpi = 600
)

# END ----------------------------------------------------------------
