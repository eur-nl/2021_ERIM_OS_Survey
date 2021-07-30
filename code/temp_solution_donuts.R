# RNG ---------------------------------------------------------------------

seed_synth <- 135249315
set.seed(seed_synth)

# Install packages --------------------------------------------------------

# install.packages("here")
# install.packages("tidyverse")
# install.packages("viridis")

# Load packages --------------------------------------------------------

library(here)
library(tidyverse)
library(viridis)

# Custom ggplot theme ----------------------------------------------------------------

theme_custom <-
  theme_void(base_size = 14) +
  theme(
    # strip.text = element_text(
    #   hjust = .5,
    #   size = 20
    # ),
    plot.title = element_text(size = 22, hjust = .5),
    legend.box.background = element_rect(color = "transparent"),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
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

data_cluster1_question1 %>% 
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = item)) +
  geom_rect() +
  geom_label(x = 3.5, aes(y = lab_pos, label = lab_perc, color = "white"), size = 6, fill = "white", color = "black") +
  scale_fill_viridis_d(option = "plasma") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  ggtitle(questions[1]) +
  guides(fill = guide_legend(nrow = length(unique(filter(cluster, question == questions[1])$item)), byrow = TRUE)) +
  theme_custom
  
  























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










































# Create test data.
data <- data.frame(
  category=c("A", "B", "C"),
  count=c(10, 60, 30)
)

# Compute percentages
data$fraction = data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) # Try to remove that to see how to make a pie chart
