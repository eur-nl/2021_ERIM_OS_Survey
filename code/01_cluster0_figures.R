
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
      cluster == "0" # keep only questions of relevant cluster
  ) %>%
  droplevels() %>%
  # drop unused levels
  dplyr::select(-c(Finished, cluster)) %>%
  # drop unused columns
  dplyr::select_if(~ sum(!is.na(.)) > 0) %>%
  # keep columns without NAs
  drop_na() %>%
  # drop rows with missing values
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
    )
  ) %>% 
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

donut_cluster0_question1 <- 
  cluster %>% 
  filter(question == questions[1]) %>% # subset data
  droplevels() %>% 
  dplyr::select(-number_responses) %>% 
  ggdonutchart(
    "perc",
    label = "lab_perc",
    lab.pos = "out",
    color = "black",
    fill = "item",
    lab.font = c(12, "plain", "black"),
    palette = plasma(length(unique(.$item))),
    ggtheme = theme_custom
  ) +
  ggtitle(questions[1]) 

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









