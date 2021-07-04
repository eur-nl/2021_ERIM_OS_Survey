
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
      cluster == "2" # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  dplyr::select(-c(Finished, cluster)) %>% # drop unused columns
  dplyr::select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
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
    perc = round(number_responses / sum(number_responses) * 100, 2), # calculate percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# extract questions
questions <- levels(cluster$question)

# Question 1 ----------------------------------------------------------------

data_cluster2_question1 <-
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
      "I don’t know/prefer not to answer"
    ),
    ordered = TRUE
  ))

donut_cluster2_question1 <-
  data_cluster2_question1 %>%
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
  ggtitle("In your opinion, how important for your field is it\nthat researchers preregister their studies?") + # title is too long, must be manually split into two lines
  guides(fill = guide_legend(nrow = length(unique(filter(cluster, question == questions[1])$item)), byrow = TRUE))

donut_cluster2_question1

# save to file
ggsave(
  filename = "donut_cluster2_question1.png",
  plot = donut_cluster2_question1,
  device = "png",
  path = here("img"),
  scale = 1,
  width = 8,
  height = 8,
  units = "in",
  dpi = 600
)



