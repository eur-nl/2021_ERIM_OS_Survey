
# RNG ---------------------------------------------------------------------

seed_synth <- 135249315
set.seed(seed_synth)

# Install packages --------------------------------------------------------

# install.packages("here")
# install.packages("tidyverse")
# install.packages("patchwork")

# Load packages --------------------------------------------------------

library(here)
library(tidyverse)
library(patchwork)

source(here("code", "theme_custom.R")) # custom ggplot2 theme

# Data ----------------------------------------------------------------

num_cluster <- 0

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
  filter(
    Finished == "TRUE" & # keep only complete questionnaires
      cluster == num_cluster # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  select(-c(Finished, cluster)) %>% # drop unused columns
  select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
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
    prop = number_responses / sum(number_responses), # proportion
    perc = round(prop * 100, 2), # percentage
    lab_perc = paste(perc, "%", sep = "") # percentage as text (for labels)
  ) %>%
  ungroup()

# extract questions
questions <- levels(cluster$question)

# save for final report
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster", num_cluster, ".csv"))
)

# Question 2, lollipop graph ----------------------------------------------------------------

num_question <- 2

data_cluster0_question2 <-
  cluster %>%
  filter(question == questions[num_question]) %>%
  droplevels() %>%
  select(-number_responses)

lollipop_cluster0_question2 <-
  data_cluster0_question2 %>%
  ggplot(aes(x = reorder(item, perc), y = perc)) +
  geom_point(size = 6, color = "#0C8066") +
  geom_segment(aes(x = item, xend = item, y = 0, yend = perc), color = "#012328") +
  geom_label(aes(item, perc, label = lab_perc), colour = "#171C54", nudge_y = -3, size = 4) +
  scale_y_continuous(
    breaks = seq(0, 30, 5),
    limits = c(0, 30)
  ) +
  labs(
    title = "RSM",
    x = "",
    y = "%"
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster0_question2

# Question 3, lollipop graph ----------------------------------------------------------------

num_question <- 3

data_cluster0_question3 <-
  cluster %>%
  filter(question == questions[num_question]) %>%
  droplevels() %>%
  select(-number_responses)

lollipop_cluster0_question3 <-
  data_cluster0_question3 %>%
  ggplot(aes(x = reorder(item, perc), y = perc)) +
  geom_point(size = 6, color = "#0C8066") +
  geom_segment(aes(x = item, xend = item, y = 0, yend = perc), color = "#012328") +
  geom_label(aes(item, perc, label = lab_perc), colour = "#171C54", nudge_y = -3, size = 4) +
  scale_y_continuous(
    breaks = seq(0, 30, 5),
    limits = c(0, 30)
  ) +
  labs(
    title = "ESE",
    x = "",
    y = "%"
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster0_question3

# Merge in one figure ----------------------------------------------------------------

lollipop_cluster0 <-
  lollipop_cluster0_question2 / lollipop_cluster0_question3 +
  plot_annotation(
    title = str_sub(questions[3], end = -7)
  ) &
  theme(plot.title = element_text(size = 26, hjust = .5))

lollipop_cluster0

# save to file
ggsave(
  filename = paste0("lollipop_cluster", num_cluster, ".png"),
  plot = lollipop_cluster0,
  device = "png",
  path = here("img"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# END ----------------------------------------------------------------
