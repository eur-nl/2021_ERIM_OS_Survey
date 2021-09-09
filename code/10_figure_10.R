
# RNG ---------------------------------------------------------------------

seed_synth <- 135249315
set.seed(seed_synth)

# Install packages --------------------------------------------------------

# install.packages("here")
# install.packages("tidyverse")
# install.packages("ggrepel")
# install.packages("patchwork")

# Load packages --------------------------------------------------------

library(here)
library(tidyverse)
library(ggrepel)
library(patchwork)

source(here("code", "theme_custom.R")) # custom ggplot2 theme

# Data ----------------------------------------------------------------

cluster <-
  read_csv(
    here("data", "preproc", "cluster5.csv"),
    show_col_types = FALSE
  ) %>% 
  mutate(
    question = as_factor(question),
    item = as_factor(item)
  ) %>%
  select(-number_responses)

# extract questions
questions <- levels(cluster$question)

# Question 1, lollipop graph ----------------------------------------------------------------

num_question <- 1

data_cluster5_question1 <-
  cluster %>%
  filter(question == questions[num_question]) %>%
  droplevels() %>% 
  mutate(item = str_wrap(item, width = 40))

lollipop_cluster5_question1 <-
  data_cluster5_question1 %>%
  ggplot(aes(x = reorder(item, perc), y = perc)) +
  geom_point(size = 6, color = "#0C8066") +
  geom_segment(aes(x = item, xend = item, y = 0, yend = perc), color = "#012328") +
  geom_label_repel(aes(item, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
  scale_y_continuous(
    breaks = seq(0, 30, 5),
    limits = c(0, 30)
  ) +
  labs(
    title = "Importance for your field",
    x = ""
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster5_question1

# Question 2, lollipop graph ----------------------------------------------------------------

num_question <- 2

data_cluster5_question2 <-
  cluster %>%
  filter(question == questions[num_question]) %>%
  droplevels() %>% 
  mutate(item = str_wrap(item, width = 40))

lollipop_cluster5_question2 <-
  data_cluster5_question2 %>%
  ggplot(aes(x = reorder(item, perc), y = perc)) +
  geom_point(size = 6, color = "#0C8066") +
  geom_segment(aes(x = item, xend = item, y = 0, yend = perc), color = "#012328") +
  geom_label_repel(aes(item, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
  scale_y_continuous(
    breaks = seq(0, 40, 5),
    limits = c(0, 40)
  ) +
  labs(
    title = "Your experience",
    x = ""
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster5_question2

# Merge in one figure ----------------------------------------------------------------

lollipop_figure10 <-
  lollipop_cluster5_question1 / lollipop_cluster5_question2 +
  plot_annotation(
    title = "Pre-publication archiving"
  ) &
  theme(plot.title = element_text(size = 26, hjust = .5))

lollipop_figure10

# save to file
ggsave(
  filename = "figure10.png",
  plot = lollipop_figure10,
  device = "png",
  path = here("img"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# END ----------------------------------------------------------------
