
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
    here("data", "preproc", "cluster0.csv"),
    show_col_types = FALSE
  ) %>% 
  mutate(
    question = as_factor(question),
    item = as_factor(item)
  ) %>% 
  select(-c(prop, number_responses))

# extract questions
questions <- levels(cluster$question)

# Question 2, lollipop graph ----------------------------------------------------------------

num_question <- 2

data_cluster0_question2 <-
  cluster %>%
  filter(question == questions[num_question]) %>%
  droplevels()

lollipop_cluster0_question2 <-
  data_cluster0_question2 %>%
  ggplot(aes(x = reorder(item, perc), y = perc)) +
  geom_point(size = 6, color = "#0C8066") +
  geom_segment(aes(x = item, xend = item, y = 0, yend = perc), color = "#012328") +
  geom_label_repel(aes(item, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
  scale_y_continuous(
    breaks = seq(0, 30, 5),
    limits = c(0, 30)
  ) +
  labs(
    title = "RSM",
    x = ""
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster0_question2

# Question 3, lollipop graph ----------------------------------------------------------------

num_question <- 3

data_cluster0_question3 <-
  cluster %>%
  filter(question == questions[num_question]) %>%
  droplevels()

lollipop_cluster0_question3 <-
  data_cluster0_question3 %>%
  ggplot(aes(x = reorder(item, perc), y = perc)) +
  geom_point(size = 6, color = "#0C8066") +
  geom_segment(aes(x = item, xend = item, y = 0, yend = perc), color = "#012328") +
  geom_label_repel(aes(item, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
  scale_y_continuous(
    breaks = seq(0, 35, 5),
    limits = c(0, 35)
  ) +
  labs(
    title = "ESE",
    x = ""
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster0_question3

# Merge in one figure ----------------------------------------------------------------

lollipop_figure1 <-
  lollipop_cluster0_question2 / lollipop_cluster0_question3 +
  plot_annotation(
    title = "Department affiliation"
  ) &
  theme(plot.title = element_text(size = 26, hjust = .5))

lollipop_figure1

# save to file
ggsave(
  filename = "figure1.png",
  plot = lollipop_figure1,
  device = "png",
  path = here("img"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# END ----------------------------------------------------------------
