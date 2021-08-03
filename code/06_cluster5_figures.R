
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

num_cluster <- 5

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
  rename("item" = "value_1") %>%
  mutate(question = factor(
    question,
    levels = c(
      "In your opinion, how important is pre-publication archiving for your field?",
      "What is your experience with pre-publication archiving?",
      "The following are possible concerns that researchers could have about uploading a manuscript to a pre-publication archive before submitting it for peer review. Which of these concerns would apply to you?"
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
    lab_perc = paste(perc, "%", sep = ""), # percentage as text (for labels)
    ymax = cumsum(prop), # top of each label
    ymin = c(0, head(ymax, n = -1)), # bottom of each label
    lab_pos = (ymax + ymin) / 2 # label position
  ) %>%
  ungroup()

# extract questions
questions <- levels(cluster$question)

# save for final report
write_csv(
  cluster,
  here("data", "preproc", paste0("cluster", num_cluster, ".csv"))
)

# Question 1 ----------------------------------------------------------------

num_question <- 1

data_cluster5_question1 <-
  cluster %>%
  filter(question == questions[num_question]) %>%
  droplevels() %>%
  select(-number_responses) %>%
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

donut_cluster5_question1 <-
  data_cluster5_question1 %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = item)) +
  geom_rect() +
  geom_label_repel(x = 3.5, aes(y = lab_pos, label = lab_perc, color = "white"), size = 6, fill = "white", color = "black", box.padding = 0.5) +
  scale_fill_viridis_d(option = "plasma") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  ggtitle(str_wrap(questions[num_question], width = 40)) + # if title is too long, split into two lines with specified max width
  guides(fill = guide_legend(nrow = length(unique(filter(cluster, question == questions[num_question])$item)), byrow = TRUE)) +
  theme_custom

donut_cluster5_question1

# save to file
ggsave(
  filename = paste0("donut_cluster", num_cluster, "_question", num_question, ".png"),
  plot = donut_cluster5_question1,
  device = "png",
  path = here("img"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 96
)

# Question 2 ----------------------------------------------------------------

num_question <- 2

data_cluster5_question2 <-
  cluster %>%
  filter(question == questions[num_question]) %>%
  droplevels() %>%
  select(-number_responses) %>%
  # reorder responses
  mutate(item = factor(
    item,
    levels = c(
      "Until now, I was unaware of \"pre-publication archiving\"",
      "I am aware of pre-publication archiving but have not used it",
      "I have some experience with pre-publication archiving",
      "I have extensive experience with preprint archiving",
      "I don’t know/prefer not to answer"
    ),
    ordered = TRUE
  ))

donut_cluster5_question2 <-
  data_cluster5_question2 %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = item)) +
  geom_rect() +
  geom_label_repel(x = 3.5, aes(y = lab_pos, label = lab_perc, color = "white"), size = 6, fill = "white", color = "black", box.padding = 0.5) +
  scale_fill_viridis_d(option = "plasma") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  ggtitle(str_wrap(questions[num_question], width = 40)) + # if title is too long, split into two lines with specified max width
  guides(fill = guide_legend(nrow = length(unique(filter(cluster, question == questions[num_question])$item)), byrow = TRUE)) +
  theme_custom

donut_cluster5_question2

# save to file
ggsave(
  filename = paste0("donut_cluster", num_cluster, "_question", num_question, ".png"),
  plot = donut_cluster5_question2,
  device = "png",
  path = here("img"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 96
)

# Question 3 ----------------------------------------------------------------

num_question <- 3

data_cluster5_question3 <-
  cluster %>%
  filter(question == questions[num_question]) %>%
  droplevels() %>%
  select(-number_responses) %>%
  # reorder responses
  mutate(item = factor(
    item,
    levels = c(
      "Non-peer-reviewed findings might add noise to the literature",
      "Some journals might not publish findings that are uploaded to a pre-publication archive",
      "Making my work available pre-publication might reduce the number of citations to the ultimately published work",
      "Other people might copy my research and publish it before I do",
      "Availability of the pre-publication manuscript might highlight differences (e.g., errors in analysis, revisions to hypotheses) between the original conception of the research and the ultimately published work",
      "I do not share any of these concerns",
      "Other"
    ),
    ordered = TRUE
  ))

donut_cluster5_question3 <-
  data_cluster5_question3 %>%
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = item)) +
  geom_rect() +
  geom_label_repel(x = 3.5, aes(y = lab_pos, label = lab_perc, color = "white"), size = 6, fill = "white", color = "black", box.padding = 0.5) +
  scale_fill_viridis_d(option = "plasma") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  ggtitle(str_wrap(questions[num_question], width = 40)) + # if title is too long, split into two lines with specified max width
  guides(fill = guide_legend(nrow = length(unique(filter(cluster, question == questions[num_question])$item)), byrow = TRUE)) +
  theme_custom +
  theme(legend.position = "right") # legend is too big to be put below the graph, so now it's on the right

donut_cluster5_question3

# save to file
ggsave(
  filename = paste0("donut_cluster", num_cluster, "_question", num_question, ".png"),
  plot = donut_cluster5_question3,
  device = "png",
  path = here("img"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 96
)

# END ----------------------------------------------------------------
