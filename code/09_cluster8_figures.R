
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
      cluster == "8" # keep only questions of relevant cluster
  ) %>%
  droplevels() %>% # drop unused levels
  dplyr::select(-c(Finished, cluster)) %>% # drop unused columns
  dplyr::select_if(~ sum(!is.na(.)) > 0) %>% # keep columns without NAs
  dplyr::filter(!is.na(value_1)) %>% # keep rows where response to value_1 is not NAs
  rename("item" = "value_1") %>%
  mutate(question = recode_factor(
    question,
    "Please indicate your awareness of each of the open science resources listed below [Open Science Framework]" = "Open Science Framework",    
    "Please indicate your awareness of each of the open science resources listed below [GitHub]" = "GitHub",      
    "Please indicate your awareness of each of the open science resources listed below [EUR data repository/Figshare]" = "EUR data repository/Figshare",
    "Please indicate your awareness of each of the open science resources listed below [4TU Center for Research Data]" = "4TU Center for Research Data",
    "Please indicate your awareness of each of the open science resources listed below [EUR SurfDrive]" = "EUR SurfDrive",
    "Please indicate your awareness of each of the open science resources listed below [EUR Dropbox (not personal)]" = "EUR Dropbox (not personal)",
    "Please indicate your awareness of each of the open science resources listed below [FAIR data principles]" = "FAIR data principles",
    "Please indicate your awareness of each of the open science resources listed below [EUR RePub]" = "EUR RePub",
    "Please indicate your awareness of each of the open science resources listed below [Zenodo]" = "Zenodo",                 
    "Please indicate your awareness of each of the open science resources listed below [Other 1]" = "Other 1",
    "Please indicate your awareness of each of the open science resources listed below [Other 2]" = "Other 2"
    )
  ) %>%
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

# save for final report
write_csv(
  cluster,
  here("data", "preproc", "cluster8.csv")
)

# Questions ----------------------------------------------------------------

cluster <-
  cluster %>% 
  dplyr::filter(question != c("Other 1", "Other 2")) %>% # filter out "Other" responses
  droplevels()

# extract questions
questions <- levels(cluster$question)

for(i_question in 1:length(questions)){
  
  data_cluster8_question <-
    cluster %>%
    filter(question == questions[i_question]) %>%
    droplevels() %>%
    dplyr::select(-number_responses) %>%
    # reorder responses
    mutate(item = factor(
      item,
      levels = c(
        "I am unaware of this",
        "I am aware of it, but don't use it",
        "I use this"
      ),
      ordered = TRUE
    ))
  
  donut_cluster8_question <-
    data_cluster8_question %>%
    ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = item)) +
    geom_rect() +
    geom_label_repel(x = 3.5, aes(y = lab_pos, label = lab_perc, color = "white"), size = 6, fill = "white", color = "black", box.padding = 0.5) +
    scale_fill_viridis_d(option = "plasma") +
    coord_polar(theta = "y") +
    xlim(c(2, 4)) +
    ggtitle(str_wrap(questions[i_question], width = 40)) +
    guides(fill = guide_legend(nrow = length(unique(filter(cluster, question == questions[i_question])$item)), byrow = TRUE)) +
    theme_custom
  
  donut_cluster8_question
  
  # save to file
  ggsave(
    filename = paste0("donut_cluster8_question", i_question, ".png"),
    plot = donut_cluster8_question,
    device = "png",
    path = here("img"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 96
  )
  
}

# END ----------------------------------------------------------------
