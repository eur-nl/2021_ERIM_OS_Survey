
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

# EUR color palette (https://www.eur.nl/en/about-eur/house-style/brand-elements/colours)
EUR_palette <- c("#0C8066", # Erasmus Bright Green
                  "#012328", # Erasmus Green
                  "#E3DAD8") # Warm Grey

# Data ----------------------------------------------------------------

cluster <-
  read_csv(
    here("data", "preproc", "cluster8.csv"),
    show_col_types = FALSE
  ) %>% 
  mutate(
    question = as_factor(question),
    item = as_factor(item)
  ) %>%
  select(-number_responses) %>% 
  group_by(question) %>%
  mutate(
    ymax = cumsum(prop), # top of each label
    ymin = c(0, head(ymax, n = -1)), # bottom of each label
    lab_pos = (ymax + ymin) / 2 # label position
  ) %>%
  ungroup()

# Questions ----------------------------------------------------------------

cluster <-
  cluster %>% 
  filter(question != c("Other 1", "Other 2")) %>% # filter out "Other" responses
  droplevels()

# repeat it, or the filtering will not work
# (I know, it sounds crazy...)
cluster <-
  cluster %>% 
  filter(question != c("Other 1", "Other 2")) %>%
  droplevels()

# extract questions
questions <- levels(cluster$question)

donut_cluster8_question <- NULL # preallocate variable with all plots

for(i_question in 1:length(questions)){
  
  data_cluster8_question <-
    cluster %>%
    filter(question == questions[i_question]) %>%
    droplevels() %>%
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
  
  donut_cluster8_question[[i_question]] <-
    data_cluster8_question %>%
    ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = item)) +
    geom_rect() +
    geom_label_repel(x = 3.5, aes(y = lab_pos, label = lab_perc, color = "white"), size = 6, fill = "white", color = "black", box.padding = 0.5) +
    scale_fill_manual(values = EUR_palette) +
    coord_polar(theta = "y") +
    xlim(c(2, 4)) +
    ggtitle(questions[i_question]) +
    theme_void() +
    theme(
      plot.title = element_text(size = 22, hjust = .5),
      legend.title = element_blank(),
      legend.text = element_text(size = 14),
      legend.position = ifelse(i_question == 4, "none", "right")
    )
  
}

# all donuts in one figure
donut_figure14 <- 
  donut_cluster8_question[[1]] +
  donut_cluster8_question[[2]] +
  donut_cluster8_question[[3]] +
  donut_cluster8_question[[4]] +
  donut_cluster8_question[[5]] +
  donut_cluster8_question[[6]] +
  donut_cluster8_question[[7]] +
  donut_cluster8_question[[8]] +
  donut_cluster8_question[[9]] +
  plot_layout(guides = 'collect')

# save to file
ggsave(
  filename = "figure14.png",
  plot = donut_figure14,
  device = "png",
  path = here("img"),
  scale = 5,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# END ----------------------------------------------------------------
