theme_custom <-
  ggplot2::theme_classic(base_size = 16) +
  ggplot2::theme(
    plot.title = element_text(size = 22, hjust = .5),
    plot.background = element_rect(fill = "white", color = "transparent"),
    legend.box.background = element_rect(color = "transparent"),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "right"
  )
