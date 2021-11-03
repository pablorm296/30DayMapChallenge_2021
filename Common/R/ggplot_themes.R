# Main challenge theme ====
challenge_theme <- function(...) {
  theme_minimal(base_family = "Roboto",
                base_size = 11) +
    theme(
      text = element_text(colour = "#404040"),
      title = element_text(family = "Open Sans", face = "bold"),
      plot.subtitle = element_text(face = "plain"),
      plot.caption = element_text(size = 9, face = "plain",
                                  family = "Roboto", colour = "#606060",
                                  hjust = 0)
    )
}

# Theme without panel lines ====
challenge_theme_no_lines <- function(...) {
  theme_minimal(base_family = "Roboto",
                base_size = 11) +
    theme(
      text = element_text(colour = "#404040"),
      title = element_text(family = "Open Sans", face = "bold"),
      plot.subtitle = element_text(face = "plain"),
      plot.caption = element_text(size = 9, face = "plain",
                                  family = "Roboto", colour = "#606060",
                                  hjust = 0),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# 01 Points theme ====
challenge_points_theme <- function(...) {
  theme_minimal(base_family = "Roboto",
                base_size = 11) +
    theme(
      text = element_text(colour = "#404040"),
      title = element_text(family = "Open Sans", face = "bold"),
      plot.subtitle = element_text(face = "plain"),
      plot.caption = element_text(size = 9, face = "plain",
                                  family = "Roboto", colour = "#606060",
                                  hjust = 0),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    )
}
