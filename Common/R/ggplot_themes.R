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

# Main challenge dark theme ====
challenge_theme_dark <- function(...) {
  theme_minimal(base_family = "Roboto",
                base_size = 11) +
    theme(
      text = element_text(colour = "#eeeeee"),
      title = element_text(family = "Open Sans", face = "bold"),
      plot.subtitle = element_text(face = "plain"),
      plot.caption = element_text(size = 9, face = "plain",
                                  family = "Roboto", colour = "#eeeeee",
                                  hjust = 0),
      panel.grid.major = element_line(colour = "#484848"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(colour = "#e0e0e0"),
      axis.text.y = element_text(colour = "#e0e0e0"),
      panel.background = element_rect(fill = "#212121", colour = NA),
      plot.background = element_rect(fill = "#212121", colour = NA)
    )
}

# Main challenge dark theme (no grid y lines) ====
challenge_theme_dark_no_grid.y <- function(...) {
  theme_minimal(base_family = "Roboto",
                base_size = 11) +
    theme(
      text = element_text(colour = "#eeeeee"),
      title = element_text(family = "Open Sans", face = "bold", size = 11),
      plot.subtitle = element_text(face = "plain"),
      plot.caption = element_text(size = 9, face = "plain",
                                  family = "Roboto", colour = "#eeeeee",
                                  hjust = 0),
      panel.grid.major = element_line(colour = "#484848"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.x = element_text(colour = "#e0e0e0"),
      axis.text.y = element_text(colour = "#e0e0e0"),
      panel.background = element_rect(fill = "#212121", colour = NA),
      plot.background = element_rect(fill = "#212121", colour = NA)
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

# 02 Lines theme ====
challenge_lines_theme <- function(...) {
  theme_minimal(base_family = "Roboto",
                base_size = 11) +
    theme(
      text = element_text(colour = "#eeeeee"),
      title = element_text(family = "Open Sans", face = "bold"),
      plot.subtitle = element_text(face = "plain"),
      plot.caption = element_text(size = 9, face = "plain",
                                  family = "Roboto", colour = "#eeeeee",
                                  hjust = 0),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.background = element_rect(fill = "#212121", colour = NA),
      plot.background = element_rect(fill = "#212121", colour = NA)
    )
}
