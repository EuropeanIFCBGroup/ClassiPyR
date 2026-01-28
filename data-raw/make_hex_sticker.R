# Script to generate ClassiPyR hex sticker
# Run this script to regenerate the logo

library(hexSticker)
library(ggplot2)

# Create a more realistic centric diatom (frustule-like)
# Based on Coscinodiscus/Thalassiosira appearance

create_diatom <- function() {
  # Outer frustule edge
  outer_circle <- function(n = 100) {
    theta <- seq(0, 2 * pi, length.out = n)
    r <- 0.85
    data.frame(x = r * cos(theta), y = r * sin(theta))
  }

  # Concentric rings
  inner_ring <- function(r, n = 80) {
    theta <- seq(0, 2 * pi, length.out = n)
    data.frame(x = r * cos(theta), y = r * sin(theta), r = r)
  }

  # Radial ribs (costae) - extended across entire diatom
  create_ribs <- function(n = 36) {
    angles <- seq(0, 2 * pi, length.out = n + 1)[-1]
    data.frame(
      x = 0, y = 0,
      xend = 0.82 * cos(angles),
      yend = 0.82 * sin(angles)
    )
  }

  outer <- outer_circle()
  rings <- do.call(rbind, lapply(c(0.15, 0.35, 0.55, 0.75), inner_ring))
  ribs <- create_ribs()

  # Build the plot
  p <- ggplot() +
    # Radial ribs (costae) across entire diatom
    geom_segment(
      data = ribs,
      aes(x = x, y = y, xend = xend, yend = yend),
      color = "#C5DFF0",
      linewidth = 0.3,
      alpha = 0.7
    ) +
    # Concentric rings
    geom_path(
      data = rings,
      aes(x = x, y = y, group = r),
      color = "#D0E8F5",
      linewidth = 0.5,
      alpha = 0.8
    ) +
    # Central point
    geom_point(
      data = data.frame(x = 0, y = 0),
      aes(x = x, y = y),
      color = "#E5F2FA",
      size = 3,
      alpha = 0.9
    ) +
    # Outer frustule edge
    geom_path(
      data = outer,
      aes(x = x, y = y),
      color = "#E0F0F8",
      linewidth = 1.2
    ) +
    coord_fixed(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA)
    )

  return(p)
}

# Generate the diatom subplot
diatom_plot <- create_diatom()

# Create the hex sticker - adjusted positioning
sticker(
  subplot = diatom_plot,
  package = "ClassiPyR",
  p_size = 16,
  p_color = "#FFFFFF",
  p_y = 1.48,
  p_fontface = "bold",
  s_x = 1,
  s_y = 0.78,
  s_width = 1.3,
  s_height = 1.3,
  h_fill = "#1A3A5C",
  h_color = "#3D8EC9",
  h_size = 1.4,
  filename = "man/figures/logo.png",
  dpi = 300
)

message("Hex sticker saved to man/figures/logo.png")

# SVG version - use smaller text size for SVG rendering
svg_sticker <- sticker(
  subplot = diatom_plot,
  package = "ClassiPyR",
  p_size = 6,
  p_color = "#FFFFFF",
  p_y = 1.48,
  p_fontface = "bold",
  s_x = 1,
  s_y = 0.78,
  s_width = 1.3,
  s_height = 1.3,
  h_fill = "#1A3A5C",
  h_color = "#3D8EC9",
  h_size = 1.4,
  filename = "man/figures/logo.svg"
)

message("Hex sticker saved to man/figures/logo.svg")
