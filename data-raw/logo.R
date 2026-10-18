# Package hex logo -> man/figures/logo.png. Run from the package root:
#   Rscript data-raw/logo.R
# Needs the "Futura Hv BT" font (falls back to the default sans if missing).
library(ggplot2)
pkgload::load_all(quiet = TRUE)

teal   <- "#269681"
purple <- "#915DBD"
mint   <- "#EEF5F3"
font   <- if ("Futura Hv BT" %in% systemfonts::system_fonts()$family) "Futura Hv BT" else ""

hex <- function(r = 1) {
  t <- seq(30, 390, by = 60) / 180 * pi
  data.frame(x = r * cos(t), y = r * sin(t))
}

# A vertical offensive zone with a shot heatmap. Keep annotate_rink()'s theme
# (it carries the vertical rink's aspect ratio) and only strip the margins.
zone <- ggplot(shots, aes(x, y, z = xg)) +
  annotate_rink(orientation = "vertical", zones = "oz", colour = teal,
                fill = "white", linewidth = 0.4, details = TRUE) +
  geom_rink_bin(style = "size", binwidth = 2.4, scales = FALSE, alpha = 0.95) +
  scale_fill_gradientn(colours = c("#BFE0D8", teal, purple), guide = "none") +
  scale_size(range = c(0.9, 2.4), guide = "none") +
  theme(plot.margin = margin(0, 0, 0, 0), legend.position = "none")

# Open the PNG device first: ggplotGrob() below otherwise opens a default
# device and leaves an Rplots.pdf behind.
ragg::agg_png("man/figures/logo.png", width = 531, height = 600, res = 120,
              background = "transparent")

logo <- ggplot() +
  geom_polygon(aes(x, y), hex(1), fill = mint, colour = teal, linewidth = 2.5) +
  annotation_custom(ggplotGrob(zone), xmin = -0.6, xmax = 0.6, ymin = -0.36, ymax = 0.7) +
  annotate("text", x = 0, y = -0.58, label = "gghockey", colour = teal,
           size = 8.5, family = font) +
  coord_fixed(xlim = c(-0.92, 0.92), ylim = c(-1.04, 1.04), expand = FALSE) +
  theme_void()

print(logo)
invisible(dev.off())
