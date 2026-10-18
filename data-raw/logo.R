# Package hex logo -> man/figures/logo.png. Run from the package root after
# installing / loading gghockey:  Rscript data-raw/logo.R
library(ggplot2)
pkgload::load_all(quiet = TRUE)

hex <- function(r = 1) {
  t <- seq(30, 390, by = 60) / 180 * pi
  data.frame(x = r * cos(t), y = r * sin(t))
}

# A vertical offensive zone with a small shot heatmap, inside a hexagon.
set.seed(7)
oz <- shots[shots$team == "Lakeside", ]

zone <- ggplot(oz, aes(x, y, z = xg)) +
  annotate_rink(orientation = "vertical", zones = "oz", colour = "white",
                fill = "#143F84", linewidth = 0.6, details = TRUE, theme = FALSE) +
  geom_rink_bin(style = "size", binwidth = 2.2, scales = FALSE, alpha = 1) +
  scale_fill_gradientn(colours = grDevices::hcl.colors(256, "Rocket")[90:256], guide = "none") +
  scale_size(range = c(0.8, 2.2), guide = "none") +
  theme_void()

logo <- ggplot() +
  geom_polygon(aes(x, y), hex(1), fill = "#143F84", colour = "#eaf2f8", linewidth = 3) +
  annotation_custom(ggplotGrob(zone), xmin = -0.52, xmax = 0.52, ymin = -0.42, ymax = 0.66) +
  annotate("text", x = 0, y = -0.6, label = "gghockey", colour = "white",
           size = 7.5, fontface = "bold") +
  coord_fixed(xlim = c(-0.92, 0.92), ylim = c(-1.04, 1.04), expand = FALSE) +
  theme_void()

ragg::agg_png("man/figures/logo.png", width = 531, height = 600, res = 120,
              background = "transparent")
print(logo)
invisible(dev.off())
