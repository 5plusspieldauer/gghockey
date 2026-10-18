# Add a hockey rink to a ggplot

Draws the rink markings and sets up the plot canvas in one call: the
rink layers, a coordinate system for the chosen orientation and, unless
`theme = FALSE`,
[`theme_rink()`](https://5plusspieldauer.github.io/gghockey/reference/theme_rink.md).
Add it right after
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html),
before your geoms, and map your data with regular horizontal rink
coordinates (`x` along the length, `y` across the width, in the frame of
`dimensions`). For `orientation = "vertical"` the display is rotated for
you, so the same data works in either orientation.

## Usage

``` r
annotate_rink(
  colour = "#143F84",
  fill = "#eaf2f8",
  dimensions = IIHF,
  orientation = c("horizontal", "vertical"),
  zones = c("dz", "nz", "oz"),
  xlim = NULL,
  ylim = NULL,
  margin = 0.5,
  zone_margin = 2,
  linewidth = 1,
  details = FALSE,
  slot = FALSE,
  slot_colour = "grey",
  slot_fill = NA,
  slot_alpha = 0.7,
  slot_linewidth = 1.25,
  logos = NULL,
  logo_alpha = 0.25,
  theme = TRUE
)
```

## Arguments

- colour:

  Colour of the rink markings.

- fill:

  Colour of the ice.

- dimensions:

  Rink dimensions to draw, e.g.
  [IIHF](https://5plusspieldauer.github.io/gghockey/reference/rink_dims.md)
  (default) or
  [NHL](https://5plusspieldauer.github.io/gghockey/reference/rink_dims.md);
  see
  [rink_dims](https://5plusspieldauer.github.io/gghockey/reference/rink_dims.md)
  and
  [`rink_dimensions()`](https://5plusspieldauer.github.io/gghockey/reference/rink_dimensions.md).

- orientation:

  `"horizontal"` (default) or `"vertical"` (the right-hand end, the
  offensive zone, at the top).

- zones:

  Which zones to show, a subset of `c("dz", "nz", "oz")` (default all
  three); `"oz"` is the high-`x` end. Sets `xlim` for you, e.g.
  `c("dz", "nz")` crops to the defensive and neutral zones. Ignored if
  `xlim` is given.

- xlim, ylim:

  Optional plot limits in rink coordinates, to crop by hand. `xlim`
  overrides `zones`; `ylim` defaults to the full width plus `margin`.

- margin:

  Space (in rink units) added beyond the end and side boards.

- zone_margin:

  Space added beyond a blue line when `zones` stops there, so the crop
  runs a little past the line instead of ending on it.

- linewidth:

  Multiplier for the rink line widths. ggplot2 line widths are absolute
  (mm on the page), so reduce this for small or heavily faceted rinks.

- details:

  If `TRUE`, draw the faceoff hash marks and the dot cut-outs.

- slot:

  If `TRUE`, shade the offensive-zone slot.

- slot_colour, slot_fill, slot_alpha, slot_linewidth:

  Slot styling.

- logos:

  Optional logos to place on the ice, as a named list with any of
  `centre`, `left` and `right` (centre ice, and just inside each blue
  line). Each entry is a path to a PNG file or an image already in
  memory (an array as returned by
  [`png::readPNG()`](https://rdrr.io/pkg/png/man/readPNG.html), a matrix
  of colours or a `raster`). Reading PNG files needs the png package. No
  logos ship with gghockey: bring the ones you have the rights to use.

- logo_alpha:

  Opacity of the logos, between 0 and 1.

- theme:

  If `TRUE` (default), add
  [`theme_rink()`](https://5plusspieldauer.github.io/gghockey/reference/theme_rink.md)
  (and, for vertical rinks, the matching aspect ratio). Set `FALSE` to
  supply your own theme.

## Value

A list of ggplot2 components (rink layers, a coordinate system and,
optionally, a theme) to add to a plot.

## See also

[`rink_geometry()`](https://5plusspieldauer.github.io/gghockey/reference/rink_geometry.md)
for the markings as data,
[`geom_rink_bin()`](https://5plusspieldauer.github.io/gghockey/reference/geom_rink_bin.md)
for heatmaps on the rink.

## Examples

``` r
library(ggplot2)

ggplot(shots, aes(x, y)) +
  annotate_rink() +
  geom_point(alpha = 0.3)


# Vertical, offensive zone only, with hash marks:
ggplot(shots, aes(x, y)) +
  annotate_rink(orientation = "vertical", zones = "oz", details = TRUE) +
  geom_point(aes(colour = factor(goal)), alpha = 0.5)


# NHL rink, in the colours of your choice:
ggplot() +
  annotate_rink(dimensions = NHL, colour = "grey30", fill = "white")
```
