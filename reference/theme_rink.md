# Rink plot theme

A
[`ggplot2::theme_void()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)-based
theme for rink plots: no axes, gridlines or backgrounds, so only the
rink and its layers show. Titles are centred, facet strips are drawn as
padded coloured chips and legends get some breathing room.
[`annotate_rink()`](https://5plusspieldauer.github.io/gghockey/reference/annotate_rink.md)
adds it for you; call it yourself when you use
`annotate_rink(theme = FALSE)` or want different settings.

## Usage

``` r
theme_rink(
  base_size = 12,
  base_family = "",
  coord = TRUE,
  strip_fill = "#143F84",
  plot_margin = 12
)
```

## Arguments

- base_size:

  Base font size, in points.

- base_family:

  Base font family. The default `""` uses the device's default
  sans-serif font.

- coord:

  If `TRUE` (default), also return a 1:1
  [`ggplot2::coord_fixed()`](https://ggplot2.tidyverse.org/reference/coord_fixed.html)
  so the rink keeps its true proportions. Set `FALSE` when the plot
  already has a coordinate system, e.g. one added by
  [`annotate_rink()`](https://5plusspieldauer.github.io/gghockey/reference/annotate_rink.md).

- strip_fill:

  Fill colour of the facet strip labels.

- plot_margin:

  Margin around the whole plot, in points.

## Value

A ggplot2 theme or, with `coord = TRUE`, a list of a theme and a
coordinate system, to add to a plot.

## Examples

``` r
library(ggplot2)

ggplot(shots, aes(x, y)) +
  annotate_rink(theme = FALSE) +
  geom_point(alpha = 0.2) +
  facet_wrap(~period) +
  theme_rink(base_size = 10, coord = FALSE, strip_fill = "grey30")
```
