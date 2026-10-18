# Binned heatmap of events on a rink

Bins events into a square grid and draws one square per cell, coloured
by the mean of the `z` aesthetic (e.g. expected goals, or any per-event
value). Squares are drawn in **rink units** and never exceed `binwidth`,
so they tile the grid without overlapping and look identical at any
export size. Add it to a rink from
[`annotate_rink()`](https://5plusspieldauer.github.io/gghockey/reference/annotate_rink.md);
facet it however you like.

## Usage

``` r
geom_rink_bin(
  mapping = NULL,
  data = NULL,
  position = "identity",
  ...,
  style = c("size", "fill", "alpha"),
  binwidth = 2.5,
  gap = 0,
  alpha = NULL,
  dimensions = IIHF,
  xlim = NULL,
  ylim = NULL,
  precomputed = FALSE,
  scales = TRUE,
  diverging = FALSE,
  colour = NA,
  linewidth = 0,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Aesthetic mapping, usually `aes(x, y, z)` (often set on the
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
  call), where `z` is averaged within each cell. `fill`, `size` and
  `alpha` are supplied by `style`; anything you map here overrides them.
  With `precomputed = TRUE`, the data needs `value` and `n` columns (as
  returned by
  [`rink_bin()`](https://5plusspieldauer.github.io/gghockey/reference/rink_bin.md)).

- data, position, na.rm, show.legend, inherit.aes:

  Standard ggplot2 layer arguments.

- ...:

  Other arguments passed to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).

- style:

  One of `"size"`, `"fill"` or `"alpha"` (see Details).

- binwidth:

  Cell size, in rink units. Also the largest square side.

- gap:

  For `"fill"` and `"alpha"`, the gap between squares in rink units (the
  square side is `binwidth - gap`).

- alpha:

  Constant square alpha for `"size"` and `"fill"` (default `0.9`).
  Ignored for `"alpha"`, where alpha is mapped.

- dimensions:

  Rink dimensions (e.g.
  [IIHF](https://5plusspieldauer.github.io/gghockey/reference/rink_dims.md),
  [NHL](https://5plusspieldauer.github.io/gghockey/reference/rink_dims.md))
  that set the binning bounds, so events on or behind the boards snap
  into the last in-rink cell. Pass the rink you are drawing, or `NULL`
  to bin without bounds. Ignored for any of `xlim` / `ylim` given, and
  when `precomputed = TRUE`.

- xlim, ylim:

  Optional binning bounds in rink coordinates.

- precomputed:

  If `TRUE`, skip binning and draw the cells as supplied, e.g. values
  you derived yourself from
  [`rink_bin()`](https://5plusspieldauer.github.io/gghockey/reference/rink_bin.md).

- scales:

  If `TRUE` (default), also return the matching scales for `style`:
  [`scale_fill_rink()`](https://5plusspieldauer.github.io/gghockey/reference/scale_rink.md)
  (or
  [`scale_fill_rink_diverging()`](https://5plusspieldauer.github.io/gghockey/reference/scale_rink.md)
  with `diverging = TRUE`) plus
  [`scale_size_rink()`](https://5plusspieldauer.github.io/gghockey/reference/scale_rink.md)
  for `"size"` or
  [`scale_alpha_rink()`](https://5plusspieldauer.github.io/gghockey/reference/scale_rink.md)
  for `"alpha"`. Set `FALSE` to add your own.

- diverging:

  Use the diverging fill scale, for maps of differences centred on 0
  (e.g. a player minus league average).

- colour, linewidth:

  Square border colour and width. The default draws no border; use e.g.
  `colour = "white", linewidth = 0.3` for gridlines.

## Value

A ggplot2 layer, or a list of a layer and scales.

## Details

Three ready-made styles cover the common maps; pick one with `style`:

- `"size"`:

  Square **size** encodes the event count (area proportional to
  frequency), **fill** the mean value.

- `"fill"`:

  Constant-size squares that fill the cell (leave a `gap` for thin
  gridlines); **fill** encodes the mean value.

- `"alpha"`:

  Constant-size squares whose **alpha** encodes the event count and
  **fill** the mean value.

Without a `z` aesthetic there is no value to average; map `fill` to
`after_stat(count)` for a pure frequency map (see examples).

## See also

[`rink_bin()`](https://5plusspieldauer.github.io/gghockey/reference/rink_bin.md),
[`scale_fill_rink()`](https://5plusspieldauer.github.io/gghockey/reference/scale_rink.md),
[`annotate_rink()`](https://5plusspieldauer.github.io/gghockey/reference/annotate_rink.md)

## Examples

``` r
library(ggplot2)

base <- ggplot(shots, aes(x, y, z = xg)) +
  annotate_rink(orientation = "vertical", zones = "oz",
                colour = "grey60", linewidth = 0.5)

base + geom_rink_bin(style = "size", binwidth = 2)

base + geom_rink_bin(style = "fill", binwidth = 2, gap = 0.1)

base + geom_rink_bin(style = "alpha", binwidth = 2)


# Frequency only: fill by count, your own scale
base +
  geom_rink_bin(aes(fill = after_stat(count)), style = "fill",
                binwidth = 2, scales = FALSE) +
  scale_fill_rink(name = "Shots", labels = NULL)
```
