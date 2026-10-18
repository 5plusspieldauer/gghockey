# Rink dimensions

Named lists describing a rink's markings, in metres, with the origin at
the bottom-left corner of the boards: `x` runs along the length of the
rink (left end boards to right end boards) and `y` across its width.
`IIHF` is the standard 60 x 30 m international rink, `NHL` the 200 x 85
ft North American rink converted to metres. Use
[`rink_dimensions()`](https://5plusspieldauer.github.io/gghockey/reference/rink_dimensions.md)
to derive a variant.

## Usage

``` r
IIHF

NHL
```

## Format

A named list. Geometry fields are in metres: `length`, `width`,
`blue_line` and `end_line` (distance from the end boards),
`faceoff_dot_x` / `faceoff_dot_y` (end-zone dot offsets from the
corner), `nz_faceoff_dot` (neutral-zone dot offset from the blue line),
`faceoff_circle` (radius), the hash-mark, crease and goal measures, and
`corner_border_start_x` / `corner_border_start_y` (corner radii).
`origin_x` / `origin_y` place the corner of the boards in data
coordinates, `lines_small` / `lines_big` are the ggplot2 line widths for
thin and thick markings, and `type` is `"IIHF"` or `"NHL"`.

An object of class `list` of length 31.

An object of class `list` of length 31.

## See also

[`rink_dimensions()`](https://5plusspieldauer.github.io/gghockey/reference/rink_dimensions.md),
[`annotate_rink()`](https://5plusspieldauer.github.io/gghockey/reference/annotate_rink.md),
[`rink_geometry()`](https://5plusspieldauer.github.io/gghockey/reference/rink_geometry.md)

## Examples

``` r
IIHF$length
#> [1] 60
NHL$length
#> [1] 60.96
```
