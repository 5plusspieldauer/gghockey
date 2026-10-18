# Build or modify a set of rink dimensions

Starts from an existing set of [rink
dimensions](https://5plusspieldauer.github.io/gghockey/reference/rink_dims.md)
and overrides individual fields, checking that every override is a known
field and a single finite number. Typical uses are matching your data's
coordinate frame (e.g. `origin_x = -30, origin_y = -15` for
centre-ice-origin IIHF data) or drawing a non-standard rink.

## Usage

``` r
rink_dimensions(base = IIHF, ...)
```

## Arguments

- base:

  The dimensions to start from, e.g.
  [IIHF](https://5plusspieldauer.github.io/gghockey/reference/rink_dims.md)
  or
  [NHL](https://5plusspieldauer.github.io/gghockey/reference/rink_dims.md).

- ...:

  Named fields to override, e.g. `width = 26`.

## Value

A named list of rink dimensions, usable wherever gghockey takes
`dimensions`.

## See also

[rink_dims](https://5plusspieldauer.github.io/gghockey/reference/rink_dims.md)

## Examples

``` r
# IIHF rink with the origin at centre ice
centred <- rink_dimensions(IIHF, origin_x = -30, origin_y = -15)

# A narrower 60 x 26 m rink
narrow <- rink_dimensions(IIHF, width = 26)
```
