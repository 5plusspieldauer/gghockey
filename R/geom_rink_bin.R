StatRinkBin <- ggplot2::ggproto("StatRinkBin", ggplot2::Stat,
  required_aes = c("x", "y"),
  optional_aes = "z",
  dropped_aes = "z",
  default_aes = ggplot2::aes(fill = ggplot2::after_stat(value)),
  compute_group = function(data, scales, binwidth = 2.5, xlim = NULL, ylim = NULL) {
    # A vertical rink applies scale_y_reverse(), so by now positions are in
    # scale-transformed space. Bin in the original rink coords (inverse-transform
    # in, transform the cell centres back out) so binning, and the xlim / ylim
    # bounds, are the same in either orientation.
    trx <- scale_transformation(scales$x)
    try <- scale_transformation(scales$y)
    cells <- bin_cells(trx$inverse(data$x), try$inverse(data$y), binwidth, xlim, ylim)
    z <- if ("z" %in% names(data)) data$z else rep(NA_real_, nrow(data))
    value <- as.numeric(tapply(z, cells$id, function(v) mean(v, na.rm = TRUE)))
    value[is.nan(value)] <- NA_real_
    data.frame(
      x = trx$transform(cells$xc),
      y = try$transform(cells$yc),
      value = value,
      count = as.numeric(tabulate(cells$id, nbins = length(cells$xc)))
    )
  }
)

# The transformation of a position scale, or identity if there is none.
scale_transformation <- function(sc) {
  identity <- list(transform = function(v) v, inverse = function(v) v)
  if (is.null(sc)) return(identity)
  tr <- if (!is.null(sc$get_transformation)) sc$get_transformation() else sc$trans
  if (is.null(tr)) identity else tr
}

# Assign points to square cells of side `binwidth`, anchored at 0. Values are
# clamped into [lim[1], lim[2]) first, so points on or behind the boards fall
# into the last in-rink cell instead of seeding one just outside.
# Returns the cell centres (one per occupied cell) and each point's cell id.
bin_cells <- function(x, y, binwidth, xlim = NULL, ylim = NULL) {
  clamp <- function(v, lim) {
    if (is.null(lim)) return(v)
    pmin(pmax(v, min(lim)), max(lim) - 1e-9)
  }
  xc <- floor(clamp(x, xlim) / binwidth) * binwidth + binwidth / 2
  yc <- floor(clamp(y, ylim) / binwidth) * binwidth + binwidth / 2
  key <- paste(xc, yc)
  first <- !duplicated(key)
  id <- match(key, key[first])
  list(xc = xc[first], yc = yc[first], id = id)
}

# Legend key: a square whose side follows the mapped `size` (in rink units) as
# a fraction of the key cell, so the legend shows squares growing with the
# count.
draw_key_rink_square <- function(data, params, size) {
  bw <- params$binwidth
  if (is.null(bw) || !is.finite(bw)) bw <- 1
  s <- if (!is.null(data$size) && is.finite(data$size)) data$size else bw
  frac <- max(min(s / bw, 1), 0.05)
  fill <- if (!is.null(data$fill)) data$fill else "grey50"
  alpha <- if (!is.null(data$alpha) && !is.na(data$alpha)) data$alpha else 1
  col <- if (!is.null(data$colour)) data$colour else NA
  lwd <- if (!is.null(data$linewidth)) data$linewidth else 0
  grid::rectGrob(
    width = grid::unit(frac, "snpc"),
    height = grid::unit(frac, "snpc"),
    gp = grid::gpar(col = col, fill = scales::alpha(fill, alpha),
                    lwd = lwd * ggplot2::.pt, lty = 1)
  )
}

# One square per cell, sized in rink units rather than page mm: `size` is the
# square's side in rink coords, capped at `binwidth` so squares never overlap
# and the map looks the same at any export size. Drawing is delegated to
# GeomRect, which handles coord_flip() and coord_fixed().
GeomRinkSquare <- ggplot2::ggproto("GeomRinkSquare", ggplot2::GeomRect,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    fill = "grey50", colour = NA, linewidth = 0, linetype = 1,
    alpha = NA, size = 1
  ),
  draw_key = draw_key_rink_square,
  # binwidth must survive the layer's parameter check on the precomputed
  # (identity stat) path too, where it isn't a stat parameter.
  extra_params = c("na.rm", "binwidth"),
  # The rect bounds depend on the scaled `size`, only known at draw time, so
  # GeomRect's xmin/xmax requirement is met in draw_panel() instead.
  setup_data = function(data, params) data,
  draw_panel = function(self, data, panel_params, coord, binwidth = 2.5, ...) {
    s <- if (is.null(data$size)) binwidth else pmin(data$size, binwidth)
    data$xmin <- data$x - s / 2
    data$xmax <- data$x + s / 2
    data$ymin <- data$y - s / 2
    data$ymax <- data$y + s / 2
    ggplot2::GeomRect$draw_panel(data, panel_params, coord)
  }
)

# Merge a layer's default mapping with the user's, keeping ggplot2's mapping
# class (the user's entries win).
merge_aes <- function(default, user) {
  if (is.null(user)) return(default)
  out <- utils::modifyList(default, user)
  class(out) <- class(default)
  out
}

#' Binned heatmap of events on a rink
#'
#' Bins events into a square grid and draws one square per cell, coloured by
#' the mean of the `z` aesthetic (e.g. expected goals, or any per-event value).
#' Squares are drawn in **rink units** and never exceed `binwidth`, so they
#' tile the grid without overlapping and look identical at any export size.
#' Add it to a rink from [annotate_rink()]; facet it however you like.
#'
#' Three ready-made styles cover the common maps; pick one with `style`:
#' \describe{
#'   \item{`"size"`}{Square **size** encodes the event count (area
#'     proportional to frequency), **fill** the mean value.}
#'   \item{`"fill"`}{Constant-size squares that fill the cell (leave a `gap`
#'     for thin gridlines); **fill** encodes the mean value.}
#'   \item{`"alpha"`}{Constant-size squares whose **alpha** encodes the event
#'     count and **fill** the mean value.}
#' }
#' Without a `z` aesthetic there is no value to average; map `fill` to
#' `after_stat(count)` for a pure frequency map (see examples).
#'
#' @param mapping Aesthetic mapping, usually `aes(x, y, z)` (often set on the
#'   `ggplot()` call), where `z` is averaged within each cell. `fill`, `size`
#'   and `alpha` are supplied by `style`; anything you map here overrides
#'   them. With `precomputed = TRUE`, the data needs `value` and `n` columns
#'   (as returned by [rink_bin()]).
#' @param data,position,na.rm,show.legend,inherit.aes Standard ggplot2 layer
#'   arguments.
#' @param ... Other arguments passed to [ggplot2::layer()].
#' @param style One of `"size"`, `"fill"` or `"alpha"` (see Details).
#' @param binwidth Cell size, in rink units. Also the largest square side.
#' @param gap For `"fill"` and `"alpha"`, the gap between squares in rink units
#'   (the square side is `binwidth - gap`).
#' @param alpha Constant square alpha for `"size"` and `"fill"` (default `0.9`).
#'   Ignored for `"alpha"`, where alpha is mapped.
#' @param dimensions Rink dimensions (e.g. [IIHF], [NHL]) that set the binning
#'   bounds, so events on or behind the boards snap into the last in-rink cell.
#'   Pass the rink you are drawing, or `NULL` to bin without bounds. Ignored
#'   for any of `xlim` / `ylim` given, and when `precomputed = TRUE`.
#' @param xlim,ylim Optional binning bounds in rink coordinates.
#' @param precomputed If `TRUE`, skip binning and draw the cells as supplied,
#'   e.g. values you derived yourself from [rink_bin()].
#' @param scales If `TRUE` (default), also return the matching scales for
#'   `style`: [scale_fill_rink()] (or [scale_fill_rink_diverging()] with
#'   `diverging = TRUE`) plus [scale_size_rink()] for `"size"` or
#'   [scale_alpha_rink()] for `"alpha"`. Set `FALSE` to add your own.
#' @param diverging Use the diverging fill scale, for maps of differences
#'   centred on 0 (e.g. a player minus league average).
#' @param colour,linewidth Square border colour and width. The default draws
#'   no border; use e.g. `colour = "white", linewidth = 0.3` for gridlines.
#'
#' @return A ggplot2 layer, or a list of a layer and scales.
#' @seealso [rink_bin()], [scale_fill_rink()], [annotate_rink()]
#' @examples
#' library(ggplot2)
#'
#' base <- ggplot(shots, aes(x, y, z = xg)) +
#'   annotate_rink(orientation = "vertical", zones = "oz",
#'                 colour = "grey60", linewidth = 0.5)
#'
#' base + geom_rink_bin(style = "size", binwidth = 2)
#' base + geom_rink_bin(style = "fill", binwidth = 2, gap = 0.1)
#' base + geom_rink_bin(style = "alpha", binwidth = 2)
#'
#' # Frequency only: fill by count, your own scale
#' base +
#'   geom_rink_bin(aes(fill = after_stat(count)), style = "fill",
#'                 binwidth = 2, scales = FALSE) +
#'   scale_fill_rink(name = "Shots", labels = NULL)
#' @export
geom_rink_bin <- function(mapping = NULL, data = NULL, position = "identity", ...,
                          style = c("size", "fill", "alpha"),
                          binwidth = 2.5, gap = 0, alpha = NULL,
                          dimensions = IIHF, xlim = NULL, ylim = NULL,
                          precomputed = FALSE, scales = TRUE, diverging = FALSE,
                          colour = NA, linewidth = 0,
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  style <- rlang::arg_match(style)
  stopifnot(is_number(binwidth), binwidth > 0, is_number(gap), gap >= 0, gap < binwidth)

  # Under the binning stat the value and count come from the stat; when
  # precomputed they are plain columns (value, n).
  if (precomputed) {
    fill_map  <- ggplot2::aes(fill = .data$value)
    size_map  <- ggplot2::aes(size = .data$n)
    alpha_map <- ggplot2::aes(alpha = .data$n)
  } else {
    fill_map  <- ggplot2::aes(fill = ggplot2::after_stat(value))
    size_map  <- ggplot2::aes(size = ggplot2::after_stat(count))
    alpha_map <- ggplot2::aes(alpha = ggplot2::after_stat(count))
  }
  default_map <- switch(style,
    size  = merge_aes(fill_map, size_map),
    fill  = fill_map,
    alpha = merge_aes(fill_map, alpha_map)
  )
  mapping <- merge_aes(default_map, mapping)

  params <- list(na.rm = na.rm, colour = colour, linewidth = linewidth,
                 binwidth = binwidth, ...)
  if (!precomputed) {
    if (!is.null(dimensions)) {
      check_dimensions(dimensions)
      if (is.null(xlim)) xlim <- dimensions$origin_x + c(0, dimensions$length)
      if (is.null(ylim)) ylim <- dimensions$origin_y + c(0, dimensions$width)
    }
    params$xlim <- xlim
    params$ylim <- ylim
  }
  # Constant-size styles set the side directly, bypassing any size scale.
  if (style %in% c("fill", "alpha")) params$size <- binwidth - gap
  if (style %in% c("size", "fill")) {
    params$alpha <- if (is.null(alpha)) 0.9 else alpha
  } else if (!is.null(alpha)) {
    params$alpha <- alpha
  }

  lyr <- ggplot2::layer(
    stat = if (precomputed) "identity" else StatRinkBin, geom = GeomRinkSquare,
    data = data, mapping = mapping, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes, params = params
  )
  if (!isTRUE(scales)) return(lyr)

  scale_list <- list(if (diverging) scale_fill_rink_diverging() else scale_fill_rink())
  if (style == "size")  scale_list <- c(scale_list, list(scale_size_rink(binwidth = binwidth)))
  if (style == "alpha") scale_list <- c(scale_list, list(scale_alpha_rink()))
  c(list(lyr), scale_list)
}

#' Bin events into a rink grid
#'
#' Aggregates events into the square grid that [geom_rink_bin()] draws: one row
#' per occupied cell (per group) with the event count and the mean value. Use
#' it to derive your own cell values (differences between teams, shrinkage,
#' rates per 60, ...) and plot them with `geom_rink_bin(precomputed = TRUE)`.
#'
#' @param data A data frame of events.
#' @param x,y Coordinate columns (unquoted).
#' @param value Column to average within each cell (unquoted). Omit for counts
#'   only (`value` is then `NA`).
#' @param binwidth Cell size, in rink units.
#' @param by Columns to group by in addition to the cell, e.g. `team` or
#'   `c(team, period)` (unquoted, tidyselect).
#' @param dimensions,xlim,ylim Binning bounds, as in [geom_rink_bin()].
#'
#' @return A tibble with the `by` columns, `x` and `y` (cell centres), `n` (event
#'   count) and `value` (mean of `value`, ignoring `NA`s).
#' @seealso [geom_rink_bin()]
#' @examples
#' cells <- rink_bin(shots, x, y, value = xg, binwidth = 2, by = team)
#' head(cells)
#'
#' # Difference between the two teams, cell by cell
#' library(ggplot2)
#' wide <- merge(cells[cells$team == "Lakeside", ], cells[cells$team == "Northfield", ],
#'               by = c("x", "y"), suffixes = c("_a", "_b"))
#' wide$value <- wide$value_a - wide$value_b
#' wide$n <- wide$n_a + wide$n_b
#'
#' ggplot(wide, aes(x, y)) +
#'   annotate_rink(zones = "oz", orientation = "vertical") +
#'   geom_rink_bin(precomputed = TRUE, style = "fill", diverging = TRUE, binwidth = 2)
#' @export
rink_bin <- function(data, x, y, value = NULL, binwidth = 2.5, by = NULL,
                     dimensions = IIHF, xlim = NULL, ylim = NULL) {
  stopifnot(is.data.frame(data), is_number(binwidth), binwidth > 0)
  if (!is.null(dimensions)) {
    check_dimensions(dimensions)
    if (is.null(xlim)) xlim <- dimensions$origin_x + c(0, dimensions$length)
    if (is.null(ylim)) ylim <- dimensions$origin_y + c(0, dimensions$width)
  }
  value_q <- rlang::enquo(value)
  by_q <- rlang::enquo(by)

  xv <- rlang::eval_tidy(rlang::enquo(x), data)
  yv <- rlang::eval_tidy(rlang::enquo(y), data)
  if (!is.numeric(xv) || !is.numeric(yv)) rlang::abort("`x` and `y` must be numeric columns.")
  cells <- bin_cells(xv, yv, binwidth, xlim, ylim)

  data$.x <- cells$xc[cells$id]
  data$.y <- cells$yc[cells$id]
  data$.value <- if (rlang::quo_is_null(value_q)) NA_real_ else rlang::eval_tidy(value_q, data)

  grouped <- if (rlang::quo_is_null(by_q)) {
    dplyr::group_by(data, .data$.x, .data$.y)
  } else {
    dplyr::group_by(data, dplyr::across(!!by_q), .data$.x, .data$.y)
  }
  out <- dplyr::summarise(
    grouped,
    n = dplyr::n(),
    value = if (all(is.na(.data$.value))) NA_real_ else mean(.data$.value, na.rm = TRUE),
    .groups = "drop"
  )
  dplyr::rename(out, x = ".x", y = ".y")
}
