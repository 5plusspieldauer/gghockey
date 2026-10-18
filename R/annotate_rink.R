# =============================================================================
# annotate_rink.R  --  the rink as ggplot2 layers + the canvas (coord, limits, theme).
#
# Geometry comes from rink_geometry(); this file only styles it, crops it and picks the
# coordinate system. Data is ALWAYS mapped in horizontal rink coords -- a vertical rink
# is a display rotation (scale_y_reverse + coord_flip), so the same data and the same
# geom_rink_bin() binning work in both orientations.
# =============================================================================

#' Add a hockey rink to a ggplot
#'
#' Draws the rink markings and sets up the plot canvas in one call: the rink layers, a
#' coordinate system for the chosen orientation and, unless `theme = FALSE`,
#' [theme_rink()]. Add it right after `ggplot()`, before your geoms, and map your data
#' in regular horizontal rink coordinates (`x` along the length, `y` across the width,
#' in the frame of `dimensions`). For `orientation = "vertical"` the display is rotated
#' for you, so the same data works in either orientation.
#' @param colour Colour of the rink markings.
#' @param fill Colour of the ice.
#' @param dimensions Rink dimensions to draw, e.g. [IIHF] (default) or [NHL]; see
#'   [rink_dims] and [rink_dimensions()].
#' @param orientation `"horizontal"` (default) or `"vertical"` (the right-hand end, the
#'   offensive zone, at the top).
#' @param zones Which zones to show, a subset of `c("dz", "nz", "oz")` (default all
#'   three); `"oz"` is the high-`x` end. Sets `xlim` for you, e.g. `c("dz", "nz")` crops
#'   to the defensive and neutral zones. Ignored if `xlim` is given.
#' @param xlim,ylim Optional plot limits in rink coordinates, to crop by hand. `xlim`
#'   overrides `zones`; `ylim` defaults to the full width plus `margin`.
#' @param margin Space (in rink units) added beyond the end and side boards.
#' @param zone_margin Space added beyond a blue line when `zones` stops there, so the
#'   crop runs a little past the line instead of ending on it.
#' @param linewidth Multiplier for the rink line widths (`lines_small` / `lines_big` in
#'   `dimensions`). ggplot2 line widths are absolute (mm on the page), so they don't
#'   shrink with the plot: the default `0.3` suits a rink a few inches across; go lower
#'   (e.g. `0.2`) for small or heavily faceted rinks and higher (e.g. `0.6`-`1`) for a
#'   large poster-size rink.
#' @param details If `TRUE`, draw the faceoff hash marks and the dot cut-outs.
#' @param slot If `TRUE`, shade the offensive-zone slot.
#' @param slot_colour,slot_fill,slot_alpha,slot_linewidth Slot styling.
#' @param logos Optional logos to place on the ice, as a named list with any of
#'   `centre`, `left` and `right` (centre ice, and just inside each blue line). Each
#'   entry is a path to a PNG file or an image already in memory (an array as returned
#'   by [png::readPNG()], a matrix of colours or a `raster`). Reading PNG files needs the
#'   \pkg{png} package. No logos ship with gghockey: bring the ones you have the rights
#'   to use.
#' @param logo_alpha Opacity of the logos, between 0 and 1.
#' @param theme If `TRUE` (default), add [theme_rink()] (and, for vertical rinks, the
#'   matching aspect ratio). Set `FALSE` to supply your own theme.
#' @return A list of ggplot2 components (rink layers, a coordinate system and,
#'   optionally, a theme) to add to a plot.
#' @seealso [rink_geometry()] for the markings as data, [geom_rink_bin()] for heatmaps
#'   on the rink.
#' @keywords rink, plot
#' @examples
#' library(ggplot2)
#'
#' ggplot(shots, aes(x, y)) +
#'   annotate_rink() +
#'   geom_point(alpha = 0.3)
#'
#' # Vertical, offensive zone only, with hash marks:
#' ggplot(shots, aes(x, y)) +
#'   annotate_rink(orientation = "vertical", zones = "oz", details = TRUE) +
#'   geom_point(aes(colour = factor(goal)), alpha = 0.5)
#'
#' # NHL rink, in the colours of your choice:
#' ggplot() +
#'   annotate_rink(dimensions = NHL, colour = "grey30", fill = "white")
#' @export
annotate_rink <- function(colour = "#143F84",
                          fill = "#eaf2f8",
                          dimensions = IIHF,
                          orientation = c("horizontal", "vertical"),
                          zones = c("dz", "nz", "oz"),
                          xlim = NULL,
                          ylim = NULL,
                          margin = 0.5,
                          zone_margin = 2,
                          linewidth = 0.3,
                          details = FALSE,
                          slot = FALSE,
                          slot_colour = "grey",
                          slot_fill = NA,
                          slot_alpha = 0.7,
                          slot_linewidth = 0.4,
                          logos = NULL,
                          logo_alpha = 0.25,
                          theme = TRUE) {
  .check_dimensions(dimensions)
  orientation <- match.arg(orientation)
  .check_number(linewidth, "linewidth", linewidth > 0, "a positive number")
  .check_number(logo_alpha, "logo_alpha", logo_alpha >= 0 && logo_alpha <= 1,
                "a number between 0 and 1")

  # Resolve the geometry's style ROLES into actual colours / widths.
  geo <- rink_geometry(dimensions, details = details, slot = slot)
  style <- list(
    colour = c(line = colour, ice = fill, slot = slot_colour),
    fill = c(line = colour, ice = fill, slot = slot_fill),
    linewidth = c(small = dimensions$lines_small * linewidth,
                  big = dimensions$lines_big * linewidth,
                  slot = slot_linewidth),
    slot_alpha = slot_alpha
  )
  rink_layers <- .geometry_layers(geo, style)

  # Logos go on top of the ice (layer 1) but UNDER the markings. (aRno drew them under
  # the ice, which hid all but the centre logo.)
  logo_layers <- .logo_annotations(logos, dimensions, orientation, logo_alpha)
  layers <- c(rink_layers[1], logo_layers, rink_layers[-1])

  lims <- .rink_limits(dimensions, zones, xlim, ylim, margin, zone_margin)

  if (orientation == "vertical") {
    # Reverse the width axis, then swap x and y, so horizontal coords render rotated
    # 90 degrees with the high-x end at the top. Reversing ylim alone does NOT work:
    # coord_flip() re-sorts the limits.
    coord_layers <- list(
      ggplot2::scale_y_reverse(),
      ggplot2::coord_flip(xlim = lims$x, ylim = lims$y, expand = FALSE)
    )
    asp <- diff(lims$x) / diff(lims$y)   # coord_flip() can't fix the ratio; set it on the panel
  } else {
    coord_layers <- list(
      ggplot2::coord_fixed(ratio = 1, xlim = lims$x, ylim = lims$y, expand = FALSE)
    )
    asp <- NULL
  }

  # Even with theme = FALSE a vertical rink needs its aspect ratio, or it gets squashed.
  theme_layers <- list()
  if (isTRUE(theme)) {
    th <- theme_rink(coord = FALSE, strip_fill = colour)
    if (!is.null(asp)) th <- th + ggplot2::theme(aspect.ratio = asp)
    theme_layers <- list(th)
  } else if (!is.null(asp)) {
    theme_layers <- list(ggplot2::theme(aspect.ratio = asp))
  }

  c(layers, coord_layers, theme_layers)
}

# Turn the geometry table into layers. CONSECUTIVE shapes with the same style share one
# layer: that keeps the drawing order (and so the look) while cutting the old ~90
# annotate() calls down to a handful of layers. Only consecutive -- merging across the
# order would let e.g. a filled crease paint over a line drawn before it.
.geometry_layers <- function(geo, style) {
  key <- paste(geo$kind, geo$colour, geo$fill, geo$linewidth, geo$alpha, geo$linejoin)
  shape_key <- key[!duplicated(geo$group)]
  runs <- cumsum(c(TRUE, shape_key[-1] != shape_key[-length(shape_key)]))
  run_of_group <- runs[match(geo$group, unique(geo$group))]

  lapply(split(geo, run_of_group), function(g) {
    s <- g[1, ]
    params <- list(
      colour = if (is.na(s$colour)) NA else unname(style$colour[s$colour]),
      linejoin = s$linejoin
    )
    # NA width = leave ggplot2's default (the filled dots), as the old annotate() calls did.
    if (!is.na(s$linewidth)) params$linewidth <- unname(style$linewidth[s$linewidth])
    if (s$kind == "polygon") {
      params$fill <- if (is.na(s$fill)) NA else unname(style$fill[s$fill])
      params$alpha <- if (is.na(s$alpha)) style$slot_alpha else s$alpha
    }
    ggplot2::layer(
      geom = if (s$kind == "polygon") ggplot2::GeomPolygon else ggplot2::GeomPath,
      stat = "identity", position = "identity",
      data = g[c("x", "y", "group")],
      mapping = ggplot2::aes(x = .data$x, y = .data$y, group = .data$group),
      # inherit.aes = FALSE: the rink must not pick up the user's aes(x, y, z, colour ...).
      params = params, inherit.aes = FALSE, show.legend = FALSE
    )
  })
}

# Plot limits in rink coords. Without xlim, the x extent comes from the selected zones:
# `margin` beyond a true rink end, the larger `zone_margin` beyond a blue-line cut.
.rink_limits <- function(d, zones, xlim, ylim, margin, zone_margin) {
  if (is.null(xlim)) {
    zones <- match.arg(zones, c("dz", "nz", "oz"), several.ok = TRUE)
    left_end   <- d$origin_x
    left_blue  <- d$origin_x + d$blue_line
    right_blue <- d$origin_x + d$length - d$blue_line
    right_end  <- d$origin_x + d$length
    zb <- list(dz = c(left_end, left_blue),
               nz = c(left_blue, right_blue),
               oz = c(right_blue, right_end))[zones]
    lo <- min(vapply(zb, `[`, numeric(1), 1))
    hi <- max(vapply(zb, `[`, numeric(1), 2))
    xlim <- c(lo - if (isTRUE(all.equal(lo, left_end))) margin else zone_margin,
              hi + if (isTRUE(all.equal(hi, right_end))) margin else zone_margin)
  }
  if (is.null(ylim)) ylim <- c(d$origin_y - margin, d$origin_y + d$width + margin)
  for (lim in list(list(xlim, "xlim"), list(ylim, "ylim"))) {
    if (!is.numeric(lim[[1]]) || length(lim[[1]]) != 2 || anyNA(lim[[1]]))
      stop(sprintf("`%s` must be two numbers, e.g. c(37.5, 60.5)", lim[[2]]), call. = FALSE)
  }
  list(x = sort(xlim), y = sort(ylim))
}
