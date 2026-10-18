#' Scales for rink heatmaps
#'
#' Matched scales for the [geom_rink_bin()] styles:
#' \describe{
#'   \item{`scale_fill_rink()`}{Sequential fill for the cell mean, labelled
#'     Low / High at the ends by default. Used by every style.}
#'   \item{`scale_fill_rink_diverging()`}{Diverging fill centred on `0`, for
#'     maps of differences. Limits are symmetric around `0` (from the data
#'     unless you pass `limits`), so `0` sits at the neutral midpoint.}
#'   \item{`scale_size_rink()`}{Event-count **size** scale for
#'     `style = "size"`. Square sides are set in rink units as a fraction of
#'     `binwidth`, so the largest square fills a cell and the smallest stays
#'     legible.}
#'   \item{`scale_alpha_rink()`}{Event-count **alpha** scale for
#'     `style = "alpha"`.}
#' }
#'
#' Fill limits are yours to choose: the default `NULL` uses the range of the
#' data, which changes from plot to plot. Fix `limits` (values outside are
#' squished to the ends) when several maps should share one colour key.
#'
#' @param name Legend title.
#' @param palette A [grDevices::hcl.colors()] palette name (see
#'   [grDevices::hcl.pals()]), e.g. `"Viridis"`, `"Mako"` or `"Rocket"` for the
#'   sequential scale and `"Blue-Red 3"`, `"Vik"` or `"Purple-Green"` for the
#'   diverging one; or a vector of colours.
#' @param limits Fill limits. For `scale_fill_rink_diverging()`, `NULL`
#'   derives symmetric limits `c(-a, a)` from the data.
#' @param breaks,labels Passed to the underlying scale. The defaults label only
#'   the ends of the bar (and `0` for the diverging scale); use e.g.
#'   `breaks = waiver(), labels = waiver()` for numeric labels.
#' @param binwidth The `binwidth` given to [geom_rink_bin()]. Square sides
#'   range over `frac * binwidth`.
#' @param frac Smallest and largest square side, as a fraction of `binwidth`.
#'   The largest is capped at `1` (one full cell) so squares never overlap.
#' @param range Smallest and largest alpha.
#' @param square_colour Fill colour of the legend squares.
#' @param ... Passed to the underlying ggplot2 scale.
#'
#' @return A ggplot2 scale.
#' @seealso [geom_rink_bin()]
#' @examples
#' library(ggplot2)
#'
#' ggplot(shots, aes(x, y, z = xg)) +
#'   annotate_rink(zones = "oz") +
#'   geom_rink_bin(binwidth = 2, scales = FALSE) +
#'   scale_fill_rink(name = "Mean xG", palette = "Rocket", limits = c(0, 0.25),
#'                   breaks = waiver(), labels = waiver()) +
#'   scale_size_rink(name = "Shots", binwidth = 2)
#' @name scale_rink
NULL

#' @rdname scale_rink
#' @export
scale_fill_rink <- function(name = "Value", palette = "Viridis", limits = NULL,
                            breaks = function(lims) lims, labels = c("Low", "High"),
                            ...) {
  ggplot2::scale_fill_gradientn(
    colours = palette_colours(palette),
    name = name, limits = limits, oob = scales::squish,
    breaks = breaks, labels = labels,
    guide = ggplot2::guide_colourbar(direction = "horizontal", theme = bar_legend_theme()),
    ...
  )
}

#' @rdname scale_rink
#' @export
scale_fill_rink_diverging <- function(name = "Difference", palette = "Blue-Red 3",
                                      limits = NULL,
                                      breaks = function(lims) c(lims[1], 0, lims[2]),
                                      labels = c("Below", "0", "Above"), ...) {
  if (is.null(limits)) limits <- function(r) c(-1, 1) * max(abs(r), na.rm = TRUE)
  ggplot2::scale_fill_gradientn(
    colours = palette_colours(palette),
    name = name, limits = limits, oob = scales::squish,
    breaks = breaks, labels = labels,
    guide = ggplot2::guide_colourbar(direction = "horizontal", theme = bar_legend_theme()),
    ...
  )
}

#' @rdname scale_rink
#' @export
scale_size_rink <- function(name = "Count", binwidth = 2.5, frac = c(0.4, 1),
                            square_colour = "#143F84", ...) {
  stopifnot(is.numeric(frac), length(frac) == 2, all(frac > 0), all(frac <= 1))
  ggplot2::scale_size(
    name = name, range = frac * binwidth,
    guide = ggplot2::guide_legend(
      nrow = 1, override.aes = list(fill = square_colour, colour = NA),
      theme = key_legend_theme()
    ),
    ...
  )
}

#' @rdname scale_rink
#' @export
scale_alpha_rink <- function(name = "Count", range = c(0.15, 1),
                             square_colour = "#143F84", ...) {
  ggplot2::scale_alpha_continuous(
    name = name, range = range,
    guide = ggplot2::guide_legend(
      nrow = 1, override.aes = list(fill = square_colour, colour = NA),
      theme = key_legend_theme()
    ),
    ...
  )
}

palette_colours <- function(palette) {
  if (length(palette) > 1) return(palette)
  if (!palette %in% grDevices::hcl.pals()) {
    rlang::abort(c(
      paste0("Unknown palette \"", palette, "\"."),
      i = "Use a name from `grDevices::hcl.pals()` or a vector of colours."
    ))
  }
  grDevices::hcl.colors(256, palette)
}

# Legend styling shared by the scales: bold centred title above, labels below.
bar_legend_theme <- function(width = 4.5, height = 0.5) {
  ggplot2::theme(
    legend.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = ggplot2::rel(0.95)),
    legend.title.position = "top",
    legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
    legend.text.position = "bottom",
    legend.key.width = grid::unit(width, "cm"),
    legend.key.height = grid::unit(height, "cm"),
    legend.ticks = ggplot2::element_blank()
  )
}

key_legend_theme <- function(key = 0.85) {
  ggplot2::theme(
    legend.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = ggplot2::rel(0.95)),
    legend.title.position = "top",
    legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
    legend.text.position = "bottom",
    legend.key.size = grid::unit(key, "cm"),
    legend.key.spacing.x = grid::unit(0.35, "cm")
  )
}
