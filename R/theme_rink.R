#' Rink plot theme
#'
#' A [ggplot2::theme_void()]-based theme for rink plots: no axes, gridlines or
#' backgrounds, so only the rink and its layers show. Titles are centred, facet
#' strips are drawn as padded coloured chips and legends get some breathing
#' room. [annotate_rink()] adds it for you; call it yourself when you use
#' `annotate_rink(theme = FALSE)` or want different settings.
#'
#' @param base_size Base font size, in points.
#' @param base_family Base font family. The default `""` uses the device's
#'   default sans-serif font.
#' @param coord If `TRUE` (default), also return a 1:1
#'   [ggplot2::coord_fixed()] so the rink keeps its true proportions. Set
#'   `FALSE` when the plot already has a coordinate system, e.g. one added by
#'   [annotate_rink()].
#' @param strip_fill Fill colour of the facet strip labels.
#' @param plot_margin Margin around the whole plot, in points.
#'
#' @return A ggplot2 theme or, with `coord = TRUE`, a list of a theme and a
#'   coordinate system, to add to a plot.
#' @examples
#' library(ggplot2)
#'
#' ggplot(shots, aes(x, y)) +
#'   annotate_rink(theme = FALSE) +
#'   geom_point(alpha = 0.2) +
#'   facet_wrap(~period) +
#'   theme_rink(base_size = 10, coord = FALSE, strip_fill = "grey30")
#' @export
theme_rink <- function(base_size = 12, base_family = "", coord = TRUE,
                       strip_fill = "#143F84", plot_margin = 12) {
  th <- ggplot2::theme_void(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      # theme_void() zeroes plot.margin; restore some so nothing hugs the edge.
      plot.margin = ggplot2::margin(plot_margin, plot_margin, plot_margin, plot_margin),
      plot.title = ggplot2::element_text(
        size = base_size * 5 / 3, face = "bold", hjust = 0.5,
        margin = ggplot2::margin(b = base_size / 3)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size, hjust = 0.5, margin = ggplot2::margin(b = base_size)
      ),
      plot.caption = ggplot2::element_text(
        size = base_size * 0.75, hjust = 1, colour = "grey40",
        margin = ggplot2::margin(t = base_size / 2)
      ),
      strip.text = ggplot2::element_text(
        size = base_size, face = "bold", colour = "white",
        margin = ggplot2::margin(5, 6, 5, 6)
      ),
      strip.background = ggplot2::element_rect(fill = strip_fill, colour = NA),
      strip.clip = "off",
      panel.spacing = grid::unit(1, "lines"),
      legend.title = ggplot2::element_text(face = "bold"),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.box.spacing = grid::unit(12, "pt"),
      legend.margin = ggplot2::margin(6, 8, 6, 8)
    )

  if (isTRUE(coord)) list(ggplot2::coord_fixed(ratio = 1), th) else th
}
