#' Rink markings as a data frame
#'
#' Returns every rink marking as tidy coordinates: one row per vertex, grouped
#' by `shape`. This is what [annotate_rink()] draws, exposed so you can reuse
#' the geometry elsewhere (another plotting library, an interactive map, a
#' point-in-zone test, ...).
#'
#' Markings are listed in drawing order (bottom to top). Shapes on the right
#' half of the rink are mirror images of the left half, built from the same
#' code.
#'
#' @param dimensions Rink dimensions, e.g. [IIHF] or [NHL] (see [rink_dims]).
#' @param details If `TRUE`, include the faceoff-circle hash marks and the
#'   cut-outs inside the faceoff dots.
#' @param slot If `TRUE`, include the offensive-zone slot (the high-danger area
#'   in front of the right-hand goal).
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{`shape`}{Marking name, e.g. `"centre_circle"`, `"blue_line_left"`.}
#'     \item{`group`}{Integer id, unique per shape, in drawing order.}
#'     \item{`kind`}{`"polygon"` (closed) or `"path"` (open line).}
#'     \item{`x`, `y`}{Vertex coordinates, in the frame of `dimensions`.}
#'     \item{`colour`, `fill`}{Style roles: `"line"` (the marking colour),
#'       `"ice"` (the rink fill colour), `"slot"` (the slot styling) or `NA`
#'       (none).}
#'     \item{`linewidth`}{`"small"`, `"big"`, `"slot"` or `NA` (the ggplot2
#'       default).}
#'     \item{`alpha`}{Fill alpha (`NA` for the slot, which takes
#'       `slot_alpha` in [annotate_rink()]).}
#'     \item{`linejoin`}{Line-join style for the outline.}
#'   }
#' @seealso [annotate_rink()]
#' @examples
#' geo <- rink_geometry(IIHF)
#' unique(geo$shape)
#'
#' # Draw it yourself
#' library(ggplot2)
#' ggplot(geo[geo$kind == "path", ], aes(x, y, group = group)) +
#'   geom_path() +
#'   coord_fixed()
#' @export
rink_geometry <- function(dimensions = IIHF, details = FALSE, slot = FALSE) {
  check_dimensions(dimensions)
  d <- dimensions
  shapes <- c(
    rink_base_shapes(d),
    if (isTRUE(details)) rink_detail_shapes(d),
    if (isTRUE(slot)) list(rink_slot_shape(d))
  )
  out <- do.call(rbind, Map(function(s, i) {
    data.frame(
      shape = s$shape, group = i, kind = s$kind,
      x = d$origin_x + s$x, y = d$origin_y + s$y,
      colour = s$colour, fill = s$fill, linewidth = s$linewidth,
      alpha = s$alpha, linejoin = s$linejoin,
      stringsAsFactors = FALSE
    )
  }, shapes, seq_along(shapes)))
  rownames(out) <- NULL
  out
}

# Shape constructors -----------------------------------------------------------
# Each returns a list in rink-relative coords (origin at the bottom-left corner
# of the boards); rink_geometry() shifts to the dimensions' origin.

shape <- function(name, kind, x, y, colour = "line", fill = NA, linewidth = "small",
                  alpha = 1, linejoin = "round") {
  list(shape = name, kind = kind, x = x, y = y, colour = colour, fill = fill,
       linewidth = linewidth, alpha = alpha, linejoin = linejoin)
}

circle_xy <- function(cx, cy, r, n = 50) {
  t <- seq(0, 2 * pi, length.out = n)
  list(x = cx + r * cos(t), y = cy + r * sin(t))
}

# The boards: straight sides joined by quarter-ellipse corners.
boards_xy <- function(d) {
  L <- d$length; W <- d$width
  cx <- d$corner_border_start_x; cy <- d$corner_border_start_y
  up <- seq(0, pi / 2, length.out = 20)
  down <- rev(up)
  list(
    x = c(L / 2, cx - cx * sin(up), cx - cx * sin(down), L / 2,
          L / 2, L - cx + cx * sin(up), L - cx + cx * sin(down), L / 2),
    y = c(0, cy - cy * cos(up), W - cy + cy * cos(down), W,
          W, W - cy + cy * cos(up), cy - cy * cos(down), 0)
  )
}

# How far the goal line sits in from the side boards: where x = end_line meets
# the corner arc (x = cx - cx sin t, y = cy - cy cos t).
end_line_inset <- function(d) {
  cx <- d$corner_border_start_x; cy <- d$corner_border_start_y
  if (cx <= d$end_line) return(0)
  s <- (cx - d$end_line) / cx
  cy * (1 - sqrt(1 - s^2))
}

# Goal crease at the left end; mirror for the right.
crease_xy <- function(d) {
  el <- d$end_line; ch <- d$crease_height; rh <- d$crease_ring_height
  W <- d$width; hw <- d$crease_width / 2
  list(
    x = c(el, el + ch, el + ch - rh * sin(seq(pi, 2 * pi, length.out = 20)), el + ch, el),
    y = c(W / 2 - hw, W / 2 - hw, W / 2 - hw * cos(seq(2 * pi, pi, length.out = 20)),
          W / 2 + hw, W / 2 + hw)
  )
}

mirror <- function(xy, d) list(x = d$length - xy$x, y = xy$y)

# The four end-zone faceoff spots, in the drawing order used throughout:
# left-bottom, left-top, right-bottom, right-top.
end_spots <- function(d) {
  L <- d$length; W <- d$width
  list(
    lb = c(d$faceoff_dot_x,     d$faceoff_dot_y),
    lt = c(d$faceoff_dot_x,     W - d$faceoff_dot_y),
    rb = c(L - d$faceoff_dot_x, d$faceoff_dot_y),
    rt = c(L - d$faceoff_dot_x, W - d$faceoff_dot_y)
  )
}

nz_spots <- function(d) {
  L <- d$length; W <- d$width
  xl <- d$blue_line + d$nz_faceoff_dot
  xr <- L - d$blue_line - d$nz_faceoff_dot
  list(lb = c(xl, d$faceoff_dot_y), lt = c(xl, W - d$faceoff_dot_y),
       rb = c(xr, d$faceoff_dot_y), rt = c(xr, W - d$faceoff_dot_y))
}

rink_base_shapes <- function(d) {
  L <- d$length; W <- d$width
  boards <- boards_xy(d)
  inset <- end_line_inset(d)
  gap <- d$center_spot_free_space / 2

  dot <- function(name, p) {
    xy <- circle_xy(p[1], p[2], d$faceoff_dot_size)
    shape(name, "polygon", xy$x, xy$y, fill = "line", linewidth = NA)
  }
  ring <- function(name, p) {
    xy <- circle_xy(p[1], p[2], d$faceoff_circle)
    shape(name, "polygon", xy$x, xy$y)
  }
  goal <- function(name, x0, x1) {
    y0 <- W / 2 - d$goal_width / 2; y1 <- W / 2 + d$goal_width / 2
    shape(name, "polygon", c(x0, x1, x1, x0), c(y0, y0, y1, y1), linejoin = "mitre")
  }
  vline <- function(name, x, y0, y1, lw) shape(name, "path", c(x, x), c(y0, y1), linewidth = lw)

  centre <- circle_xy(L / 2, W / 2, d$faceoff_circle)
  centre_dot <- circle_xy(L / 2, W / 2, d$faceoff_dot_size_center)
  es <- end_spots(d)
  ns <- nz_spots(d)
  crease_l <- crease_xy(d)
  crease_r <- mirror(crease_l, d)

  c(
    list(
      shape("ice", "polygon", boards$x, boards$y, fill = "ice"),
      shape("centre_circle", "polygon", centre$x, centre$y),
      shape("centre_dot", "polygon", centre_dot$x, centre_dot$y, fill = "line", linewidth = NA)
    ),
    unlist(lapply(names(es), function(k) {
      list(dot(paste0("faceoff_dot_", k), es[[k]]),
           ring(paste0("faceoff_circle_", k), es[[k]]))
    }), recursive = FALSE),
    lapply(names(ns), function(k) dot(paste0("nz_dot_", k), ns[[k]])),
    list(
      goal("goal_right", L - d$end_line, L - d$end_line + d$goal_depth),
      goal("goal_left", d$end_line - d$goal_depth, d$end_line),
      vline("centre_line_bottom", L / 2, 0, W / 2 - gap, "big"),
      vline("centre_line_top", L / 2, W / 2 + gap, W, "big"),
      vline("blue_line_left", d$blue_line, 0, W, "big"),
      vline("blue_line_right", L - d$blue_line, 0, W, "big"),
      vline("goal_line_left", d$end_line, inset, W - inset, "small"),
      vline("goal_line_right", L - d$end_line, inset, W - inset, "small"),
      shape("crease_left_fill", "polygon", crease_l$x, crease_l$y, fill = "line", alpha = 0.5),
      shape("crease_left", "polygon", crease_l$x, crease_l$y),
      shape("crease_right_fill", "polygon", crease_r$x, crease_r$y, fill = "line", alpha = 0.5),
      shape("crease_right", "polygon", crease_r$x, crease_r$y),
      shape("boards", "polygon", boards$x, boards$y, linewidth = "big")
    )
  )
}

rink_detail_shapes <- function(d) {
  es <- end_spots(d)
  ns <- nz_spots(d)
  signs <- list(c(1, 1), c(1, -1), c(-1, 1), c(-1, -1))

  hashes <- unlist(lapply(names(es), function(k) {
    cx <- es[[k]][1]; cy <- es[[k]][2]
    # Inner L-shaped marks around the dot, one per quadrant.
    inner <- lapply(signs, function(s) {
      shape(paste0("hash_inner_", k), "path",
            x = cx + s[1] * c(d$inner_hash_x, d$inner_hash_x, d$inner_hash_x + d$inner_hash_length_vert),
            y = cy + s[2] * c(d$inner_hash_y + d$inner_hash_length_hor, d$inner_hash_y, d$inner_hash_y))
    })
    # Outer ticks on the circle, above and below.
    outer <- lapply(list(c(1, 1), c(-1, 1), c(1, -1), c(-1, -1)), function(s) {
      shape(paste0("hash_outer_", k), "path",
            x = rep(cx + s[1] * d$outer_hash_y, 2),
            y = cy + s[2] * c(d$faceoff_circle - d$outer_hash_x,
                              d$faceoff_circle + d$outer_hash_length - d$outer_hash_x))
    })
    c(inner, outer)
  }), recursive = FALSE)

  # Ice-coloured wedges cut into each faceoff dot.
  r <- d$faceoff_dot_white_radius
  cutouts <- unlist(lapply(c(es, ns), function(p) {
    lapply(list(c(220, 140), c(-40, 40)), function(deg) {
      t <- seq(deg[1], deg[2], length.out = 50) / 180 * pi
      shape("dot_cutout", "polygon", p[1] - r * cos(t), p[2] - r * sin(t),
            colour = "ice", fill = "ice", linewidth = NA)
    })
  }), recursive = FALSE)

  c(hashes, cutouts)
}

rink_slot_shape <- function(d) {
  L <- d$length; W <- d$width
  x_dot <- L - d$faceoff_dot_x
  shape("slot", "polygon",
        x = c(L - d$end_line, L - d$end_line, x_dot,
              x_dot - d$faceoff_circle, x_dot - d$faceoff_circle, x_dot),
        y = c(12 / 30 * W, 18 / 30 * W, W - d$faceoff_dot_y,
              W - d$faceoff_dot_y, d$faceoff_dot_y, d$faceoff_dot_y),
        colour = "slot", fill = "slot", linewidth = "slot", alpha = NA)
}
