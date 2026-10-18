# Logo placement for annotate_rink(). Boxes are in rink-relative coords and
# scale with the rink; the image keeps its own aspect ratio inside the box.
logo_annotations <- function(logos, d, orientation, alpha) {
  if (is.null(logos) || length(logos) == 0) return(list())
  if (!is.list(logos) || is.null(names(logos)) || any(names(logos) == "")) {
    rlang::abort("`logos` must be a named list, e.g. `list(centre = \"logo.png\")`.")
  }
  names(logos)[names(logos) == "center"] <- "centre"
  unknown <- setdiff(names(logos), c("centre", "left", "right"))
  if (length(unknown) > 0) {
    rlang::abort(c(
      "Unknown logo position(s).",
      x = paste0("`", unknown, "`", collapse = ", "),
      i = "Use `centre`, `left` and/or `right`."
    ))
  }

  # Box centres and display sizes (along the display's width x height), so a
  # wide logo stays wide when the rink is drawn vertically.
  L <- d$length; W <- d$width
  side_x <- d$blue_line - 4 / 60 * L
  spots <- list(
    centre = list(x = L / 2,      size = c(10 / 60 * L, 6 / 30 * W)),
    left   = list(x = side_x,     size = c(5 / 60 * L, 5 / 30 * W)),
    right  = list(x = L - side_x, size = c(5 / 60 * L, 5 / 30 * W))
  )

  lapply(names(logos), function(pos) {
    img <- logo_raster(logos[[pos]], alpha, pos)
    sp <- spots[[pos]]
    # Horizontal: size[1] runs along x. Vertical: the display is rotated, so
    # the rink's x is the display's height.
    half <- if (orientation == "vertical") rev(sp$size) / 2 else sp$size / 2
    xmin <- d$origin_x + sp$x - half[1]; xmax <- d$origin_x + sp$x + half[1]
    ymin <- d$origin_y + W / 2 - half[2]; ymax <- d$origin_y + W / 2 + half[2]
    if (orientation == "vertical") {
      # annotation_custom() positions skip scale transforms, so apply the
      # vertical rink's scale_y_reverse() by hand.
      y <- c(-ymax, -ymin); ymin <- y[1]; ymax <- y[2]
    }
    ggplot2::annotation_custom(
      grid::rasterGrob(img, interpolate = TRUE),
      xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
    )
  })
}

# Read / coerce a logo into a raster with its alpha scaled by `alpha`.
logo_raster <- function(x, alpha, pos) {
  if (is.character(x)) {
    if (length(x) != 1 || !file.exists(x)) {
      rlang::abort(paste0("Logo file for `", pos, "` not found: ", x[1]))
    }
    rlang::check_installed("png", reason = "to read PNG logos.")
    x <- png::readPNG(x)
  }
  if (inherits(x, "nativeRaster")) {
    rlang::abort(paste0("Logo `", pos, "`: read PNGs with `native = FALSE` (the default)."))
  }

  if (is.array(x) && is.numeric(x) && length(dim(x)) %in% c(2, 3)) {
    if (length(dim(x)) == 2) x <- array(x, c(dim(x), 1))
    ch <- dim(x)[3]
    rgb <- switch(as.character(ch),
      "1" = list(x[, , 1], x[, , 1], x[, , 1], 1),
      "2" = list(x[, , 1], x[, , 1], x[, , 1], x[, , 2]),
      "3" = list(x[, , 1], x[, , 2], x[, , 3], 1),
      "4" = list(x[, , 1], x[, , 2], x[, , 3], x[, , 4]),
      rlang::abort(paste0("Logo `", pos, "` must have 1-4 colour channels."))
    )
    a <- rgb[[4]] * alpha
    if (length(a) == 1) a <- array(a, dim(x)[1:2])
    return(matrix(grDevices::rgb(rgb[[1]], rgb[[2]], rgb[[3]], a),
                  nrow = dim(x)[1]))
  }

  if (is.matrix(x) || grDevices::is.raster(x)) {
    m <- as.matrix(x)
    cols <- grDevices::col2rgb(m, alpha = TRUE) / 255
    out <- grDevices::rgb(cols[1, ], cols[2, ], cols[3, ], cols[4, ] * alpha)
    return(matrix(out, nrow = nrow(m)))
  }

  rlang::abort(paste0(
    "Logo `", pos, "` must be a PNG file path, an image array or a raster."
  ))
}
