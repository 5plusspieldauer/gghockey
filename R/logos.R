# =============================================================================
# logos.R  --  user-supplied logos on the ice, for annotate_rink(logos = ...).
#
# gghockey ships NO logos (they belong to the teams); this only places what the user
# brings. Boxes are in rink-relative coords and scale with the rink, and the image
# keeps its own aspect ratio inside its box.
# =============================================================================

.logo_annotations <- function(logos, d, orientation, alpha) {
  if (is.null(logos) || length(logos) == 0) return(list())
  if (!is.list(logos) || is.null(names(logos)) || any(names(logos) == ""))
    stop('`logos` must be a named list, e.g. `list(centre = "logo.png")`', call. = FALSE)
  names(logos)[names(logos) == "center"] <- "centre"   # accept the US spelling too
  unknown <- setdiff(names(logos), c("centre", "left", "right"))
  if (length(unknown))
    stop(sprintf("Unknown logo position(s): %s -- use centre, left and/or right",
                 paste(unknown, collapse = ", ")), call. = FALSE)

  # Box centres and DISPLAY sizes (width x height as seen on the page), so a wide logo
  # stays wide when the rink is drawn vertically. Sizes scale off the IIHF layout
  # (centre box 10 x 6 m, side boxes 5 x 5 m, side boxes 4 m inside the blue lines).
  L <- d$length; W <- d$width
  side_x <- d$blue_line - 4 / 60 * L
  spots <- list(
    centre = list(x = L / 2,      size = c(10 / 60 * L, 6 / 30 * W)),
    left   = list(x = side_x,     size = c(5 / 60 * L, 5 / 30 * W)),
    right  = list(x = L - side_x, size = c(5 / 60 * L, 5 / 30 * W))
  )

  lapply(names(logos), function(pos) {
    img <- .logo_raster(logos[[pos]], alpha, pos)
    sp <- spots[[pos]]
    # Horizontal: size[1] runs along x. Vertical: the display is rotated, so the rink's
    # x is the display's height -- swap.
    half <- if (orientation == "vertical") rev(sp$size) / 2 else sp$size / 2
    xmin <- d$origin_x + sp$x - half[1]; xmax <- d$origin_x + sp$x + half[1]
    ymin <- d$origin_y + W / 2 - half[2]; ymax <- d$origin_y + W / 2 + half[2]
    if (orientation == "vertical") {
      # annotation_custom() positions SKIP scale transforms, so apply the vertical
      # rink's scale_y_reverse() by hand (coord_flip() itself is handled by ggplot2).
      y <- c(-ymax, -ymin); ymin <- y[1]; ymax <- y[2]
    }
    ggplot2::annotation_custom(
      grid::rasterGrob(img, interpolate = TRUE),
      xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
    )
  })
}

# Read / coerce a logo into a colour matrix with its alpha scaled by `alpha`. Accepts
# a PNG path, a numeric image array (1-4 channels, as from png::readPNG()) or a
# matrix / raster of colours.
.logo_raster <- function(x, alpha, pos) {
  if (is.character(x) && !is.matrix(x)) {
    if (length(x) != 1 || !file.exists(x))
      stop(sprintf("Logo file for `%s` not found: %s", pos, x[1]), call. = FALSE)
    if (!requireNamespace("png", quietly = TRUE))
      stop("reading PNG logos needs the png package: install.packages(\"png\")", call. = FALSE)
    x <- png::readPNG(x)
  }
  if (inherits(x, "nativeRaster"))
    stop(sprintf("Logo `%s`: read PNGs with `native = FALSE` (the default)", pos), call. = FALSE)

  if (is.array(x) && is.numeric(x) && length(dim(x)) %in% c(2, 3)) {
    if (length(dim(x)) == 2) x <- array(x, c(dim(x), 1))   # grey matrix -> 1 channel
    ch <- dim(x)[3]
    # Channels -> r, g, b, alpha (1 = grey, 2 = grey + alpha, 3 = RGB, 4 = RGBA).
    rgb <- switch(as.character(ch),
      "1" = list(x[, , 1], x[, , 1], x[, , 1], 1),
      "2" = list(x[, , 1], x[, , 1], x[, , 1], x[, , 2]),
      "3" = list(x[, , 1], x[, , 2], x[, , 3], 1),
      "4" = list(x[, , 1], x[, , 2], x[, , 3], x[, , 4]),
      stop(sprintf("Logo `%s` must have 1-4 colour channels", pos), call. = FALSE)
    )
    a <- rgb[[4]] * alpha
    if (length(a) == 1) a <- array(a, dim(x)[1:2])
    return(matrix(grDevices::rgb(rgb[[1]], rgb[[2]], rgb[[3]], a), nrow = dim(x)[1]))
  }

  if (is.matrix(x) || grDevices::is.raster(x)) {
    m <- as.matrix(x)
    cols <- grDevices::col2rgb(m, alpha = TRUE) / 255
    out <- grDevices::rgb(cols[1, ], cols[2, ], cols[3, ], cols[4, ] * alpha)
    return(matrix(out, nrow = nrow(m)))   # col2rgb works column-major, as matrix() refills
  }

  stop(sprintf("Logo `%s` must be a PNG file path, an image array or a raster", pos),
       call. = FALSE)
}
