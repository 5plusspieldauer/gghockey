has_class <- function(x, cls) any(vapply(x, inherits, logical(1), cls))

test_that("annotate_rink() returns layers, a coord and a theme", {
  r <- annotate_rink()
  expect_type(r, "list")
  expect_true(has_class(r, "LayerInstance"))
  coord <- r[[which(vapply(r, inherits, logical(1), "Coord"))]]
  expect_equal(coord$ratio, 1)
  expect_true(has_class(r, "theme"))
  expect_false(has_class(annotate_rink(theme = FALSE), "theme"))
})

test_that("vertical rinks flip and reverse, with the right aspect ratio", {
  r <- annotate_rink(orientation = "vertical", theme = FALSE)
  expect_true(has_class(r, "CoordFlip"))
  expect_true(has_class(r, "ScaleContinuousPosition"))
  th <- r[[which(vapply(r, inherits, logical(1), "theme"))]]
  expect_equal(th$aspect.ratio, 61 / 31)   # (60 + 2 * 0.5) / (30 + 2 * 0.5)
})

test_that("zones set the x limits", {
  coord_of <- function(r) r[[which(vapply(r, inherits, logical(1), "Coord"))]]
  full <- coord_of(annotate_rink())
  expect_equal(full$limits$x, c(-0.5, 60.5))
  expect_equal(full$limits$y, c(-0.5, 30.5))

  oz <- coord_of(annotate_rink(zones = "oz"))
  expect_equal(oz$limits$x, c(37.5 - 2, 60.5))

  dznz <- coord_of(annotate_rink(zones = c("nz", "dz")))
  expect_equal(dznz$limits$x, c(-0.5, 37.5 + 2))

  manual <- coord_of(annotate_rink(xlim = c(40, 50), zones = "dz"))
  expect_equal(manual$limits$x, c(40, 50))
  expect_error(annotate_rink(zones = "slot"))
})

test_that("the rink builds and draws in every orientation", {
  for (o in c("horizontal", "vertical")) {
    p <- ggplot2::ggplot(shots, ggplot2::aes(x, y)) +
      annotate_rink(orientation = o, details = TRUE, slot = TRUE) +
      ggplot2::geom_point()
    expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)))
  }
})

test_that("vertical rinks put the high-x end at the top", {
  p <- ggplot2::ggplot(data.frame(x = c(5, 55), y = c(15, 15)), ggplot2::aes(x, y)) +
    annotate_rink(orientation = "vertical") +
    ggplot2::geom_point()
  b <- ggplot2::ggplot_build(p)
  pts <- b$data[[length(b$data)]]
  # After coord_flip the rink's x is the display's vertical axis.
  rng <- b$layout$panel_params[[1]]$y.range
  expect_lt(pts$x[1], pts$x[2])
  expect_true(all(pts$x >= rng[1] & pts$x <= rng[2]))
})

test_that("logos are placed from files or arrays, and validated", {
  skip_if_not_installed("png")
  img <- array(0.5, c(10, 20, 4))
  f <- tempfile(fileext = ".png")
  png::writePNG(img, f)

  n_base <- length(annotate_rink(theme = FALSE))
  r <- annotate_rink(logos = list(centre = f, left = img, right = img), theme = FALSE)
  expect_length(r, n_base + 3)
  expect_true(has_class(r, "LayerInstance"))
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(ggplot2::ggplot() + r)))
  expect_no_error(ggplot2::ggplot_gtable(ggplot2::ggplot_build(
    ggplot2::ggplot() + annotate_rink(orientation = "vertical", logos = list(center = f))
  )))

  expect_error(annotate_rink(logos = list(centre = "nope.png")), "not found")
  expect_error(annotate_rink(logos = list(middle = img)), "Unknown logo position")
  expect_error(annotate_rink(logos = list(img)), "named list")
  expect_error(annotate_rink(logos = list(centre = 42)), "must be a PNG file path")
})

test_that("logo alpha is applied to the image", {
  img <- array(1, c(2, 2, 4))
  m <- gghockey:::logo_raster(img, alpha = 0.25, pos = "centre")
  expect_equal(unique(as.vector(m)), grDevices::rgb(1, 1, 1, 0.25))
  rgb_only <- gghockey:::logo_raster(array(0, c(2, 2, 3)), alpha = 0.5, pos = "centre")
  expect_equal(unique(as.vector(rgb_only)), grDevices::rgb(0, 0, 0, 0.5))
})
