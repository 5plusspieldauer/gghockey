# Binning contract: rink_bin() and the stat produce the SAME cells (in both
# orientations), board-edge events snap into the last in-rink cell, all-NA cells give
# NA (not NaN), and squares never exceed one cell. Uses the simulated shots data.

pts <- data.frame(
  x = c(0.1, 0.4, 1.2, 1.9, 59.99, 60, 61),
  y = c(0.1, 0.2, 0.3, 1.5, 15, 15, 15),
  v = c(1, 3, 10, NA, 2, 4, 6)
)

test_that("rink_bin() counts and averages per cell", {
  cells <- rink_bin(pts, x, y, value = v, binwidth = 1)
  expect_equal(sum(cells$n), nrow(pts))
  first <- cells[cells$x == 0.5 & cells$y == 0.5, ]
  expect_equal(first$n, 2)
  expect_equal(first$value, 2)
  # A cell whose only value is NA gets NA, not NaN.
  expect_true(is.na(cells$value[cells$x == 1.5 & cells$y == 1.5]))
  # Points on and behind the end boards snap into the last in-rink cell.
  edge <- cells[cells$y == 15.5, ]
  expect_equal(edge$x, 59.5)
  expect_equal(edge$n, 3)
  expect_equal(edge$value, 4)
})

test_that("rink_bin() can skip the bounds, count only and group", {
  unbounded <- rink_bin(pts, x, y, binwidth = 1, dimensions = NULL)
  expect_true(any(unbounded$x > 60))
  expect_true(all(is.na(unbounded$value)))

  by_team <- rink_bin(shots, x, y, value = xg, binwidth = 2, by = team)
  expect_named(by_team, c("team", "x", "y", "n", "value"))
  expect_equal(sum(by_team$n), nrow(shots))
  expect_equal(
    sum(by_team$n[by_team$team == "Lakeside"]),
    sum(shots$team == "Lakeside")
  )
  # Weighted cell means recover the overall mean.
  expect_equal(sum(by_team$value * by_team$n) / sum(by_team$n), mean(shots$xg))

  expect_error(rink_bin(shots, team, y), "must be numeric")
})

test_that("the stat matches rink_bin() in both orientations", {
  ref <- rink_bin(shots, x, y, value = xg, binwidth = 2.5)
  ref <- ref[order(ref$x, ref$y), ]
  for (o in c("horizontal", "vertical")) {
    p <- ggplot2::ggplot(shots, ggplot2::aes(x, y, z = xg)) +
      annotate_rink(orientation = o) +
      geom_rink_bin(binwidth = 2.5)
    b <- ggplot2::ggplot_build(p)
    ld <- b$data[[length(b$data)]]
    # Undo the vertical rink's reversed y scale before comparing.
    if (o == "vertical") ld$y <- -ld$y
    ld <- ld[order(ld$x, ld$y), ]
    expect_equal(ld$x, ref$x)
    expect_equal(ld$y, ref$y)
    expect_equal(ld$count, ref$n)
    expect_equal(ld$value, ref$value)
  }
})

test_that("squares never exceed one cell", {
  p <- ggplot2::ggplot(shots, ggplot2::aes(x, y, z = xg)) +
    annotate_rink() +
    geom_rink_bin(binwidth = 2, scales = FALSE) +
    ggplot2::scale_size(range = c(1, 50))
  g <- ggplot2::ggplot_build(p)
  ld <- g$data[[length(g$data)]]
  expect_gt(max(ld$size), 2)
  # draw_panel caps the side at binwidth
  expect_no_error(ggplot2::ggplot_gtable(g))
})

test_that("each style attaches its matching scales", {
  scale_aes <- function(x) unlist(lapply(x[-1], function(s) s$aesthetics))
  expect_setequal(scale_aes(geom_rink_bin(style = "size")), c("fill", "size"))
  expect_setequal(scale_aes(geom_rink_bin(style = "alpha")), c("fill", "alpha"))
  expect_setequal(scale_aes(geom_rink_bin(style = "fill")), "fill")
  expect_s3_class(geom_rink_bin(scales = FALSE), "LayerInstance")
  expect_error(geom_rink_bin(style = "hex"))
  expect_error(geom_rink_bin(binwidth = 2, gap = 2), "`gap` must be a number from 0 up to")
})

test_that("precomputed cells draw as supplied", {
  cells <- rink_bin(shots, x, y, value = xg, binwidth = 2, by = team)
  for (style in c("size", "fill", "alpha")) {
    p <- ggplot2::ggplot(cells, ggplot2::aes(x, y)) +
      annotate_rink(zones = "oz") +
      geom_rink_bin(precomputed = TRUE, style = style, binwidth = 2) +
      ggplot2::facet_wrap(~team)
    b <- ggplot2::ggplot_build(p)
    expect_equal(nrow(b$data[[length(b$data)]]), nrow(cells))
    expect_no_error(ggplot2::ggplot_gtable(b))
  }
})
