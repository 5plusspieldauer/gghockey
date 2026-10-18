# Rink geometry: tidy, in-bounds, drawn ice-first, mirror-symmetric end to end and
# side to side, shifted by the dimensions' origin, and the goal line meets the boards
# exactly where the corner arc says it should.

test_that("rink_geometry() returns tidy, in-bounds markings", {
  geo <- rink_geometry(IIHF)
  expect_s3_class(geo, "data.frame")
  expect_named(geo, c("shape", "group", "kind", "x", "y", "colour", "fill",
                      "linewidth", "alpha", "linejoin"))
  expect_true(all(geo$kind %in% c("polygon", "path")))
  expect_true(all(is.finite(geo$x) & is.finite(geo$y)))
  # Everything sits on the rink (the goals poke behind the goal line, not
  # past the boards).
  expect_true(all(geo$x >= 0 - 1e-9 & geo$x <= IIHF$length + 1e-9))
  expect_true(all(geo$y >= 0 - 1e-9 & geo$y <= IIHF$width + 1e-9))
  # Groups are contiguous and in drawing order, the ice first.
  expect_identical(unique(geo$group), seq_len(max(geo$group)))
  expect_identical(geo$shape[1], "ice")
})

test_that("the rink is symmetric end to end and side to side", {
  for (d in list(IIHF, NHL)) {
    geo <- rink_geometry(d, details = TRUE)
    # Circles are sampled at 50 angles, which don't mirror onto each other
    # exactly, so check those via their centres (the mean of their distinct
    # vertices; a closed circle repeats its first one).
    round_shape <- grepl("circle|dot", geo$shape)
    distinct <- round_shape & !duplicated(data.frame(geo$group, round(geo$x, 9), round(geo$y, 9)))
    centres <- stats::aggregate(cbind(x, y) ~ group, geo[distinct, ], mean)
    expect_equal(sort(centres$x), sort(d$length - centres$x), tolerance = 1e-6)
    expect_equal(sort(centres$y), sort(d$width - centres$y), tolerance = 1e-6)
    geo <- geo[!round_shape, ]
    key <- function(x, y) sort(paste(round(x, 6), round(y, 6)))
    expect_identical(key(geo$x, geo$y), key(d$length - geo$x, geo$y))
    expect_identical(key(geo$x, geo$y), key(geo$x, d$width - geo$y))
  }
})

test_that("geometry follows the origin of the dimensions", {
  centred <- rink_dimensions(IIHF, origin_x = -30, origin_y = -15)
  a <- rink_geometry(IIHF)
  b <- rink_geometry(centred)
  expect_equal(b$x, a$x - 30)
  expect_equal(b$y, a$y - 15)
})

test_that("details and slot add shapes", {
  base <- rink_geometry(IIHF)
  det <- rink_geometry(IIHF, details = TRUE)
  slot <- rink_geometry(IIHF, slot = TRUE)
  expect_gt(max(det$group), max(base$group))
  # 4 circles x (4 inner + 4 outer) hash marks
  expect_equal(length(unique(det$group[grepl("^hash_", det$shape)])), 32)
  expect_identical(tail(slot$shape, 1), "slot")
  # The slot is in the right-hand (offensive) zone.
  expect_true(all(slot$x[slot$shape == "slot"] > IIHF$length - IIHF$blue_line))
})

test_that("the goal lines meet the corner boards", {
  # IIHF: corner radius 8 m, goal line 4 m from the end, so the line starts
  # where the arc is at x = 4: y = 7 * (1 - cos(pi / 6)).
  geo <- rink_geometry(IIHF)
  gl <- geo[geo$shape == "goal_line_left", ]
  expect_equal(min(gl$y), 7 * (1 - cos(pi / 6)))
})
