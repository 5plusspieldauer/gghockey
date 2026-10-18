test_that("fill scales use user limits and squish out-of-range values", {
  s <- scale_fill_rink(limits = c(0, 0.2))
  expect_equal(s$get_limits(), c(0, 0.2))
  expect_equal(s$oob(c(-1, 0.1, 1), c(0, 0.2)), c(0, 0.1, 0.2))
  expect_error(scale_fill_rink(palette = "Not A Palette"), "Unknown palette")
  expect_s3_class(scale_fill_rink(palette = c("white", "red")), "ScaleContinuous")
})

test_that("the diverging scale is symmetric around zero", {
  s <- scale_fill_rink_diverging()
  s$train(c(-0.1, 0.4))
  expect_equal(s$get_limits(), c(-0.4, 0.4))
  expect_equal(scale_fill_rink_diverging(limits = c(-1, 1))$get_limits(), c(-1, 1))
})

test_that("size scale range is in rink units", {
  s <- scale_size_rink(binwidth = 2, frac = c(0.5, 1))
  expect_equal(s$palette(c(0, 1)), c(1, 2))
  expect_error(scale_size_rink(frac = c(0.5, 1.5)))
})

test_that("team_palette() builds and validates palettes", {
  pal <- team_palette(c("A", "B"), c("#112233", "red"), c(NA, "white"))
  expect_s3_class(pal, "gghockey_palette")
  expect_equal(pal$secondary, c("#112233", "white"))
  expect_output(print(pal), "2 teams")

  expect_error(team_palette(c("A", "A"), c("red", "blue")), "unique")
  expect_error(team_palette("A", c("red", "blue")), "same length")
  expect_error(team_palette("A", "notacolour"), "Invalid `primary`")
  expect_error(team_palette("A", "red", "notacolour"), "Invalid `secondary`")
  expect_error(team_palette("A", NA), "Invalid `primary`")
})

test_that("team scales map colours by team name", {
  pal <- team_palette(c("Lakeside", "Northfield"), c("#0B3D91", "#C8102E"), c("#F2A900", NA))
  p <- ggplot2::ggplot(shots, ggplot2::aes(x, y, colour = team)) +
    ggplot2::geom_point() +
    scale_colour_team(pal, which = "secondary")
  ld <- ggplot2::layer_data(p)
  expect_equal(unique(ld$colour[shots$team == "Lakeside"]), "#F2A900")
  expect_equal(unique(ld$colour[shots$team == "Northfield"]), "#C8102E")

  expect_error(scale_fill_team(c(A = "red")), "team_palette")
  expect_error(scale_fill_team(pal, which = "tertiary"))
})
