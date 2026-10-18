test_that("built-in rinks have the regulation sizes", {
  expect_equal(c(IIHF$length, IIHF$width), c(60, 30))
  expect_equal(c(NHL$length, NHL$width), c(200, 85) * 0.3048)
  expect_identical(setdiff(names(IIHF), names(NHL)), character(0))
})

test_that("rink_dimensions() overrides fields and validates them", {
  narrow <- rink_dimensions(IIHF, width = 26)
  expect_equal(narrow$width, 26)
  expect_equal(narrow$length, IIHF$length)
  expect_identical(rink_dimensions(NHL), NHL)

  expect_error(rink_dimensions(IIHF, wdth = 26), "Unknown rink dimension")
  expect_error(rink_dimensions(IIHF, 26), "must be named")
  expect_error(rink_dimensions(IIHF, width = "wide"), "single finite number")
  expect_error(rink_dimensions(IIHF, blue_line = 40), "not a valid rink")
  expect_error(rink_dimensions(list(length = 60)), "must be a set of rink dimensions")
})
