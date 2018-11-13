context("test-to_rad")

test_that("Errors when abs(deg) > 180", {
  expect_error(
    to_rad(deg = 181)
  )
})

test_that("Returns value when abs(deg) <= 180", {
  x <- (180 * pi) / 180
  y <- to_rad(180)
  expect_equal(object = y, expected = x)
})
