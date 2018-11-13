context("test-to_deg")

test_that("Errors when abs(rad) > 2pi", {
  expect_error(
    to_deg(deg = 3 * pi)
  )
})

test_that("Returns value when abs(rad) <= 2pi", {
  x <- (6.283185 * 180) / pi
  y <- to_deg(6.283185)
  expect_equal(object = y, expected = x)
})
