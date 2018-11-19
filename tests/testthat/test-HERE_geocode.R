context("test-HERE_geocode")

test_that("Errors when 'dev' is non-boolean", {
  expect_error(
    HERE_geocode(
      address = "1600 Pennsylvania Ave NW, Washington, DC 20500"
      , app_id = Sys.getenv("HERE_APP")
      , app_code = Sys.getenv("HERE_KEY")
      , dev = "FOO"
      , verbose = T
    )
  )
})
