context("test-HERE_geocode")

test_that("Errors when 'dev' is non-boolean", {
  expect_error(HERE_geocode(
    address = "1600 Pennsylvania Ave NW, Washington, DC 20500"
    , app_id = "zX8ajlObLPCYD2kaobj6"
    , app_code = "NvhoO2EZsmjnExdmJtgf6g"
    , dev = "FOO"
    , verbose = T))
})

test_that("Returns dataframe of results", {
  df <- HERE_geocode(
    address = "1600 Pennsylvania Ave NW, Washington, DC 20500"
    , app_id = "zX8ajlObLPCYD2kaobj6"
    , app_code = "NvhoO2EZsmjnExdmJtgf6g"
    , dev = F
    , verbose = T
  )
  expect_is(df, "data.frame")
})
