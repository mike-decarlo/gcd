context("test-HERE_geocode")

test_that("Errors when 'dev' is non-boolean", {
  wh <- "1600 Pennsylvania Ave NW, Washington, DC 20500"
  expect_error(HERE_geocode(
    address = wh
    , app_id = "zX8ajlObLPCYD2kaobj6"
    , app_code = "NvhoO2EZsmjnExdmJtgf6g"
    , dev = "FOO"
    , verbose = T))
})

test_that("Returns dataframe of results", {
  wh <- "1600 Pennsylvania Ave NW, Washington, DC 20500"
  df <- HERE_geocode(
    address = wh
    , app_id = "zX8ajlObLPCYD2kaobj6"
    , app_code = "NvhoO2EZsmjnExdmJtgf6g"
    , dev = F
    , verbose = F)
  expect_is(df, "data.frame")
})
