context("test-HERE_drivetime")

test_that("Errors when 'dev' is non-boolean", {
  expect_error(
    HERE_drivetime(
      origin = c(38.8730144, -77.0096216)
      , destination = c(38.8980623, -77.0230686)
      , app_id = Sys.getenv("HERE_APP")
      , app_code = Sys.getenv("HERE_KEY")
      , time_frmt = "minutes"
      , type = "fastest"
      , trnsprt = "car"
      , trfc = "enabled"
      , coord_typ = "deg"
      , dev = "FOO"
      , verbose = T
    )
  )
})
