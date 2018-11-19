context("test-HERE_drivetime")

test_that("Errors when 'dev' is non-boolean", {
  expect_error(
    HERE_drivetime(
      orgn_lat = 38.89875
      , orgn_lon = -77.03653
      , dest_lat = 38.88349
      , dest_lon = -77.00588
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
