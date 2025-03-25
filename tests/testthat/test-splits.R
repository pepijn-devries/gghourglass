test_that("A split datetime components add up to its input value", {
  expect_true({
    x <- Sys.time()
    date <- get_date(x)
    time <- get_hour(x)
    date + time == x
  })
})

test_that("Center hour drives get_hour outcome", {
  expect_true({
    x <- as.POSIXct("2020-01-01 01:00 UTC", tz = "UTC")
    comparison <-
      get_hour(x, hour_center = +12) -
      get_hour(x, hour_center = -12)
    as.numeric(comparison) == 24*60*60
  })
})

test_that("StatHourglass doesn't throw an error", {
  expect_no_error({
    stat_hourglass()
  })
})
