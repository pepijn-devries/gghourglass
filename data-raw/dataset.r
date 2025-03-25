if (requireNamespace(c("readr", "usethis"))) {
  bats <-
    readr::read_csv("data-raw/bats.csv") |>
    dplyr::mutate(RECDATETIME = as.POSIXct(paste(RECDATETIME, "UTC"),
                                           format = "%Y-%m-%d %H:%M", tz = "UTC"),
                  SPECDESCSCI = as.factor(SPECDESCSCI))
  attributes(bats)$monitoring <-
    dplyr::tibble(
      start       = as.POSIXct(c("2017-10-31 12:00 UTC",
                                 "2018-03-11 12:00 UTC",
                                 "2019-01-01 12:00 UTC",
                                 "2020-07-13 12:00 UTC"), tz = "UTC"),
      end         = as.POSIXct(c("2017-12-31 12:00 UTC",
                                 "2018-12-21 12:00 UTC",
                                 "2019-12-31 12:00 UTC",
                                 "2020-12-31 12:00 UTC"), tz = "UTC"),
      time_on     = lubridate::period(12, "h") + lubridate::period(15, "min"),
      time_off    = lubridate::period(12, "h") + lubridate::period(00, "min"),
      longitude   = 3.7575,
      latitude    = 52.75611,
      altitude    = 23,
      orientation = 110)
  usethis::use_data(bats, overwrite = TRUE)
}
