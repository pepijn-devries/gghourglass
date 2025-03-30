tryCatch({
  library(ggplot2)
  library(dplyr)
  library(lubridate)
  Sys.setlocale("LC_ALL", "English")
}, warning = \(w){})
library(ggplot2) |> suppressWarnings()

test_that("Hourglass plot looks OK", {
  vdiffr::expect_doppelganger("basic hourglass", {

    data(bats)
    
    bats_sub <- subset(bats, format(RECDATETIME, "%Y") == "2018")
    
    lon <- attr(bats, "monitoring")$longitude[1]
    lat <- attr(bats, "monitoring")$latitude[1]
    
    ggplot(bats_sub, aes(x = RECDATETIME, col = SPECDESCSCI)) +
      annotate_daylight(lon, lat, c("sunset", "sunrise")) +
      annotate_daylight(lon, lat, c("dusk", "dawn")) +
      geom_hourglass() +
      labs(x = "Date", y = "Time of day", col = "Species")
  })
})

test_that("Period annotation looks OK", {
  vdiffr::expect_doppelganger("monitoring periods", {
    monitoring_periods <-
      attr(bats, "monitoring") |>
      mutate(time_on  = as_datetime(time_on),
             time_off = as_datetime(time_off))
    
    ggplot(bats, aes(x = RECDATETIME, col = SPECDESCSCI)) +
      
      ## Set background to transparent red to contrast with
      ## monitoring periods
      theme(panel.background = element_rect(fill = "#FF000044")) +
      
      ## Annotate periods in which the detector was active with
      ## white rectangles
      annotate_periodstates(
        aes(x    = start,   xend = end,
            y    = time_on, yend = time_off),
        monitoring_periods,
        fill = "white") +
      
      ## plot observations
      geom_hourglass(hour_center = -6)
  })
})