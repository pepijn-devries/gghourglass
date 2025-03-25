tryCatch({
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
      annotate_daylight(lon, lat) +
      annotate_daylight(lon, lat, c("dusk", "dawn")) +
      geom_hourglass() +
      labs(x = "Date", y = "Time of day", col = "Species")
  })
})