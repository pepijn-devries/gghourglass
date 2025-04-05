library(ggplot2, warn.conflicts = FALSE)

test_that("Lunar polygon looks OK", {
  vdiffr::expect_doppelganger("basic moon phase", {
    disc_illum <- lunar_phase_polygon(as.POSIXct("2025-04-01"), 5, 50)
    plot(NA, NA, xlim = c(-1,1), ylim = c(-1, 1), asp = 1,
         xlab = "x coord", ylab = "y coord")
    polygon(disc_illum$x, disc_illum$y)
  })
})

test_that("Lunar annotation looks OK", {
  vdiffr::expect_doppelganger("annotate moon phase", {
    ggplot(data.frame(stamp = seq(as_datetime("2025-04-01 UTC"),
                                  as_datetime("2025-04-30 UTC"),
                                  length.out = 20),
                      y = 0), aes(x = stamp, y = y)) +
      geom_point(col = NA, na.rm = TRUE) +
      annotate_lunarphase()
  })
})
