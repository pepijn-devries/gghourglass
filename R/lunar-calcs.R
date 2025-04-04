#' Get the shape of the illuminated part of the moon
#' 
#' Function that calculates coordinates of a polygon representing the shape of the
#' illuminated fraction of the moon, as observed from Earth. The shape has a radius
#' of 1 and is centred around (0, 0). It does not consider lunar eclipses.
#' @param date A datetime object used to calculate the illuminated fraction of the moon
#' @param longitude,latitude Used to calculate zenith angle. This will result in a more accurate
#' shape of the moon as observed at the specified location.
#' @param n Number of coordinates in the returned polygon shape (should be even).
#' @returns Returns a `data.frame` with  coordinates of a polygon representing
#' the shape of the illuminated fraction of the moon.
#' @examples
#' disc_illum <- lunar_phase_polygon(as.POSIXct("2025-04-01"))
#' plot(NA, NA, xlim = c(-1,1), ylim = c(-1, 1), asp = 1)
#' polygon(disc_illum$x, disc_illum$y, col = "white")
#' 
#' disc_illum <- lunar_phase_polygon(as.POSIXct("2025-04-01"), 5, 50)
#' plot(NA, NA, xlim = c(-1,1), ylim = c(-1, 1), asp = 1)
#' polygon(disc_illum$x, disc_illum$y, col = "white")
#' @author Pepijn de Vries
#' @export
lunar_phase_polygon <- function(date, longitude, latitude, n = 100) {
  moon_illum <- suncalc::getMoonIllumination(date = date)
  if (missing(longitude) || missing(latitude) || is.null(longitude) || is.null(latitude)) {
    bright_limb_angle <- pi
  } else {

    moon_pos   <- suncalc::getMoonPosition(date = date, lon = longitude, lat = latitude)
    bright_limb_angle <- moon_illum$angle - moon_pos$parallacticAngle - pi/2 +
      ifelse((moon_illum$phase %% 1) >= 0.5, pi, 0)
  }

  disc_illum <- data.frame(
    period = seq(0, pi, length.out = floor(n/2))) |>
    dplyr::mutate(
      id = dplyr::row_number(),
      x_1 = sin(.data$period)*(-1 + (4*moon_illum$phase) %% 2),
      x_2 = sin(rev(.data$period + ifelse((moon_illum$phase %% 1) >= 0.5, 0, pi))),
      y_1 = cos(.data$period),
      y_2 = cos(rev(.data$period))) |>
    tidyr::pivot_longer(cols = !c("id", "period"), names_sep = "_",
                        names_to = c("coord", "label"), values_to = "val") |>
    tidyr::pivot_wider(id_cols = c("id", "period", "label"),
                       names_from = "coord", values_from = "val") |>
    dplyr::arrange(.data$label, .data$id) |>
    dplyr::mutate(
      angle = atan2(.data$y, .data$x) + bright_limb_angle,
      radius = sqrt(.data$x^2 + .data$y^2),
      x = .data$radius*cos(.data$angle),
      y = .data$radius*sin(.data$angle)
    ) |>
    dplyr::select(dplyr::any_of(c("x", "y")))
}

#' @rdname annotate_lunarphase
#' @export
AnnotateLunarphase <-
  ggplot2::ggproto(
    "AnnotateLunarphase",
    ggplot2::GeomPolygon,
    setup_data = function(data, params) {

      data |>
        dplyr::distinct() |>
        dplyr::mutate(
          date      = params$date,
          lon       = params$longitude,
          lat       = params$latitude,
          breaks    = list(params$breaks),
          placement = list(params$placement),
          radius    = params$radius,
          n         = params$n)
    },
    draw_panel = function(self, data, panel_params, coord) {
      if (inherits(coord, "CoordHourglass")) {
        orientation <- ifelse((unlist(panel_params$y$scale$name) %||% "") == "hourglass",
                              "x", "y")
      } else if (inherits(coord, "CoordCartesian") &&
                 inherits(panel_params$x$scale, "ScaleContinuousDatetime")) {
        orientation <- "x"
        
      } else if (inherits(coord, "CoordCartesian") &&
                 inherits(panel_params$y$scale, "ScaleContinuousDatetime")) {
        orientation <- "y"
      } else if (!is.null(data$date[[1]])) {
        orientation <- "x"
      } else {
        rlang::abort(c(x = "Coordinate system is not supported by AnnotateLunarphase",
                       i = "Try adding a different coordinate system to your plot, or setting the date"))
      }
      
      moon_grobs <- function(x, y, date) {
        lunar_data <- data.frame(
          x     = x,
          x_inv = date,
          y     = y
        ) |>
          coord$transform(panel_params) |>
          dplyr::rename(x_center = "x", y_center = "y") |>
          dplyr::rowwise() |>
          dplyr::mutate(
            lunar_stamps = {
              pol <- lunar_phase_polygon(
                .data$x_inv, data$lon[[1]], data$lat[[1]], data$n[[1]]) |>
                dplyr::summarise(
                  x = list(grid::unit(.env$x_center, "native") + .data$x * data$radius[[1]]),
                  y = list(grid::unit(.env$y_center, "native") + .data$y * data$radius[[1]])
                )
              circ <-
                dplyr::tibble(
                  per = seq(0, 2*pi, length.out = data$n[[1]]),
                  x = list(grid::unit(.env$x_center, "native") +
                             sin(per)*data$radius[[1]]),
                  y = list(grid::unit(.env$y_center, "native") +
                             cos(per)*data$radius[[1]])
                )
              
              gp0 <- do.call(
                grid::gpar,
                data[1,] |>
                  dplyr::select(c(col = "colour", fill = "fill_dark", "linewidth", "linetype", "alpha")) |>
                  dplyr::mutate(alpha = ifelse(is.na(alpha), 1, alpha))
              )
              
              gp1 <- do.call(
                grid::gpar,
                data[1,] |>
                  dplyr::select(c(col = "colour", "fill", "linewidth", "linetype", "alpha")) |>
                  dplyr::mutate(alpha = ifelse(is.na(alpha), 1, alpha))
              )
              
              
              list(
                grid::gList(
                  grid::polygonGrob(x = circ$x[[1]], y = circ$y[[1]],
                                    gp = gp0),
                  grid::polygonGrob(x = pol$x[[1]], y = pol$y[[1]],
                                    gp = gp1)
                )
              )
            }
          )
        
        do.call(grid::gList, lunar_data$lunar_stamps)
      }

      opposite <- ifelse(orientation == "x", "y", "x")
      
      rng        <- panel_params[[orientation]]$continuous_range
      trans      <- panel_params[[orientation]]$get_transformation()
      breaks     <- data$breaks[[1]]
      if (identical(breaks, ggplot2::waiver())) {
        breaks     <- panel_params[[orientation]]$breaks
        breaks     <- na.omit(breaks)
      } else if (is.function(breaks)) {
        breaks <- breaks (trans$inverse(rng))
      }
      
      opp_range <- panel_params[[opposite]]$continuous_range

      if (!is.null(data$date[[1]])) {
        placement <- data$placement[[1]] |> unlist()
        if (length(placement) == 1) placement <- c(placement, placement)

        moon_grobs(
          min(rng)       + placement[[1]]*diff(rng),
          min(opp_range) + placement[[2]]*diff(opp_range),
          data$date[[1]]
        )
      } else {
        
        moon_grobs( breaks, min(opp_range) + data$placement[[1]][[1]]*diff(opp_range), trans$inverse(breaks) )
        
      }

    },
    required_aes = NULL,
    extra_params = c("date", "longitude", "latitude", "breaks", "placement", "radius", "n"),
    default_aes  = ggplot2::aes(
      colour    = NA,
      fill      = "white",
      fill_dark = "black",
      linewidth = 0.5,
      linetype  = 1,
      alpha     = NA,
      subgroup  = NULL
    )
  )

#' Annotate ggplot with lunar phases
#' 
#' This function uses the `suncalc` package to calculate the lunar phase
#' and uses it to annotate your plot. If your plot has an axis with a
#' continuous datetime scale, lunar phases are plot along this axis.
#' Otherwise you have to specify the date of the lunar phase.
#' @inheritParams lunar_phase_polygon
#' @inheritParams ggplot2::scale_x_date
#' @param placement Relative placement of the lunar annotation in the plotting
#' panel. It should be between 0 and 1. Default is 0.9.
#' @param radius Size of the lunar pictogram. It is best to use an absolute
#' unit from the `grid` package. Default is a radius of 5 mm (`grid::unit(5, "mm")`)
#' @param ... Passed to the list of layer parameters.
#' @returns Returns a [ggplot2::layer()] which can be added to a [ggplot2::ggplot()]
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(lubridate)
#' data(bats)
#' 
#' monitoring <- attr(bats, "monitoring")
#' 
#' ## A lunar annotation can be added to a geom_hourglass layer
#' ggplot(mutate(bats, YEAR = year(RECDATETIME), MONTH = month(RECDATETIME)) |>
#'          filter(YEAR == 2018, MONTH == 5),
#'        aes(x = RECDATETIME, col = SPECDESCSCI)) +
#'   
#'   geom_hourglass() +
#'   
#'   annotate_lunarphase(
#'     longitude = monitoring$longitude[[1]],
#'     latitude  = monitoring$latitude[[1]],
#'     placement = 0.8) +
#'   
#'   scale_x_datetime(limits = as_datetime(c("2018-04-27", "2018-05-31")))
#' 
#' ## In fact, it can be added to any plot with a continuous datetime scale
#' 
#' ggplot(data.frame(stamp = seq(as_datetime("2025-04-01 UTC"),
#'                               as_datetime("2025-04-30 UTC"),
#'                               length.out = 20),
#'                   value = 1:20), aes(x = stamp, y = value)) +
#'   geom_point() +
#'   annotate_lunarphase()
#' 
#' ## Moreover, you can add it to an arbitrary plot without such scales,
#' ## but then you need to specify the date
#' 
#' ggplot(data.frame(stamp = 1:20,
#'                   value = 1:20), aes(x = stamp, y = value)) +
#'   geom_point() +
#'   annotate_lunarphase(date = "2020-01-01", placement = c(0.1, 0.9))
#' @author Pepijn de Vries
#' @export
annotate_lunarphase <- function(
    date = NULL, longitude = NULL, latitude = NULL, breaks = ggplot2::waiver(),
    placement = 0.9, radius = grid::unit(5, "mm"), n = 26, ...) {
  ggplot2::layer(
    geom     = AnnotateLunarphase, mapping = NULL, data = NULL, stat = "identity",
    position = "identity", show.legend = FALSE, inherit.aes = FALSE,
    params   = rlang::list2(
      date = date, longitude = longitude, latitude = latitude, breaks = breaks,
      placement = placement, radius = radius, n = n, ...)
  )  
}