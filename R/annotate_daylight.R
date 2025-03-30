#' @rdname annotate_daylight
#' @export
AnnotateDaylight <-
  ggplot2::ggproto(
    "AnnotateDaylight",
    ggplot2::GeomPolygon,
    draw_panel = function(self, data, panel_params, coord) {
      if (!inherits(coord, "CoordHourglass"))
        rlang::abort(c(x = "Can only add daylight annotation to CoordHourglass",
                       i = "Add this annotation to a `geom_hourglass()`"))
      if (data$latitude >= 60 || data$latitude <= -60) {
        rlang::warn("Daylight annotation may not be accurate beyond polar circles.")
      }
      orientation <- ifelse((unlist(panel_params$y$scale$name) %||% "") == "hourglass",
                            "x", "y")
      opposite    <- ifelse(orientation == "x", "y", "x")
      axis_scale <- panel_params[[orientation]]$scale
      day        <- 60*60*24
      xsc        <- axis_scale$trans$inverse(panel_params[[orientation]]$continuous_range)
      if (uses_dst(xsc)) {
        rlang::warn(
          c(
            x = paste0("You are displaying a timezone that uses daylight saving time.\n",
                       "It may contain ambiguous time stamps."),
            i = paste0("Convert the object to a time zone without daylight time (e.g. 'UTC').\n",
                       "You can use `lubridate::with_tz()`.")
          )
        )
      }
      sun        <- seq(get_date(xsc[1] - 3*day),
                        get_date(xsc[2] + 3*day), by = day)

      sun <-
        suncalc::getSunlightTimes(
          data = data.frame(date = as.Date(sun),
                            lon = data$longitude[[1]], lat = data$latitude[[1]]),
          keep = data$sun_prop[[1]]
        ) |>
        dplyr::rename_with(~c("prop1", "prop2"),
                           data$sun_prop[[1]]) |>
        dplyr::select(dplyr::any_of(c("date", "prop1", "prop2"))) |>
        dplyr::mutate(
          date = {
            date <- .data$date
            lubridate::tz(date) <- lubridate::tz(xsc)
            date
          },
          dplyr::across(
            dplyr::starts_with("prop"), \(x) {
              get_hour(lubridate::with_tz(x, lubridate::tz(xsc)))
            }
          )
        ) |>
        dplyr::mutate(
          prop2 = if(any(.data$prop2 < .data$prop1))
            dplyr::lead(.data$prop2, 1L) + lubridate::as.period(1, "day") else
              .data$prop2
        ) |>
        dplyr::filter(!is.na(.data$prop2))
      
      sun <- dplyr::bind_rows(
        sun |> dplyr::select("date", prop = "prop1"),
        sun |> dplyr::select("date", prop = "prop2") |>
          dplyr::arrange(dplyr::desc(dplyr::row_number()))
      ) |>
        dplyr::transmute(
          x = axis_scale$trans$transform(lubridate::as_datetime(.data$date)),
          y = panel_params[[orientation]]$scale$trans$transform(
            lubridate::as_datetime(.data$prop))
        ) |>
        dplyr::filter(
          duplicated(.data$x, fromLast = TRUE) |
            duplicated(.data$x, fromLast = FALSE)
        )
      sun2 <- sun
      if (max(sun$y, na.rm = TRUE) > max(panel_params[[opposite]]$continuous_range,
                                                 na.rm = TRUE)) {
        sun2$y <- sun2$y - day
        sun2$x <- sun2$x + day
      } else {
        sun2$y <- sun2$y + day
        sun2$x <- sun2$x - day
      }
      sun <-
        sun |> dplyr::mutate(pos = "A") |>
        dplyr::bind_rows(
          sun2 |> dplyr::mutate(pos = "B")
        )
      if (orientation == "y") {
        names(sun)[match(c("x", "y"), names(sun))] <- c("y", "x")
      }
      sun <-
        sun |>
        coord$transform(panel_params) |>
        dplyr::group_by(.data$pos) |>
        dplyr::group_map(~{
          grid::polygonGrob(
            x = .$x,
            y = .$y,
            gp = grid::gpar(
              fill      = data$fill[[1]],
              col       = data$colour[[1]],
              linewidth = data$linewidth[[1]],
              linetype  = data$linetype[[1]],
              alpha     = ifelse(is.na(data$alpha[[1]]), 1, data$alpha[[1]])
            )
          )
        })
      grid::gList(sun[[1]], sun[[2]])
    },
    setup_data = function(data, params) {
      data |>
        ggplot2::GeomPolygon$setup_data() |>
        dplyr::distinct() |>
        dplyr::mutate(sun_prop    = list(params$sun_prop),
                      longitude   = params$longitude,
                      latitude    = params$latitude)
    },
    default_aes  = {
      def <- ggplot2::GeomPolygon$default_aes
      def$alpha <- 0.1
      def
    },
    required_aes = NULL,
    extra_params = c("longitude", "latitude", "sun_prop")
  )

#' Annotate ggplot with a band indicating solar events
#' 
#' Annotate a ggplot (currently only plots using [coord_hourglass()]
#' is supported) with a coloured band indicating solar events, such
#' as sunset and sunrise.
#' @param longitude,latitude Geographical location that will be used
#' to calculate sunlight times.
#' @param sun_prop A vector of two solar events that should be
#' captured by the annotation. It will be shown as a coloured band
#' between these two events. Default is `c("sunrise", "sunset")`,
#' but could also be `c("dusk", "dawn")`. See [suncalc::getSunlightTimes()]
#' for all allowed solar events.
#' @param ... Passed to the list of layer parameters.
#' @returns Returns a [ggplot2::layer()] which can be added to a [ggplot2::ggplot()]
#' @author Pepijn de Vries
#' @examples
#' library(ggplot2)
#' data(bats)
#' 
#' monitoring <- attr(bats, "monitoring")
#' 
#' ggplot(subset(bats, format(RECDATETIME, "%Y") == "2018"),
#'        aes(x = RECDATETIME, col = SPECDESCSCI)) +
#'   annotate_daylight(monitoring$longitude[1], monitoring$latitude[1]) +
#'   annotate_daylight(monitoring$longitude[1], monitoring$latitude[1], c("dusk", "dawn")) +
#'   geom_hourglass()
#' @export
annotate_daylight <-
  function(longitude = 0, latitude = 60, sun_prop = c("sunrise", "sunset"), ...) {
  ggplot2::layer(
    geom     = AnnotateDaylight, mapping = NULL, data = NULL, stat = "identity",
    position = "identity", show.legend = FALSE, inherit.aes = FALSE,
    params   = rlang::list2(longitude = longitude, latitude = latitude,
                            sun_prop = sun_prop, ...)
  )  
}
