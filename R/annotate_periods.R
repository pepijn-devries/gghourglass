GeomPeriodstates <-
  ggplot2::ggproto(
    "GeomPeriodstates",
    ggplot2::GeomRect,
    setup_data = function(data, params) {
      data |>
        dplyr::rename(xmin = "x", xmax = "xend", ymin = "y", ymax = "yend") |>
        dplyr::rename_with(
          ~ {
            paste0("untrained_", .)
          },
          dplyr::any_of(c("xmin", "ymin", "xmax", "ymax"))
        )
    },
    draw_panel = function (self, data, panel_params, coord, lineend = "butt", 
                           linejoin = "mitre") {
      orientation <- data$orientation[1]
      data <-
        data |>
        dplyr::rename_with(
          ~{ gsub("^untrained_", "", .) },
          dplyr::starts_with("untrained_")
        )
      if (length(orientation) > 0) {
        data <-
          dplyr::bind_rows(
            data,
            data |>
              dplyr::mutate(
                dplyr::across(
                  dplyr::starts_with(orientation), ~ {
                    . -24*60*60
                  })
              ),
            data |>
              dplyr::mutate(
                dplyr::across(
                  dplyr::starts_with(orientation), ~ {
                    . +24*60*60
                  })
              )
          )
        
      }
      ggplot2::GeomRect$draw_panel(data, panel_params, coord,
                                   lineend = lineend, linejoin = linejoin)
    },
    required_aes = c("x", "y", "xend", "yend")
  )

#' Annotate a period in an hourglass plot
#' 
#' Adds rectangles to a `geom_hourglass()` plot layer. It can be used to
#' mark specific periods. The example shows how this annotation can be
#' used to mark the periods when detector (used for the observations) was active.
#' Note that this may not work correctly when displaying data that uses
#' datetime objects with daylight saving time. In those cases you could
#' split the periods into parts with and without daylight saving. Or convert
#' your data to a timezone without daylight saving time (e.g. UTC).
#' @param mapping A `ggplot2::aes()` object that maps the periods. It needs
#' `x`, `y`, `xend` and `yend`, which mark the conrners of the rectangles (i.e. periods)
#' @param data A `data.frame` containing information about the periods.
#' @inheritParams geom_hourglass
#' @param ... Passed to layer parameters.
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(lubridate)
#' 
#' ## Extract monitoring periods from 'bats' data
#' monitoring_periods <-
#'   attr(bats, "monitoring") |>
#'   mutate(time_on  = as_datetime(time_on),
#'          time_off = as_datetime(time_off))
#' 
#' ggplot(bats, aes(x = RECDATETIME, col = SPECDESCSCI)) +
#' 
#'   ## Set background to transparent red to contrast with
#'   ## monitoring periods
#'   theme(panel.background = element_rect(fill = "#FF000044")) +
#'   
#'   ## Annotate periods in which the detector was active with
#'   ## white rectangles
#'   annotate_periodstates(
#'     aes(x    = start,   xend = end,
#'         y    = time_on, yend = time_off),
#'     monitoring_periods,
#'     fill = "white") +
#'     
#'  ## plot observations
#'   geom_hourglass(hour_center = -6)
#' @author Pepijn de Vries
#' @export
annotate_periodstates <- function(mapping, data, hour_center = 0, ...) {
  ggplot2::layer(
    geom     = GeomPeriodstates,
    mapping  = mapping,
    data     = data,
    stat     = "hourglass",
    position = "identity", show.legend = FALSE, inherit.aes = FALSE,
    params   = rlang::list2(hour_center = hour_center, ...)
  )
}
