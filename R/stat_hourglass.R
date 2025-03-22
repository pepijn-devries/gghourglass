#' @rdname stat_hourglass
#' @export
StatHourglass <-
  ggplot2::ggproto(
    "StatHourglass", ggplot2::Stat,
    setup_params = function(self, data, params) {
      params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE)
      params
    },
    compute_group = function(self, data, scales, hour_center, flipped_aes, ...) {
      orientation <- ifelse(flipped_aes, "x", "y")
      opposite     <- ifelse(flipped_aes, "y", "x")
      if (!inherits(scales[[opposite]], "ScaleContinuousDatetime")) {
        rlang::abort(c(x = sprintf("Aesthetic `%s` is not a date/time-object",
                                   orientation),
                       i = sprintf("Convert `%s` with `as.POSIXct`", orientation)))
      }
      
      if (hour_center < -12 || hour_center > 12)
        rlang::abort(c(x = "`hour_center` should be between -12 and 12.",
                       i = "Use a value between -12 and 12."))
      
      secs     <- 24*60*60
      hours    <- 60*60
      trans    <- scales::transform_time(scales[[opposite]]$timezone)
      datetime <- trans$inverse(data[[opposite]])
      data[[opposite]]     <- get_date(datetime, hour_center) |>
        trans$transform()

      data[[orientation]] <- get_hour(datetime, hour_center)
      data
    },
    required_aes = c("x|y"),
    extra_params = c("na.rm", "hour_center")
  )

#' A ggplot2 `stat` function to wrangle data for `geom_hourglass`.
#' 
#' Splits mapped `x` or `y` aesthetic from a continuous datetime into
#' discrete date values on the mapped axis. The hour of day is mapped to
#' the opposite axis.
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]. If
#' specified and `inherit.aes = TRUE` (the default), it is combined with the
#' default mapping at the top level of the plot. You must supply mapping if
#' there is no plot mapping. The `hourglass` `stat` and `geom` requires either
#' the `x` axis or the `y` axis to be mapped. The mapped aesthetic will show
#' the date of the variable, whereas the opposite axis will show the time of day.
#' @param data The data to be displayed in this layer. If `NULL`, the default,
#' the data is inherited from the plot data as specified in the call to
#' [ggplot2::ggplot()]. Otherwise, a `data.frame`, or other object, will override
#' the plot data. All objects will be fortified to produce a data frame.
#' See [ggplot2::fortify()] for which variables will be created. The data should
#' contain a column with datetime values (e.g., `?POSIXct`)
#' @param geom Can be used to overwrite the default connection between `stat_hourglass`
#' and `[geom_hourglass]`.
#' @inheritParams ggplot2::stat_identity
#' @inheritParams get_hour
#' @param na.rm If `FALSE`, the default, missing values are removed with a warning.
#' If `TRUE`, missing values are silently removed.
#' @param ... Arguments passed to [ggplot2::layer()]
#' @returns Returns a [ggplot2::layer()] which can be added to a [ggplot2::ggplot()]
#' @author Pepijn de Vries
#' @examples
#' stat_hourglass()
#' @export
stat_hourglass <- function(mapping = NULL, data = NULL, geom = "hourglass",
                           position = "identity", show.legend = NA,
                           inherit.aes = TRUE, hour_center = 0, na.rm = FALSE,
                            ...) {
  ggplot2::layer(
    stat     = StatHourglass, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params   = rlang::list2(hour_center = hour_center, na.rm = na.rm, ...)
  )
}
