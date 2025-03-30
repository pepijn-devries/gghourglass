#' @rdname geom_hourglass
#' @export
GeomHourglass <-
  ggplot2::ggproto(
    "GeomHourglass",
    ggplot2::GeomPoint,
    required_aes = c("x|y"),
    extra_params = c("na.rm", "hour_center")
  )

#' Add an 'hourglass' layer to a ggplot
#' 
#' `geom_hourglass()` takes a continuous datetime object, splits in in discrete dates and
#' time of day with `stat_hourglass()`. This geometry is a wrapper
#' to add it as a layer to a ggplot. `GeomHourglass` is a ggproto object inheriting from
#' `?ggplot2::GeomPoint`. It should not be used directly. Instead call [`geom_hourglass()`].
#' 
#' @inheritParams stat_hourglass
#' @param stat Can be used to overwrite the default connection between `geom_hourglass`
#' and [`stat_hourglass()`].
#' @inheritParams ggplot2::geom_segment
#' @param ... Arguments passed to geometry.
#' @returns Returns a [ggplot2::layer()] which can be added to a [ggplot2::ggplot()]
#' @author Pepijn de Vries
#' @examples
#' library(ggplot2)
#' data(bats)
#' 
#' monitoring <- attr(bats, "monitoring")
#' 
#' ggplot(subset(bats, format(RECDATETIME, "%Y") == "2019"),
#'        aes(x = RECDATETIME, col = SPECDESCSCI)) +
#'   geom_hourglass()
#'
#' ggplot(dplyr::mutate(bats, YEAR = format(RECDATETIME, "%Y")),
#'        aes(x = RECDATETIME, col = SPECDESCSCI)) +
#'   geom_hourglass() +
#'   facet_wrap(~YEAR, scales = "free_x")
#' 
#' @export
geom_hourglass <-
  function(mapping     = NULL, data = NULL,
           stat        = "hourglass",
           position    = "identity", na.rm = FALSE, show.legend = NA,
           hour_center = 0,
           inherit.aes = TRUE, ...) {
    lyr <- ggplot2::layer(
      geom     = GeomHourglass, mapping = mapping, data = data, stat = stat,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params   = rlang::list2(na.rm = na.rm, hour_center = hour_center, ...)
    )
    c(lyr, coord_hourglass(geom = lyr))
  }

