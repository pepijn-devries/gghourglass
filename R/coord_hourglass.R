#' @rdname coord_hourglass
#' @export
CoordHourglass <-
  ggplot2::ggproto(
    "CoordHourglass",
    ggplot2::CoordCartesian,
    setup_panel_params = function(self, scale_x, scale_y, params) {
      scale     <- list()
      scale_fun <- list()
      parent_mapping <- self$params$geom$computed_mapping
      orientation <- NULL
      if (!is.null(parent_mapping)) {
        orientation <- names(parent_mapping)
        if (any("x" %in% orientation)) orientation <- "y" else
          if (any("y" %in% orientation)) orientation <- "x" else
            orientation <- NULL
      } else {
        rlang::abort(c(x = "Could not determine 'hourglass' orientation",
                       i = "Check if one of the axis has datetime values as required"))
      }
      
      if (orientation == "y") {
        scale     <- scale_y
        scale$timezone <- scale_x$timezone
        scale_fun <- ggplot2::scale_y_datetime
      } else if (orientation == "x") {
        scale     <- scale_x
        scale$timezone <- scale_y$timezone
        scale_fun <- ggplot2::scale_x_datetime
      } else {
        rlang::abort(c(x = sprintf("Unkown orientation '%s'", orientation),
                       i = "Parameter 'orientation' should have value 'x' or 'y'."))
      }
      
      scale <- scale_fun(
        limits      = as.POSIXct(scale$get_limits()),
        name        = "hourglass",
        date_labels = self$params$date_labels,
        timezone    = "UTC"
      )
      if(orientation == "x")
        scale_x <- scale else scale_y <- scale
      
      ggplot2::ggproto_parent(ggplot2::CoordCartesian, self)$setup_panel_params(
        scale_x, scale_y, params)
    },
    extra_params = c("date_labels", "geom")
  )

#' Hourglass coordinates for a ggplot
#' 
#' A Cartesian coordinate system that adds sensible
#' guides to axes in a `geom_hourglass()` layer. It is added automatically
#' to `geom_hourglass()`. There is no need to explicitly add it to a ggplot,
#' unless you wish to tweak the coordinate system.
#' 
#' @inheritParams ggplot2::coord_cartesian
#' @param date_labels Formating string for formatting the time
#' labels on the axis. By default it is `"%H:%M"`.
#' @param layer This argument allows you to pass the `hourglass` layer
#' to which the coordinate system should be applied. It is used to
#' derive the orientation of the plot. By default it is `NULL` and
#' an attempt is made to derive the orientation of the plot from its
#' axes scales.
#' @param ... Arguments passed as extra `param`s to [ggplot2::layer()]
#' @returns Returns a `ggproto` object inheriting from `coord_cartesian()`.
#' @author Pepijn de Vries
#' @examples
#' coord_hourglass()
#' @export
coord_hourglass <-
  function(xlim = NULL, ylim = NULL, expand = TRUE, default = FALSE, 
           clip = "on", date_labels = "%H:%M", layer = NULL, ...){
    ggplot2::ggproto(NULL, CoordHourglass, limits = list(x = xlim, y = ylim), 
                     expand = expand, default = default, clip = clip,
                     params = list(date_labels = date_labels, layer = layer, ...))
  }