#' @importFrom rlang %||% .data
NULL

.dummy <- function() {
  ## suncalc and dplyr functions are called in object created at
  ## installtime (AnnotateDaylight). It causes problems during checks, so
  ## refer to them in a dummy function to pass checks...
  suncalc::getSunlightTimes
  dplyr::mutate
}