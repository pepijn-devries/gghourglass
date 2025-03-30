#' Test if datetime object potentially uses daylight saving time
#' 
#' Function to check if a datetime object potentially uses daylight
#' saving time. It is not the same as [`lubridate::dst()`], which will
#' determine if daylight saving time is set for the requested date.
#' @param x A datetime object.
#' @returns Returns a `logical` value indicating of the time zone
#' used by the datetime object potentially uses daylight saving time.
#' @examples
#' uses_dst(as.POSIXct("2020-03-29 02:00:00 CET", tz = "CET"))
#' uses_dst(as.POSIXct("2020-03-29 02:00:00 UTC", tz = "UTC"))
#' @author Pepijn de Vries
#' @export
uses_dst <- function(x) {
  timezone <- lubridate::tz(x)
  sprintf("2020-%02i-01 %s", c(1L, 4L, 8L, 12L), timezone) |>
    as.POSIXct(tz = timezone) |>
    lubridate::dst() |>
    any()
}