#' Functions to split datetime into date and time of day
#' 
#' Split a datetime object in a date component (`get_date()`) and
#' time of day (`get_hour()`) component, centred around a specific
#' hour of the day. They are used by `stat_hourglass()`.
#' @param x A datetime object (e.g., [as.POSIXct()]) to extract day of time
#' from
#' @param hour_center The hour at which the time of day is centred. Default is 0,
#' meaning midnight. -12 centres around noon of the preceding day, +12 centres
#' around noon of the next day.
#' @param ... Ignored
#' @returns Returns a `period` ([lubridate::as.period()]) in case of `get_hour()`.
#' Returns a datetime object in case of `get_date()`
#' @author Pepijn de Vries
#' @examples
#' my_datetime <- as.POSIXct("2020-02-02 02:20:02 UTC", tz = "UTC")
#' get_hour(my_datetime)
#' get_hour(my_datetime, -12)
#' 
#' get_date(my_datetime)
#' get_date(my_datetime, -12)
#' 
#' ## This will return the original `my_date`
#' get_date(my_datetime) + get_hour(my_datetime)
#' 
#' ## This will too
#' get_date(my_datetime, -12) + get_hour(my_datetime, -12)
#' @rdname get_hour
#' @export
get_hour <- function(x, hour_center = 0, ...) {
  (x - lubridate::floor_date(
    lubridate::as_datetime(x) +
      lubridate::as.difftime(12 - hour_center, units = "hours"), "days")) |>
    lubridate::as.period("seconds")
}

#' @rdname get_hour
#' @export
get_date <- function(x, hour_center = 0, ...) {
  lubridate::floor_date(
    lubridate::as_datetime(x) +
      lubridate::as.difftime(12 - hour_center, units = "hours"), "days")
}